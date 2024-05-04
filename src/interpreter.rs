use std::sync::Arc;

use jvmrs_lib::method;

use crate::{
    parser::syntax::{BinaryOperator, Expression, ImportTree, TopLevel},
    types::{FieldType, InnerFieldType},
};

use self::{
    context::{Context, CtxItem},
    ir::{IRFunction, IRLocation, IRStatement, Symbol},
    types::TypeHint,
};

pub mod context;
pub mod ir;
pub mod types;

/// # Errors
pub fn resolve_imports(
    parents: &[&str],
    tree: &ImportTree,
    context: &mut Context,
) -> Result<(), String> {
    if tree.children.is_empty() {
        // import this exact item
        match (parents, &*tree.current) {
            (["chai"], "print") => {
                context.insert(
                    "print".into(),
                    CtxItem::Function(method!(((Object("java/lang/String".into()))) -> void)),
                );
            }
            (["chai"], "range") => {
                context.insert(
                    "range".into(),
                    CtxItem::Function(method!((int, int) -> int)),
                );
            }
            (["chai", "option"], "*") => {
                context.insert("none".into(), CtxItem::Field);
                context.insert("some".into(), CtxItem::Function(method!(((Object("java/lang/Object".into()))) -> Object("java/lang/Optional".into()))));
            }
            (["java", "lang"], "Optional") => {
                context.insert("Optional".into(), CtxItem::Class);
            }
            (parents, current) => {
                return Err(format!("Can't import {}.{current}", parents.join(".")));
            }
        }
    }
    for child in &tree.children {
        resolve_imports(&[parents, &[&*tree.current]].concat(), child, context)?;
    }
    Ok(())
}

/// # Errors
pub fn interpret(syn: Vec<TopLevel>) -> Result<Vec<IRFunction>, String> {
    let mut global_context = Context::new();
    for syn in &syn {
        match syn {
            TopLevel::Import(import_tree) => {
                resolve_imports(&[], import_tree, &mut global_context)?;
            }
            TopLevel::Function {
                name,
                params,
                return_type,
                body: _,
            } => {}
        }
    }
    let functions: Vec<_> = syn
        .into_iter()
        .filter_map(|top_level| match top_level {
            TopLevel::Function {
                name,
                params,
                return_type,
                body,
            } => Some((name, params, return_type, body)),
            _ => None,
        })
        .collect();
    let mut ir_functions = Vec::new();
    for (name, params, return_type, body) in &functions {
        let mut function_context = global_context.child();
        let mut local_var_table = Vec::new();
        for (ty, param) in params {
            function_context.insert(
                param.clone(),
                CtxItem::Variable(local_var_table.len(), ty.clone()),
            );
            local_var_table.push((ty.clone(), param.clone()));
        }

        let (body, loc, ty) = interpret_syntax(body, &mut function_context, &mut local_var_table)?;

        // TODO: make sure type hint matches
        ir_functions.push(IRFunction {
            name: name.clone(),
            params: params.clone(),
            ret: return_type.clone(),
            body,
        });
    }
    Ok(ir_functions)
}

#[allow(clippy::too_many_lines)]
fn interpret_syntax(
    syn: &Expression,
    function_context: &mut Context,
    local_var_table: &mut Vec<(FieldType, Arc<str>)>,
) -> Result<(Vec<IRStatement>, IRLocation, TypeHint), String> {
    match syn {
        Expression::Ident(i) => {
            let Some(item) = function_context.get(i) else {
                return Err(format!("Unresolved identifier `{i}`"));
            };
            match item {
                CtxItem::Variable(var, ty) => Ok((
                    Vec::new(),
                    IRLocation::LocalVar(*var),
                    TypeHint::Concrete(ty.clone()),
                )),
                CtxItem::Field => todo!(),
                _ => Err(format!("Unresolved identifier `{i}`")),
            }
        }
        Expression::Int(i) => Ok((Vec::new(), IRLocation::Int(*i), TypeHint::Integral)),
        Expression::Float(f) => Ok((Vec::new(), IRLocation::Float(*f), TypeHint::Floating)),
        Expression::String(s) => Ok((
            Vec::new(),
            IRLocation::String(s.clone()),
            TypeHint::Concrete(
                InnerFieldType::Object {
                    base: "java/lang/String".into(),
                    generics: Vec::new(),
                }
                .into(),
            ),
        )),
        Expression::Block { statements, ret } => {
            let mut output = Vec::new();
            let mut block_context = function_context.child();
            for statement in statements {
                let (syn, loc, _ty) =
                    interpret_syntax(statement, &mut block_context, local_var_table)?;
                if loc == IRLocation::Stack {
                    output.push(IRStatement::Pop);
                }
                output.extend(syn);
            }
            if let Some(ret) = ret {
                let (syn, loc, ty) = interpret_syntax(ret, &mut block_context, local_var_table)?;
                output.extend(syn);
                Ok((output, loc, ty))
            } else {
                Ok((output, IRLocation::Void, TypeHint::Void))
            }
        }
        Expression::Tuple(elements) => {
            let mut out = Vec::new();
            let mut tuple_types = Vec::new();
            for element in elements {
                let (code, loc, ty) = interpret_syntax(element, function_context, local_var_table)?;
                out.extend(code);
                tuple_types.push(ty);
                if loc != IRLocation::Stack {
                    out.push(IRStatement::Move(loc, IRLocation::Stack));
                }
            }
            out.push(IRStatement::MakeTuple(elements.len()));
            Ok((out, IRLocation::Stack, TypeHint::Tuple(tuple_types)))
        }
        Expression::UnaryOperation(op, inner) => {
            let (mut output, loc, ty) = interpret_syntax(inner, function_context, local_var_table)?;
            output.push(IRStatement::UnaryOperation(*op, loc));
            // TODO: Do all unary operations maintain type?
            Ok((output, IRLocation::Stack, ty))
        }
        Expression::BinaryOperation(lhs, op, rhs) => {
            let (mut output, lhs_loc, lhs_ty) =
                interpret_syntax(lhs, function_context, local_var_table)?;
            let (rhs_out, rhs_loc, rhs_ty) =
                interpret_syntax(rhs, function_context, local_var_table)?;
            // TODO: make sure LHS and RHS are compatible with this operator
            output.extend(rhs_out);
            output.push(IRStatement::BinaryOperation(lhs_loc, *op, rhs_loc));
            Ok((output, IRLocation::Stack, lhs_ty))
        }
        Expression::FunctionCall { function, args } => {
            let mut arg_code = Vec::new();
            let mut arg_types = Vec::new();
            for arg in args {
                let (code, loc, arg_ty) = interpret_syntax(arg, function_context, local_var_table)?;
                arg_code.extend(code);
                arg_types.push(arg_ty);
                if loc != IRLocation::Stack {
                    arg_code.push(IRStatement::Move(loc, IRLocation::Stack));
                }
            }
            // TODO: use arg_types to help resolve the function and include the return type
            let (mut output, method_name) =
                resolve_function(function, function_context, local_var_table)?;
            output.extend(arg_code);
            output.push(IRStatement::Invoke(method_name));
            Ok((output, IRLocation::Stack, TypeHint::Any))
        }
        Expression::If {
            condition,
            body,
            else_body,
        } => {
            let symbol_then = Symbol::new("ternary.then".into());
            let symbol_end = Symbol::new("ternary.endif".into());

            let (false_body, false_loc, else_ty) = match else_body {
                Some(else_body) => interpret_syntax(else_body, function_context, local_var_table)?,
                None => (Vec::new(), IRLocation::Void, TypeHint::Void),
            };
            let (true_body, true_loc, true_ty) =
                interpret_syntax(body, function_context, local_var_table)?;
            let output_loc = if true_loc == false_loc {
                true_loc.clone()
            } else {
                IRLocation::Stack
            };

            // if condition jmp then
            let mut output = ir_branch(condition, &symbol_then, function_context, local_var_table)?;
            // else:
            output.extend(false_body);
            if output_loc != false_loc {
                output.push(IRStatement::Move(false_loc, output_loc.clone()));
            }
            // jmp endif
            output.push(IRStatement::Jump(symbol_end.clone()));
            // then:
            output.push(IRStatement::Label(symbol_then));
            output.extend(true_body);
            if output_loc != true_loc {
                output.push(IRStatement::Move(true_loc, output_loc.clone()));
            }
            // endif:
            output.push(IRStatement::Label(symbol_end));
            // TODO: make sure True and False types are compatible
            Ok((output, output_loc, true_ty))
        }
        Expression::Let { ty, var, value } => {
            let local_index = local_var_table.len();
            function_context.insert(var.clone(), CtxItem::Variable(local_index, ty.clone()));
            local_var_table.push((ty.clone(), var.clone()));
            let local_symbol = Symbol::new(var.clone());
            let (mut output, loc, expr_ty) =
                interpret_syntax(value, function_context, local_var_table)?;
            output.push(IRStatement::Move(loc, IRLocation::LocalVar(local_index)));
            // TODO: make sure the expression type is valid
            Ok((output, IRLocation::Void, TypeHint::Void))
        }
        Expression::Loop { body, condition } => {
            let condition_symbol = Symbol::new("loop.condition".into());
            let body_symbol = Symbol::new("loop.body".into());
            let mut output = Vec::new();

            // goto condition
            if condition.is_some() {
                output.push(IRStatement::Jump(condition_symbol.clone()));
            }
            // body:
            output.push(IRStatement::Label(body_symbol.clone()));
            let (body, body_loc, _body_ty) =
                interpret_syntax(body, function_context, local_var_table)?;
            if body_loc != IRLocation::Void {
                return Err(format!("Loop body should return void; got `{body_loc:?}`"));
            }
            output.extend(body);
            // condition:
            output.push(IRStatement::Label(condition_symbol));
            // if condition goto body
            match condition {
                Some(condition) => {
                    let branch =
                        ir_branch(condition, &body_symbol, function_context, local_var_table)?;
                    output.extend(branch);
                }
                None => {
                    output.push(IRStatement::Jump(body_symbol));
                }
            }
            Ok((output, IRLocation::Void, TypeHint::Void))
        }
        Expression::For {
            ty,
            var,
            range,
            body,
        } => {
            if let Expression::FunctionCall { function, args } = &**range {
                if let (Expression::Ident(range_kw), [start, end, step @ ..]) =
                    (&**function, &args[..])
                {
                    if &**range_kw == "range" {
                        let body_symbol = Symbol::new("for.body".into());

                        // initialize
                        let (mut output, loop_var_loc, loop_var_ty) =
                            interpret_syntax(start, function_context, local_var_table)?;
                        if loop_var_loc != IRLocation::Stack {
                            output.push(IRStatement::Move(loop_var_loc, IRLocation::Stack));
                        }
                        // jump body
                        output.push(IRStatement::Jump(body_symbol));
                        // increment
                        // TODO
                        // body
                        let (body, body_loc, _body_ty) =
                            interpret_syntax(body, function_context, local_var_table)?;
                        output.extend(body);
                        if body_loc != IRLocation::Void {
                            return Err(format!(
                                "Expected for loop to return void; got `{body_loc:?}`"
                            ));
                        }
                        // if condition jump body
                        // TODO
                        return Ok((output, IRLocation::Void, TypeHint::Void));
                    }
                }
            }
            todo!("For-each loop not implemented")
        }
    }
}

/**
 * Create a branch that evaluates the provided condition. Jumps to the provided symbol if the condition evaluates to true; otherwise ends execution at the end of the provided list of instructions
 */
fn ir_branch(
    condition: &Expression,
    jump: &Symbol,
    context: &mut Context,
    local_var_table: &mut Vec<(FieldType, Arc<str>)>,
) -> Result<Vec<IRStatement>, String> {
    match condition {
        Expression::BinaryOperation(lhs, BinaryOperator::Or, rhs) => {
            let mut output = ir_branch(lhs, jump, context, local_var_table)?;
            output.extend(ir_branch(rhs, jump, context, local_var_table)?);
            Ok(output)
        }
        Expression::BinaryOperation(lhs, BinaryOperator::And, rhs) => {
            todo!()
        }
        cond => {
            let (mut output, cnd_loc, _cnd_ty) = interpret_syntax(cond, context, local_var_table)?;
            output.push(IRStatement::Branch(cnd_loc, jump.clone()));
            Ok(output)
        }
    }
}

fn resolve_function(
    function: &Expression,
    context: &mut Context,
    local_var_table: &mut Vec<(FieldType, Arc<str>)>,
) -> Result<(Vec<IRStatement>, Arc<str>), String> {
    match function {
        Expression::BinaryOperation(obj, BinaryOperator::Dot, func) => {
            let (mut output, loc, obj_ty) = interpret_syntax(obj, context, local_var_table)?;
            if loc != IRLocation::Stack {
                output.push(IRStatement::Move(loc, IRLocation::Stack));
            }
            // TODO: use the object type to help resolve the function
            let Expression::Ident(id) = &**func else {
                return Err(format!("Expected function name; got `{func:?}`"));
            };
            Ok((output, id.clone()))
        }
        Expression::Ident(id) => {
            let func_set = context.get(id);
            Ok((Vec::new(), id.clone()))
        }
        other => Err(format!("`{other:?}` is not a function")),
    }
}
