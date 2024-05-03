use std::sync::Arc;

use jvmrs_lib::method;

use crate::{
    parser::syntax::{BinaryOperator, Expression, ImportTree, TopLevel},
    types::FieldType,
};

use self::{
    context::{Context, CtxItem},
    ir::{IRFunction, IRLocation, IRStatement, Symbol},
};

pub mod context;
pub mod ir;

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
            function_context.insert(param.clone(), CtxItem::Variable(local_var_table.len()));
            local_var_table.push((ty.clone(), param.clone()));
        }

        let (body, loc) = interpret_syntax(body, &mut function_context, &mut local_var_table)?;

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
) -> Result<(Vec<IRStatement>, IRLocation), String> {
    match syn {
        Expression::Ident(i) => {
            let Some(options) = function_context.get(i) else {
                return Err(format!("Unresolved identifier `{i}`"));
            };
            for opt in options {
                if let CtxItem::Variable(var) = opt {
                    return Ok((Vec::new(), IRLocation::LocalVar(*var)));
                }
            }
            return Err(format!("Unresolved identifier `{i}`"));
        }
        Expression::Int(i) => Ok((Vec::new(), IRLocation::Int(*i))),
        Expression::Float(f) => Ok((Vec::new(), IRLocation::Float(*f))),
        Expression::String(s) => Ok((Vec::new(), IRLocation::String(s.clone()))),
        Expression::Block { statements, ret } => {
            let mut output = Vec::new();
            let mut block_context = function_context.child();
            for statement in statements {
                let (syn, loc) = interpret_syntax(statement, &mut block_context, local_var_table)?;
                if loc == IRLocation::Stack {
                    output.push(IRStatement::Pop);
                }
                output.extend(syn);
            }
            if let Some(ret) = ret {
                let (syn, loc) = interpret_syntax(ret, &mut block_context, local_var_table)?;
                output.extend(syn);
                Ok((output, loc))
            } else {
                Ok((output, IRLocation::Void))
            }
        }
        Expression::Tuple(elements) => {
            let mut out = Vec::new();
            for element in elements {
                let (code, loc) = interpret_syntax(element, function_context, local_var_table)?;
                out.extend(code);
                if loc != IRLocation::Stack {
                    out.push(IRStatement::Move(loc, IRLocation::Stack));
                }
            }
            out.push(IRStatement::MakeTuple(elements.len()));
            Ok((out, IRLocation::Stack))
        }
        Expression::UnaryOperation(op, inner) => {
            let (mut output, loc) = interpret_syntax(inner, function_context, local_var_table)?;
            output.push(IRStatement::UnaryOperation(*op, loc));
            Ok((output, IRLocation::Stack))
        }
        Expression::BinaryOperation(lhs, op, rhs) => {
            let (mut output, lhs_loc) = interpret_syntax(lhs, function_context, local_var_table)?;
            let (rhs_out, rhs_loc) = interpret_syntax(rhs, function_context, local_var_table)?;
            output.extend(rhs_out);
            output.push(IRStatement::BinaryOperation(lhs_loc, *op, rhs_loc));
            Ok((output, IRLocation::Stack))
        }
        Expression::FunctionCall { function, args } => {
            let (mut output, method_name) =
                resolve_function(function, function_context, local_var_table)?;
            for arg in args {
                let (code, loc) = interpret_syntax(arg, function_context, local_var_table)?;
                output.extend(code);
                if loc != IRLocation::Stack {
                    output.push(IRStatement::Move(loc, IRLocation::Stack));
                }
            }
            output.push(IRStatement::Invoke(method_name));
            Ok((output, IRLocation::Stack))
        }
        Expression::If {
            condition,
            body,
            else_body,
        } => {
            let symbol_then = Symbol::new("ternary.then".into());
            let symbol_end = Symbol::new("ternary.endif".into());

            let (false_body, false_loc) = match else_body {
                Some(else_body) => interpret_syntax(else_body, function_context, local_var_table)?,
                None => (Vec::new(), IRLocation::Void),
            };
            let (true_body, true_loc) = interpret_syntax(body, function_context, local_var_table)?;
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
            Ok((output, output_loc))
        }
        Expression::Let { ty, var, value } => {
            let local_index = local_var_table.len();
            function_context.insert(var.clone(), CtxItem::Variable(local_index));
            local_var_table.push((ty.clone(), var.clone()));
            let local_symbol = Symbol::new(var.clone());
            let (mut output, loc) = interpret_syntax(value, function_context, local_var_table)?;
            output.push(IRStatement::Move(loc, IRLocation::LocalVar(local_index)));
            Ok((output, IRLocation::Void))
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
            let (body, body_loc) = interpret_syntax(body, function_context, local_var_table)?;
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
            Ok((output, IRLocation::Void))
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
                        let (mut output, loop_var_loc) =
                            interpret_syntax(start, function_context, local_var_table)?;
                        if loop_var_loc != IRLocation::Stack {
                            output.push(IRStatement::Move(loop_var_loc, IRLocation::Stack));
                        }
                        // jump body
                        output.push(IRStatement::Jump(body_symbol));
                        // increment
                        // TODO
                        // body
                        let (body, body_loc) =
                            interpret_syntax(body, function_context, local_var_table)?;
                        output.extend(body);
                        if body_loc != IRLocation::Void {
                            return Err(format!(
                                "Expected for loop to return void; got `{body_loc:?}`"
                            ));
                        }
                        // if condition jump body
                        // TODO
                        return Ok((output, IRLocation::Void));
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
            let (mut output, cnd_loc) = interpret_syntax(cond, context, local_var_table)?;
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
            let (mut output, loc) = interpret_syntax(obj, context, local_var_table)?;
            if loc != IRLocation::Stack {
                output.push(IRStatement::Move(loc, IRLocation::Stack));
            }
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
