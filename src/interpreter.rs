use std::{collections::HashMap, sync::Arc};

use jvmrs_lib::{access, FieldType};

use crate::{
    parser::syntax::{BinaryOperator, Expression, ImportTree, TopLevel},
    types::{IRFieldType, InnerFieldType},
};

use self::{
    context::{ClassInfo, Context, CtxItem, FunctionInfo},
    ir::{IRFunction, IRLocation, IRStatement, Symbol},
    types::{operate_types, TypeHint},
};

pub mod context;
pub mod ir;
pub mod types;

/// # Errors
#[allow(clippy::too_many_lines)]
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
                    CtxItem::Function(vec![Arc::new(FunctionInfo {
                        class: "<chai>".into(),
                        access: access!(public static),
                        name: "print".into(),
                        params: vec![IRFieldType::string()],
                        ret: IRFieldType::VOID,
                    })]),
                );
            }
            (["chai"], "range") => {
                context.insert(
                    "range".into(),
                    CtxItem::Function(vec![Arc::new(FunctionInfo {
                        class: "<chai>".into(),
                        access: access!(public static),
                        name: "range".into(),
                        params: vec![InnerFieldType::Int.into(), InnerFieldType::Int.into()],
                        ret: InnerFieldType::Int.into(),
                    })]),
                );
            }
            (["chai", "option"], "*") => {
                context.insert(
                    "none".into(),
                    CtxItem::Function(vec![Arc::new(FunctionInfo {
                        class: "<chai>".into(),
                        access: access!(public static),
                        name: "none".into(),
                        params: Vec::new(),
                        ret: InnerFieldType::Object {
                            base: "java/lang/Optional".into(),
                            generics: Vec::new(),
                        }
                        .into(),
                    })]),
                );
                context.insert(
                    "some".into(),
                    CtxItem::Function(vec![Arc::new(FunctionInfo {
                        class: "<chai>".into(),
                        access: access!(public static),
                        name: "some".into(),
                        params: vec![InnerFieldType::Object {
                            base: "java/lang/Object".into(),
                            generics: Vec::new(),
                        }
                        .into()],
                        ret: InnerFieldType::Object {
                            base: "java/lang/Optional".into(),
                            generics: Vec::new(),
                        }
                        .into(),
                    })]),
                );
            }
            (["java", "lang"], "Integer") => {
                let mut methods = HashMap::new();
                methods.insert(
                    "parseInt".into(),
                    vec![Arc::new(FunctionInfo {
                        name: "parseInt".into(),
                        class: "java/lang/Integer".into(),
                        access: access!(public static),
                        params: vec![IRFieldType::string()],
                        ret: InnerFieldType::Int.into(),
                    })],
                );
                context.insert(
                    "Integer".into(),
                    CtxItem::Class(ClassInfo {
                        name: "java/lang/Integer".into(),
                        superclass: "java/lang/Object".into(),
                        fields: Vec::new(),
                        methods,
                    }),
                );
            }
            (["java", "lang"], "Optional") => {
                context.insert(
                    "Optional".into(),
                    CtxItem::Class(ClassInfo {
                        name: "java/lang/Optional".into(),
                        superclass: "java/lang/Object".into(),
                        fields: Vec::new(),
                        methods: HashMap::new(),
                    }),
                );
            }
            (["java", "lang"], "String") => {
                context.insert(
                    "String".into(),
                    CtxItem::Class(ClassInfo {
                        name: "java/lang/String".into(),
                        superclass: "java/lang/Object".into(),
                        fields: Vec::new(),
                        methods: HashMap::new(),
                    }),
                );
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
            } => {
                let function_info = FunctionInfo {
                    class: "<this>".into(),
                    access: access!(private static),
                    name: name.clone(),
                    params: params.iter().map(|(ty, _)| ty.clone()).collect(),
                    ret: return_type
                        .as_ref()
                        .map_or_else(|| IRFieldType::VOID, Clone::clone),
                };
                global_context.insert(
                    name.clone(),
                    CtxItem::Function(vec![Arc::new(function_info)]),
                );
            }
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
        let mut param_types = Vec::new();
        for (ty, param) in params {
            let ty = if let IRFieldType {
                ty: InnerFieldType::Object { base, generics },
                array_depth,
            } = ty
            {
                let Some(resolved) = function_context.get(base) else {
                    return Err(format!("Unresolved identifier `{base}`"));
                };
                let CtxItem::Class(class_info) = resolved else {
                    return Err(format!("Expected a class type; got `{resolved:?}`"));
                };
                IRFieldType {
                    ty: InnerFieldType::Object {
                        base: class_info.name.clone(),
                        generics: generics.clone(),
                    },
                    array_depth: *array_depth,
                }
            } else {
                ty.clone()
            };
            param_types.push(ty.clone());
            function_context.insert(
                param.clone(),
                CtxItem::Variable(local_var_table.len(), ty.clone()),
            );
            local_var_table.push((ty, param.clone()));
        }

        let (mut body, loc) = interpret_syntax(
            body,
            &mut function_context,
            &mut local_var_table,
            return_type
                .as_ref()
                .cloned()
                .unwrap_or_else(|| IRFieldType {
                    ty: InnerFieldType::Tuple(Vec::new()),
                    array_depth: 0,
                }),
        )?;

        // TODO: make sure type hint matches

        if loc != IRLocation::Stack {
            body.push(IRStatement::Move(loc, IRLocation::Stack));
        }
        body.push(IRStatement::Return);
        ir_functions.push(IRFunction {
            name: name.clone(),
            params: param_types,
            ret: return_type.clone(),
            body,
        });
    }
    Ok(ir_functions)
}

#[allow(clippy::too_many_lines)]
fn type_hint(
    syn: &Expression,
    function_context: &mut Context,
    local_var_table: &mut Vec<(IRFieldType, Arc<str>)>,
) -> Result<TypeHint, String> {
    match syn {
        Expression::Ident(i) => {
            let Some(item) = function_context.get(i) else {
                return Err(format!("Unresolved Identifier `{i}`"));
            };
            match item {
                CtxItem::Variable(_, ty) => Ok(TypeHint::Concrete(ty.clone())),
                other => Err(format!("Expected a variable; got `{other:?}`")),
            }
        }
        Expression::Int(_) => Ok(TypeHint::Integral),
        Expression::Float(_) => Ok(TypeHint::Floating),
        Expression::String(_) => Ok(TypeHint::Concrete(IRFieldType {
            ty: InnerFieldType::Object {
                base: "java/lang/String".into(),
                generics: Vec::new(),
            },
            array_depth: 0,
        })),
        Expression::Block {
            statements: _,
            ret: Some(ret),
        } => type_hint(ret, function_context, local_var_table),
        Expression::Tuple(elems) => Ok(TypeHint::Tuple(
            elems
                .iter()
                .map(|expr| type_hint(expr, function_context, local_var_table))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        Expression::UnaryOperation(_, _) => todo!(),
        Expression::BinaryOperation(_, _, _) => todo!(),
        Expression::FunctionCall { function, args } => todo!(),
        Expression::If {
            condition: _,
            body,
            else_body: Some(else_body),
        } => {
            let body_ty = type_hint(body, function_context, local_var_table)?;
            let else_ty = type_hint(else_body, function_context, local_var_table)?;
            body_ty.intersect(&else_ty)
        }
        Expression::Block { ret: None, .. }
        | Expression::If {
            else_body: None, ..
        }
        | Expression::Let { .. }
        | Expression::Loop { .. }
        | Expression::For { .. } => Ok(TypeHint::Void),
    }
}

#[allow(clippy::too_many_lines)]
fn interpret_syntax(
    syn: &Expression,
    function_context: &mut Context,
    local_var_table: &mut Vec<(IRFieldType, Arc<str>)>,
    expected_ty: IRFieldType,
) -> Result<(Vec<IRStatement>, IRLocation), String> {
    match (syn, expected_ty) {
        (Expression::Ident(i), expected) => {
            let Some(item) = function_context.get(i) else {
                return Err(format!("Unresolved identifier `{i}`"));
            };
            match item {
                CtxItem::Variable(var, ty) => {
                    if &expected != ty {
                        Err(format!(
                            "Type Error for variable {i}: Expected `{expected:?}`; got `{ty:?}`"
                        ))
                    } else {
                        Ok((Vec::new(), IRLocation::LocalVar(*var)))
                    }
                }
                _ => Err(format!("Unresolved identifier `{i}`")),
            }
        }
        (
            Expression::Int(i),
            IRFieldType {
                ty: InnerFieldType::Int,
                array_depth: 0,
            },
        ) => match i32::try_from(*i) {
            Ok(i) => Ok((Vec::new(), IRLocation::Int(i))),
            Err(err) => Err(err.to_string()),
        },
        (
            Expression::Float(f),
            IRFieldType {
                ty: InnerFieldType::Float,
                array_depth: 0,
            },
        ) => Ok((Vec::new(), IRLocation::Float(*f as f32))),
        (
            Expression::String(s),
            IRFieldType {
                ty: InnerFieldType::Object { base, generics },
                array_depth: 0,
            },
        ) if &*base == "java/lang/String" => Ok((Vec::new(), IRLocation::String(s.clone()))),
        (Expression::Block { statements, ret }, ret_ty) => {
            let mut output = Vec::new();
            let mut block_context = function_context.child();
            for statement in statements {
                let (syn, loc) = interpret_syntax(
                    statement,
                    &mut block_context,
                    local_var_table,
                    IRFieldType {
                        ty: InnerFieldType::Tuple(Vec::new()),
                        array_depth: 0,
                    },
                )?;
                if loc == IRLocation::Stack {
                    output.push(IRStatement::Pop);
                }
                output.extend(syn);
            }
            if let Some(ret) = ret {
                let (syn, loc) =
                    interpret_syntax(ret, &mut block_context, local_var_table, ret_ty)?;
                output.extend(syn);
                Ok((output, loc))
            } else {
                Ok((output, IRLocation::Void))
            }
        }
        (
            Expression::Tuple(elements),
            IRFieldType {
                ty: InnerFieldType::Tuple(tup_types),
                array_depth: 0,
            },
        ) => {
            let mut out = Vec::new();
            for (element, ty) in elements.iter().zip(tup_types) {
                let (code, loc) = interpret_syntax(element, function_context, local_var_table, ty)?;
                out.extend(code);
                if loc != IRLocation::Stack {
                    out.push(IRStatement::Move(loc, IRLocation::Stack));
                }
            }
            out.push(IRStatement::MakeTuple(elements.len()));
            Ok((out, IRLocation::Stack))
        }
        (Expression::UnaryOperation(op, inner), ty) => {
            // TODO: get a type hint
            let (mut output, loc) = interpret_syntax(inner, function_context, local_var_table, ty)?;
            output.push(IRStatement::UnaryOperation(*op, loc));
            // TODO: Do all unary operations maintain type?
            Ok((output, IRLocation::Stack))
        }
        (Expression::BinaryOperation(lhs, op, rhs), ty) => {
            let lhs_ty = type_hint(lhs, function_context, local_var_table)?;
            let rhs_ty = type_hint(rhs, function_context, local_var_table)?;
            let output_ty = operate_types(&lhs_ty, *op, &rhs_ty)?;
            let (mut output, lhs_loc) =
                interpret_syntax(lhs, function_context, local_var_table, lhs_ty.as_concrete())?;
            let (rhs_out, rhs_loc) =
                interpret_syntax(rhs, function_context, local_var_table, rhs_ty.as_concrete())?;
            output.extend(rhs_out);
            output.push(IRStatement::BinaryOperation(lhs_loc, *op, rhs_loc));
            Ok((
                output,
                if output_ty == TypeHint::Void {
                    IRLocation::Void
                } else {
                    IRLocation::Stack
                },
            ))
        }
        (Expression::FunctionCall { function, args }, ty) => {
            let mut arg_code = Vec::new();
            let arg_types = args
                .iter()
                .map(|arg| type_hint(arg, function_context, local_var_table))
                .collect::<Result<Vec<_>, String>>()?;
            let (mut output, method_info) =
                resolve_function(function, &arg_types, function_context, local_var_table)?;

            for (arg, ty) in args.iter().zip(&method_info.params) {
                let (code, loc) =
                    interpret_syntax(arg, function_context, local_var_table, ty.clone())?;
                arg_code.extend(code);
                if loc != IRLocation::Stack {
                    arg_code.push(IRStatement::Move(loc, IRLocation::Stack));
                }
            }
            output.extend(arg_code);
            let ret = method_info.ret.clone();
            output.push(IRStatement::Invoke(method_info));
            Ok((output, IRLocation::Stack))
        }
        (
            Expression::If {
                condition,
                body,
                else_body,
            },
            ty,
        ) => {
            let symbol_then = Symbol::new("ternary.then".into());
            let symbol_end = Symbol::new("ternary.endif".into());

            let (false_body, false_loc) = match else_body {
                Some(else_body) => {
                    interpret_syntax(else_body, function_context, local_var_table, ty.clone())?
                }
                None => (Vec::new(), IRLocation::Void),
            };
            let (true_body, true_loc) =
                interpret_syntax(body, function_context, local_var_table, ty)?;
            let output_loc = if true_loc == false_loc {
                true_loc.clone()
            } else {
                IRLocation::Stack
            };

            // if condition jmp then
            let mut output = Vec::new();
            // let mut output = ir_branch(condition, &symbol_then, function_context, local_var_table)?;
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
        (Expression::Let { ty, var, value }, expected_ty) if expected_ty.is_void() => {
            let local_index = local_var_table.len();
            function_context.insert(var.clone(), CtxItem::Variable(local_index, ty.clone()));
            local_var_table.push((ty.clone(), var.clone()));
            let local_symbol = Symbol::new(var.clone());
            let (mut output, loc) =
                interpret_syntax(value, function_context, local_var_table, ty.clone())?;
            output.push(IRStatement::Move(loc, IRLocation::LocalVar(local_index)));
            // TODO: make sure the expression type is valid
            Ok((output, IRLocation::Void))
        }
        (Expression::Loop { body, condition }, expected_ty) if expected_ty.is_void() => {
            let condition_symbol = Symbol::new("loop.condition".into());
            let body_symbol = Symbol::new("loop.body".into());
            let mut output = Vec::new();

            // goto condition
            if condition.is_some() {
                output.push(IRStatement::Jump(condition_symbol.clone()));
            }
            // body:
            output.push(IRStatement::Label(body_symbol.clone()));
            let (body, body_loc) =
                interpret_syntax(body, function_context, local_var_table, IRFieldType::VOID)?;
            if body_loc != IRLocation::Void {
                return Err(format!("Loop body should return void; got `{body_loc:?}`"));
            }
            output.extend(body);
            // condition:
            output.push(IRStatement::Label(condition_symbol));
            // if condition goto body
            match condition {
                Some(condition) => {
                    let branch = Vec::new();
                    // let branch =
                    //     ir_branch(condition, &body_symbol, function_context, local_var_table)?;
                    output.extend(branch);
                }
                None => {
                    output.push(IRStatement::Jump(body_symbol));
                }
            }
            Ok((output, IRLocation::Void))
        }
        (
            Expression::For {
                ty,
                var,
                range,
                body,
            },
            expected_ty,
        ) if expected_ty.is_void() => {
            if let Expression::FunctionCall { function, args } = &**range {
                if let (Expression::Ident(range_kw), [start, end, step @ ..]) =
                    (&**function, &args[..])
                {
                    if &**range_kw == "range" {
                        let mut loop_context = function_context.child();
                        let var_idx = local_var_table.len();
                        local_var_table.push((ty.clone(), var.clone()));

                        let body_symbol = Symbol::new("for.body".into());
                        loop_context.insert(var.clone(), CtxItem::Variable(var_idx, ty.clone()));
                        let loop_var_ty = type_hint(start, &mut loop_context, local_var_table)?
                            .intersect(&type_hint(end, &mut loop_context, local_var_table)?)?
                            .as_concrete();
                        // initialize
                        let (mut output, loop_var_loc) = interpret_syntax(
                            start,
                            &mut loop_context,
                            local_var_table,
                            loop_var_ty,
                        )?;
                        if loop_var_loc != IRLocation::Stack {
                            output.push(IRStatement::Move(loop_var_loc, IRLocation::Stack));
                        }
                        // jump body
                        output.push(IRStatement::Jump(body_symbol));
                        // increment
                        // TODO
                        // body
                        let (body, body_loc) = interpret_syntax(
                            body,
                            &mut loop_context,
                            local_var_table,
                            IRFieldType::VOID,
                        )?;
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
        (expr, ty) => Err(format!("Expected `{ty:?}`; got `{expr:?}`")),
    }
}

fn resolve_function(
    function: &Expression,
    param_types: &[TypeHint],
    context: &mut Context,
    local_var_table: &mut Vec<(IRFieldType, Arc<str>)>,
) -> Result<(Vec<IRStatement>, Arc<FunctionInfo>), String> {
    match function {
        Expression::BinaryOperation(obj, BinaryOperator::Dot, func) => {
            let Expression::Ident(id) = &**func else {
                return Err(format!("Expected function name; got `{func:?}`"));
            };
            if let Expression::Ident(class) = &**obj {
                if let Some(CtxItem::Class(class)) = context.get(class) {
                    let Some(method_choices) = class.methods.get(id) else {
                        return Err(format!("No function {id} found on class {}", class.name));
                    };
                    for method in method_choices {
                        if method.params.len() != param_types.len() {
                            continue;
                        }
                        if !method
                            .params
                            .iter()
                            .zip(param_types)
                            .all(|(param, arg)| arg.is_subtype(&TypeHint::Concrete(param.clone())))
                        {
                            continue;
                        }
                        return Ok((Vec::new(), method.clone()));
                    }
                    return Err(format!(
                        "No function {id} found on class {} that matches type contraints: {:?}",
                        class.name, param_types
                    ));
                }
            }
            let obj_ty = type_hint(obj, context, local_var_table)?.as_concrete();
            let (mut output, loc) = interpret_syntax(obj, context, local_var_table, obj_ty)?;
            if loc != IRLocation::Stack {
                output.push(IRStatement::Move(loc, IRLocation::Stack));
            }
            // TODO: use the object type to help resolve the function
            todo!("Resolve the instance function");
            // Ok((output, id.clone()))
        }
        Expression::Ident(id) => {
            let Some(func_item) = context.get(id) else {
                return Err(format!("Unresolved function identifier `{id}`"));
            };
            let CtxItem::Function(func_info) = func_item else {
                return Err(format!("Expected a function; got {func_item:?}"));
            };
            for func_option in func_info {
                if func_option.params.len() != param_types.len() {
                    continue;
                }
                if !func_option
                    .params
                    .iter()
                    .zip(param_types)
                    .all(|(param, arg)| arg.is_subtype(&TypeHint::Concrete(param.clone())))
                {
                    continue;
                }
                return Ok((Vec::new(), func_option.clone()));
            }
            Err(format!(
                "No function `{id}` found for argument types {param_types:?}"
            ))
        }
        other => Err(format!("`{other:?}` is not a function")),
    }
}
