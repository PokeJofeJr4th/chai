use std::{borrow::Cow, collections::HashMap, ops::Add, sync::Arc};

use jvmrs_lib::FieldType;

use crate::{
    parser::syntax::{BinaryOperator, Expression, ImportTree, TopLevel, UnaryOperator},
    types::{IRFieldType, InnerFieldType},
};

use self::{
    context::{ClassInfo, Context, CtxItem, FunctionInfo},
    ir::{IRExpression, IRFunction, IRStatement},
    types::TypeHint,
};

pub mod context;
pub mod ir;
pub mod types;

fn import_from_context<'a>(
    path: &[&str],
    context: &'a Context,
) -> Result<Cow<'a, CtxItem>, String> {
    if path.is_empty() {
        return Err(String::from("Invalid empty import path"));
    }
    let Some(ctx_item) = context.get(path[0]) else {
        return Err(format!("Failed to resolve import `{}`", path[0]));
    };
    if path.len() == 1 {
        return Ok(Cow::Borrowed(ctx_item));
    }
    import_from_item(&path[1..], ctx_item)
}

fn import_from_item<'a>(path: &[&str], item: &'a CtxItem) -> Result<Cow<'a, CtxItem>, String> {
    match item {
        CtxItem::Class(cls) => import_from_class(path, cls),
        CtxItem::Module(module) => {
            let Some(ctx_item) = module.get(path[0]) else {
                return Err(format!("Failed to resolve import `{}`", path.join(".")));
            };
            if path.len() == 1 {
                Ok(Cow::Borrowed(ctx_item))
            } else {
                import_from_item(&path[1..], ctx_item)
            }
        }
        CtxItem::Function(_) => Err(format!("Can't resolve {} from a function", path.join("."))),
        CtxItem::Variable(_, _) => Err(format!("Can't resolve {} from a variable", path.join("."))),
        CtxItem::Field(_) => Err(format!("Can't resolve {} from a field", path.join("."))),
    }
}

fn import_from_class<'a>(path: &[&str], cls: &'a ClassInfo) -> Result<Cow<'a, CtxItem>, String> {
    if path.is_empty() {
        return Ok(Cow::Owned(CtxItem::Class(cls.clone())));
    }
    if let Some(method_info) = cls.methods.get(path[0]) {
        return Ok(Cow::Owned(CtxItem::Function(method_info.clone())));
    }

    for _field in &cls.fields {}

    for class in &cls.inner_classes {
        if class.name.split('/').last() != Some(path[0]) {
            continue;
        }
        return import_from_class(&path[1..], class);
    }

    Err(format!(
        "Failed to resolve `{}` from class `{}`",
        path.join("."),
        cls.name
    ))
}

/// # Errors
#[allow(clippy::too_many_lines)]
pub fn resolve_imports(
    parents: &[&str],
    tree: &ImportTree,
    context: &mut Context,
) -> Result<(), String> {
    if tree.children.is_empty() {
        if &*tree.current == "*" {
            // import everything
            let ctx_item = import_from_context(parents, context)?.into_owned();
            match ctx_item {
                CtxItem::Class(cls) => {
                    for mut inner in cls.inner_classes {
                        inner.name = (parents.join("/") + &*inner.name).into();
                        context.insert(inner.name.clone(), CtxItem::Class(inner));
                    }
                    for func in cls.methods {
                        context.insert(func.0, CtxItem::Function(func.1));
                    }
                }
                CtxItem::Field(_field) => {
                    todo!()
                }
                CtxItem::Function(_func) => {
                    todo!()
                }
                CtxItem::Module(_module) => {
                    todo!()
                }
                CtxItem::Variable(_v, _c) => {
                    todo!()
                }
            }
        } else {
            // import this exact item
            let ctx_item =
                import_from_context(&[parents, &[&*tree.current]].concat(), context)?.into_owned();
            context.insert(tree.current.clone(), ctx_item);
        }
    }
    for child in &tree.children {
        resolve_imports(&[parents, &[&*tree.current]].concat(), child, context)?;
    }
    Ok(())
}

/// # Errors
/// uhh...
pub fn get_global_context(syn: &[TopLevel]) -> Result<Context, String> {
    let mut global_context = Context::new();
    for syn in syn {
        apply_top_level(&[], &mut global_context, syn)?;
    }
    Ok(global_context)
}

fn apply_top_level(parents: &[&str], context: &mut Context, syn: &TopLevel) -> Result<(), String> {
    match syn {
        TopLevel::Import(import_tree) => {
            resolve_imports(parents, import_tree, context)
                .or_else(|_| resolve_imports(&[], import_tree, context))?;
        }
        TopLevel::Function {
            name,
            generics,
            access,
            params,
            return_type,
            body: _,
        } => {
            let function_info = FunctionInfo {
                class: parents.join("/").into(),
                access: *access,
                name: name.clone(),
                generics: generics.clone(),
                params: params
                    .iter()
                    .map(|(ty, _)| resolve_type(ty, context))
                    .collect::<Result<_, _>>()?,
                ret: return_type
                    .as_ref()
                    .map_or_else(|| IRFieldType::VOID, Clone::clone),
            };
            context.insert(
                name.clone(),
                CtxItem::Function(vec![Arc::new(function_info)]),
            );
        }
        TopLevel::Class {
            class_name,
            body,
            generics: _,
        } => {
            let class_name: Arc<str> = Arc::from(parents.join("/") + &**class_name);
            context.insert(
                class_name.clone(),
                CtxItem::Class(class_info(parents, &class_name, body)),
            );
        }
    }
    Ok(())
}

fn class_info(parents: &[&str], class_name: &Arc<str>, items: &[TopLevel]) -> ClassInfo {
    let mut info = ClassInfo {
        name: Arc::from(
            parents.join("/") + if parents.is_empty() { "" } else { "/" } + &**class_name,
        ),
        superclass: "java/lang/Object".into(),
        fields: Vec::new(),
        methods: HashMap::new(),
        inner_classes: Vec::new(),
    };
    for item in items {
        match item {
            TopLevel::Function {
                name,
                generics,
                access,
                params,
                return_type,
                body: _,
            } => info
                .methods
                .entry(name.clone())
                .or_default()
                .push(Arc::new(FunctionInfo {
                    class: info.name.clone(),
                    access: *access,
                    name: name.clone(),
                    generics: generics.clone(),
                    params: params.iter().map(|(t, _)| t.clone()).collect(),
                    ret: return_type.clone().unwrap_or(IRFieldType::VOID),
                })),
            TopLevel::Import(_) => {}
            TopLevel::Class {
                class_name: name,
                body,
                generics: _,
            } => {
                info.inner_classes
                    .push(class_info(&[parents, &[class_name]].concat(), name, body));
            }
        }
    }
    info
}

/// # Errors
pub fn interpret(
    syn: Vec<TopLevel>,
    global_context: &mut Context,
) -> Result<HashMap<Arc<str>, Vec<IRFunction>>, String> {
    for syn in &syn {
        apply_top_level(&[], global_context, syn)?;
    }
    println!("{global_context:#?}");
    let mut class_table = HashMap::new();
    for syn in syn {
        let TopLevel::Class {
            class_name,
            generics: _,
            body,
        } = syn
        else {
            continue;
        };
        interpret_class(&[], &class_name, body, global_context, &mut class_table)?;
    }
    Ok(class_table)
}

fn interpret_class(
    parents: &[&str],
    class_name: &Arc<str>,
    syn: Vec<TopLevel>,
    context: &Context,
    class_table: &mut HashMap<Arc<str>, Vec<IRFunction>>,
) -> Result<(), String> {
    let mut context = context.child();
    for syn in &syn {
        apply_top_level(&[parents, &[class_name]].concat(), &mut context, syn)?;
    }
    let mut ir_functions = Vec::new();
    for x in syn {
        match x {
            TopLevel::Function {
                name,
                generics,
                access,
                params,
                return_type,
                body,
            } => {
                let mut function_context = context.child();
                let mut local_var_table = Vec::new();
                let mut param_types = Vec::new();
                if !access.is_static() {
                    let this_type = IRFieldType {
                        ty: InnerFieldType::Object {
                            base: parents.join("/").add(class_name).into(),
                            generics: Vec::new(),
                        },
                        array_depth: 0,
                    };
                    param_types.push(this_type.clone());
                    function_context.insert("this".into(), CtxItem::Variable(0, this_type.clone()));
                    local_var_table.push((this_type, "this".into()));
                }
                for (ty, param) in params {
                    let ty = resolve_type(&ty, &function_context)?;
                    param_types.push(ty.clone());
                    function_context.insert(
                        param.clone(),
                        CtxItem::Variable(local_var_table.len(), ty.clone()),
                    );
                    local_var_table.push((ty, param.clone()));
                }

                let body = interpret_syntax(
                    &body,
                    &mut function_context,
                    &mut local_var_table,
                    return_type.clone().unwrap_or_else(|| IRFieldType {
                        ty: InnerFieldType::Tuple(Vec::new()),
                        array_depth: 0,
                    }),
                )?;
                ir_functions.push(IRFunction {
                    name: name.clone(),
                    params: param_types,
                    ret: return_type.as_ref().map_or(Ok(None), |rt| {
                        resolve_type(rt, &function_context).map(Option::Some)
                    })?,
                    locals: local_var_table.into_iter().map(|(ty, _)| ty).collect(),
                    body,
                });
            }
            TopLevel::Class {
                class_name: inner_class_name,
                generics: _,
                body,
            } => {
                interpret_class(
                    parents,
                    &(class_name.to_string() + "$" + &inner_class_name).into(),
                    body,
                    &context,
                    class_table,
                )?;
            }
            TopLevel::Import(_) => {}
        }
    }
    class_table.insert(
        (parents.join("/") + if parents.is_empty() { "" } else { "/" } + class_name).into(),
        ir_functions,
    );
    Ok(())
}

fn resolve_type(ty: &IRFieldType, function_context: &Context) -> Result<IRFieldType, String> {
    if let IRFieldType {
        ty: InnerFieldType::Object { base, generics },
        array_depth,
    } = ty
    {
        let Some(resolved) = function_context.get(base) else {
            return Err(format!("Unresolved identifier `{base}`; expected a type"));
        };
        let CtxItem::Class(class_info) = resolved else {
            return Err(format!("Expected a class type; got `{resolved:?}`"));
        };
        Ok(IRFieldType {
            ty: InnerFieldType::Object {
                base: class_info.name.clone(),
                generics: generics.clone(),
            },
            array_depth: *array_depth,
        })
    } else {
        Ok(ty.clone())
    }
}

#[allow(clippy::too_many_lines)]
fn type_hint(
    syn: &Expression,
    function_context: &mut Context,
    local_var_table: &mut Vec<(IRFieldType, Arc<str>)>,
) -> Result<TypeHint, String> {
    match syn {
        Expression::Ident(i, generics) => {
            let Some(item) = function_context.get(i) else {
                return Err(format!(
                    "Unresolved Identifier `{i}`; expected an expression"
                ));
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
        Expression::BinaryOperation(lhs, BinaryOperator::Index, rhs) => {
            let lhs = type_hint(lhs, function_context, local_var_table)?;
            match lhs {
                TypeHint::Concrete(IRFieldType {
                    ty,
                    array_depth: depth @ 1..,
                }) => Ok(TypeHint::Concrete(IRFieldType {
                    ty,
                    array_depth: depth - 1,
                })),
                TypeHint::Tuple(mut types) => {
                    let Expression::Int(idx) = &**rhs else {
                        return Err(format!(
                            "Expected constant index into tuple type; got `{rhs:?}`"
                        ));
                    };
                    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                    let idx = *idx as usize;
                    if idx >= types.len() {
                        return Err(format!(
                            "Index `{idx}` is past the length of tuple type `{types:?}`"
                        ));
                    }
                    Ok(types.swap_remove(idx))
                }
                other => Err(format!("Can't index into a value of type `{other:?}`")),
            }
        }
        Expression::BinaryOperation(lhs, BinaryOperator::Add, rhs)
            if type_hint(lhs, function_context, local_var_table).is_ok_and(|ty| ty.is_string())
                || type_hint(rhs, function_context, local_var_table)
                    .is_ok_and(|ty| ty.is_string()) =>
        {
            Ok(TypeHint::Concrete(IRFieldType::string()))
        }
        Expression::BinaryOperation(
            lhs,
            BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::Mul
            | BinaryOperator::Div
            | BinaryOperator::Mod,
            rhs,
        ) => {
            let lhs = type_hint(lhs, function_context, local_var_table)?;
            let rhs = type_hint(rhs, function_context, local_var_table)?;

            Ok(lhs.intersect(&rhs))
        }
        Expression::BinaryOperation(
            _,
            BinaryOperator::Set
            | BinaryOperator::AddEq
            | BinaryOperator::DivEq
            | BinaryOperator::ModEq
            | BinaryOperator::MulEq
            | BinaryOperator::SubEq
            | BinaryOperator::XorEq
            | BinaryOperator::BitOrEq
            | BinaryOperator::BitAndEq,
            _,
        )
        | Expression::Block { ret: None, .. }
        | Expression::If {
            else_body: None, ..
        }
        | Expression::Let { .. }
        | Expression::Loop { .. }
        | Expression::For { .. } => Ok(TypeHint::Void),
        Expression::BinaryOperation(lhs, op, rhs) => todo!("{lhs:?} {op:?} {rhs:?}"),
        Expression::FunctionCall { function, args } => {
            let (_, func) = resolve_function(
                function,
                &args
                    .iter()
                    .map(|arg| type_hint(arg, function_context, local_var_table))
                    .collect::<Result<Vec<_>, _>>()?,
                function_context,
                local_var_table,
            )?;
            Ok(TypeHint::Concrete(func.ret.clone()))
        }
        Expression::If {
            condition: _,
            body,
            else_body: Some(else_body),
        } => {
            let body_ty = type_hint(body, function_context, local_var_table)?;
            let else_ty = type_hint(else_body, function_context, local_var_table)?;
            Ok(body_ty.intersect(&else_ty))
        }
    }
}

#[allow(clippy::too_many_lines)]
fn interpret_syntax(
    syn: &Expression,
    function_context: &mut Context,
    local_var_table: &mut Vec<(IRFieldType, Arc<str>)>,
    expected_ty: IRFieldType,
) -> Result<IRExpression, String> {
    match (syn, expected_ty) {
        (Expression::Ident(i, generics), expected) => {
            let Some(item) = function_context.get(i) else {
                return Err(format!("Unresolved identifier `{i}`"));
            };
            match item {
                CtxItem::Variable(var, ty) => {
                    if &expected == ty {
                        Ok(IRExpression::LocalVar(ty.clone(), *var))
                    } else {
                        Err(format!(
                            "Type Error for variable {i}: Expected `{expected:?}`; got `{ty:?}`"
                        ))
                    }
                }
                _ => Err(format!("Unresolved identifier `{i}`; expected a variable")),
            }
        }
        (
            Expression::Int(i),
            IRFieldType {
                ty: InnerFieldType::Int,
                array_depth: 0,
            },
        ) => match i32::try_from(*i) {
            Ok(i) => Ok(IRExpression::Int(i)),
            Err(err) => Err(err.to_string()),
        },
        #[allow(clippy::cast_possible_truncation)]
        (
            Expression::Float(f),
            IRFieldType {
                ty: InnerFieldType::Float,
                array_depth: 0,
            },
        ) => Ok(IRExpression::Float(*f as f32)),
        (
            Expression::String(s),
            IRFieldType {
                ty: InnerFieldType::Object { base, generics },
                array_depth: 0,
            },
        ) if &*base == "java/lang/String" && generics.is_empty() => {
            Ok(IRExpression::String(s.clone()))
        }
        (Expression::Block { statements, ret }, ret_ty) => {
            let mut output = Vec::new();
            let mut block_context = function_context.child();
            for statement in statements {
                let stmt_ty = type_hint(statement, function_context, local_var_table)?;
                let statement = interpret_syntax(
                    statement,
                    &mut block_context,
                    local_var_table,
                    stmt_ty.as_concrete(),
                )?;
                output.push(statement);
            }
            if let Some(ret) = ret {
                let ret = interpret_syntax(ret, &mut block_context, local_var_table, ret_ty)?;

                Ok(IRExpression::Block(output, Some(Box::new(ret))))
            } else {
                Ok(IRExpression::Block(output, None))
            }
        }
        (
            Expression::Tuple(elements),
            IRFieldType {
                ty: ref ty @ InnerFieldType::Tuple(ref tup_types),
                array_depth: 0,
            },
        ) => {
            let mut out = Vec::new();
            let FieldType::Array(inner) = ty.to_field_type() else {
                unreachable!()
            };
            let inner = *inner;
            // println!("Building tuple with {inner:?} elements");
            for (element, ty) in elements.iter().zip(tup_types) {
                let expr =
                    interpret_syntax(element, function_context, local_var_table, ty.clone())?;
                let expr = if ty.is_primitive() && inner.is_reference() {
                    IRExpression::UnaryOperation(ty.clone(), UnaryOperator::Box, Box::new(expr))
                } else if !ty.is_primitive() && !inner.is_reference() {
                    IRExpression::UnaryOperation(ty.clone(), UnaryOperator::Unbox, Box::new(expr))
                } else {
                    expr
                };
                out.push(expr);
            }
            Ok(IRExpression::MakeTuple(inner, out))
        }
        (Expression::UnaryOperation(op, inner), ty) => {
            // TODO: get a type hint
            let val = interpret_syntax(inner, function_context, local_var_table, ty.clone())?;
            Ok(IRExpression::UnaryOperation(ty, *op, Box::new(val)))
        }
        (Expression::BinaryOperation(lhs, op, rhs), _ty) => {
            let lhs_ty = type_hint(lhs, function_context, local_var_table)?;
            let rhs_ty = type_hint(rhs, function_context, local_var_table)?;
            // let output_ty = operate_types(&lhs_ty, *op, &rhs_ty)?;
            let lhs = interpret_syntax(
                lhs,
                function_context,
                local_var_table,
                lhs_ty.clone().as_concrete(),
            )?;
            let rhs = interpret_syntax(
                rhs,
                function_context,
                local_var_table,
                rhs_ty.clone().as_concrete(),
            )?;

            if (lhs_ty.is_string() || rhs_ty.is_string()) && *op == BinaryOperator::Add {
                match (lhs, rhs) {
                    (IRExpression::String(lhs), rhs) => Ok(IRExpression::StringConcat {
                        pattern: String::from(&*lhs) + "\x01",
                        slots: vec![(rhs_ty.as_concrete(), rhs)],
                    }),
                    (lhs, IRExpression::String(rhs)) => {
                        let mut pattern = String::from(&*rhs);
                        pattern.insert(0, '\x01');
                        Ok(IRExpression::StringConcat {
                            pattern,
                            slots: vec![(lhs_ty.as_concrete(), lhs)],
                        })
                    }
                    (
                        IRExpression::StringConcat {
                            mut pattern,
                            mut slots,
                        },
                        IRExpression::StringConcat {
                            pattern: r_pattern,
                            slots: r_slots,
                        },
                    ) => {
                        pattern.push_str(&r_pattern);
                        slots.extend(r_slots);
                        Ok(IRExpression::StringConcat { pattern, slots })
                    }
                    (
                        IRExpression::StringConcat {
                            mut pattern,
                            mut slots,
                        },
                        rhs,
                    ) => {
                        pattern.push('\x01');
                        slots.push((rhs_ty.as_concrete(), rhs));
                        Ok(IRExpression::StringConcat { pattern, slots })
                    }
                    (
                        lhs,
                        IRExpression::StringConcat {
                            mut pattern,
                            mut slots,
                        },
                    ) => {
                        pattern.insert(0, '\x01');
                        slots.push((lhs_ty.as_concrete(), lhs));
                        Ok(IRExpression::StringConcat { pattern, slots })
                    }
                    _ => unreachable!(),
                }
            } else {
                Ok(IRExpression::BinaryOperation(
                    lhs_ty.as_concrete(),
                    Box::new(lhs),
                    *op,
                    Box::new(rhs),
                ))
            }
        }
        (Expression::FunctionCall { function, args }, _ty) => {
            let mut arg_expr = Vec::new();
            let arg_types = args
                .iter()
                .map(|arg| type_hint(arg, function_context, local_var_table))
                .collect::<Result<Vec<_>, String>>()?;
            let (_output, method_info) =
                resolve_function(function, &arg_types, function_context, local_var_table)?;

            for ((arg, ty), hint) in args.iter().zip(&method_info.params).zip(arg_types) {
                let arg = interpret_syntax(arg, function_context, local_var_table, ty.clone())?;
                let arg = if hint.is_primitive() && !ty.is_primitive() {
                    IRExpression::UnaryOperation(
                        hint.as_concrete(),
                        UnaryOperator::Box,
                        Box::new(arg),
                    )
                } else if !hint.is_primitive() && ty.is_primitive() {
                    IRExpression::UnaryOperation(
                        hint.as_concrete(),
                        UnaryOperator::Unbox,
                        Box::new(arg),
                    )
                } else {
                    arg
                };
                arg_expr.push(arg);
            }
            Ok(IRExpression::Invoke(method_info, arg_expr))
        }
        (
            Expression::If {
                condition,
                body,
                else_body,
            },
            ty,
        ) => {
            let condition = interpret_syntax(
                condition,
                function_context,
                local_var_table,
                InnerFieldType::Boolean.into(),
            )?;
            let else_body = match else_body {
                Some(else_body) => {
                    interpret_syntax(else_body, function_context, local_var_table, ty.clone())?
                }
                None => IRExpression::Void,
            };
            let then_body = interpret_syntax(body, function_context, local_var_table, ty)?;
            Ok(IRExpression::If(
                Box::new(condition),
                Box::new(then_body),
                Box::new(else_body),
            ))
        }
        (Expression::Let { ty, var, value }, expected_ty) if expected_ty.is_void() => {
            let local_index = local_var_table.len();
            function_context.insert(var.clone(), CtxItem::Variable(local_index, ty.clone()));
            local_var_table.push((ty.clone(), var.clone()));
            let val = interpret_syntax(value, function_context, local_var_table, ty.clone())?;
            Ok(IRExpression::SetLocal {
                ty: ty.clone(),
                index: local_index,
                value: Box::new(val),
            })
        }
        (Expression::Loop { body, condition }, expected_ty) if expected_ty.is_void() => {
            let loop_body =
                interpret_syntax(body, function_context, local_var_table, IRFieldType::VOID)?;
            let condition = match condition {
                Some(condition) => interpret_syntax(
                    condition,
                    function_context,
                    local_var_table,
                    InnerFieldType::Boolean.into(),
                )?,
                None => IRExpression::Void,
            };
            Ok(IRExpression::For {
                init: Box::new(IRExpression::Void),
                inc: Box::new(IRExpression::Void),
                body: Box::new(loop_body),
                condition: Box::new(condition),
            })
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
                if let (Expression::Ident(range_kw, generics), [start, end, _step @ ..]) =
                    (&**function, &args[..])
                {
                    if &**range_kw == "range" && generics.is_empty() {
                        let mut loop_context = function_context.child();
                        let var_idx = local_var_table.len();
                        local_var_table.push((ty.clone(), var.clone()));
                        loop_context.insert(var.clone(), CtxItem::Variable(var_idx, ty.clone()));

                        let start = interpret_syntax(
                            start,
                            &mut loop_context,
                            local_var_table,
                            ty.clone(),
                        )?;

                        let body = interpret_syntax(
                            body,
                            &mut loop_context,
                            local_var_table,
                            IRFieldType::VOID,
                        )?;

                        let end =
                            interpret_syntax(end, &mut loop_context, local_var_table, ty.clone())?;

                        return Ok(IRExpression::For {
                            init: Box::new(IRExpression::SetLocal {
                                ty: ty.clone(),
                                index: var_idx,
                                value: Box::new(start),
                            }),
                            inc: Box::new(IRExpression::BinaryOperation(
                                ty.clone(),
                                Box::new(IRExpression::LocalVar(ty.clone(), var_idx)),
                                BinaryOperator::AddEq,
                                Box::new(IRExpression::Int(1)),
                            )),
                            body: Box::new(body),
                            condition: Box::new(IRExpression::BinaryOperation(
                                ty.clone(),
                                Box::new(IRExpression::LocalVar(ty.clone(), var_idx)),
                                BinaryOperator::Lt,
                                Box::new(end),
                            )),
                        });
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
            let Expression::Ident(id, generics) = &**func else {
                return Err(format!("Expected function name; got `{func:?}`"));
            };
            if let Expression::Ident(class, generics) = &**obj {
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
            let _instance = interpret_syntax(obj, context, local_var_table, obj_ty)?;
            // TODO: use the object type to help resolve the function
            todo!("Resolve the instance function");
            // Ok((output, id.clone()))
        }
        Expression::Ident(id, generics) => {
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
                let concrete_option = func_option.apply_generics(generics);
                if !concrete_option
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
