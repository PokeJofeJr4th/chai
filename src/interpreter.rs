use crate::parser::syntax::{Expression, ImportTree, TopLevel};

use self::{
    context::{Context, CtxItem},
    ir::{IRExpr, IRStatement},
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
                context.insert("print".into(), CtxItem::Function);
            }
            (["chai"], "range") => {
                context.insert("range".into(), CtxItem::Function);
            }
            (parents, current) => {
                return Err(format!("Can't import {}.{current}", parents.join(".")));
            }
        }
    }
    Ok(())
}

pub fn interpret(syn: Vec<TopLevel>) -> Result<(), String> {
    let mut global_context = Context::new();
    for syn in &syn {
        match syn {
            TopLevel::Import(import_tree) => todo!(),
            TopLevel::Function {
                name,
                params,
                return_type,
                body: _,
            } => todo!(),
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
    for (name, params, return_type, body) in &functions {
        let mut function_context = global_context.child();
        for (ty, param) in params {
            function_context.insert(param.clone(), CtxItem::Variable);
        }

        let body = interpret_syntax(body, &mut function_context)?;
    }
    Ok(())
}

fn interpret_syntax(
    syn: &Expression,
    function_context: &mut Context,
) -> Result<Vec<IRStatement>, String> {
    match syn {
        Expression::Ident(_) => todo!(),
        Expression::Int(_) => todo!(),
        Expression::Float(_) => todo!(),
        Expression::String(_) => todo!(),
        Expression::Block { statements, ret } => {
            let mut output = Vec::new();
            let mut block_context = function_context.child();
            for statement in statements {
                output.extend(interpret_syntax(statement, &mut block_context)?);
            }
            if let Some(ret) = ret {
                output.extend(interpret_syntax(ret, &mut block_context)?);
            }
            Ok(output)
        }
        Expression::Tuple(_) => todo!(),
        Expression::UnaryOperation(_, _) => todo!(),
        Expression::BinaryOperation(_, _, _) => todo!(),
        Expression::TernaryOperation {
            condition,
            if_true,
            if_false,
        } => todo!(),
        Expression::FunctionCall { function, args } => todo!(),
        Expression::If {
            condition,
            body,
            else_body,
        } => todo!(),
        Expression::Let { ty, var, value } => todo!(),
        Expression::Loop { body, condition } => todo!(),
        Expression::For {
            ty,
            var,
            range,
            body,
        } => todo!(),
    }
}

fn interpret_ir_expression(
    syn: &Expression,
    function_context: &mut Context,
) -> Result<IRExpr, String> {
    todo!()
}
