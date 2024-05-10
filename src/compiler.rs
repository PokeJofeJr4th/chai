use std::collections::HashMap;

use jvmrs_lib::{access, Constant, FieldType, MethodDescriptor};

use crate::{
    interpreter::{
        context::FunctionInfo,
        ir::{IRExpression, IRFunction, IRStatement, Symbol},
    },
    parser::syntax::{BinaryOperator, UnaryOperator},
    types::{IRFieldType, InnerFieldType},
};

use self::{
    class::{AttributeInfo, Class, MethodInfo},
    instruction::{Const, ICmp, Operator, PrimitiveType},
};

mod class;
pub mod instruction;

type Instruction = instruction::Instruction<Symbol>;

/// # Errors
pub fn compile(syn: Vec<IRFunction>) -> Result<Class, String> {
    let mut class = Class::new("Chai".into(), "java/lang/Object".into());

    for func in syn {
        let func = compile_function(&mut class, func)?;
        class.register_method(func);
    }

    Ok(class)
}

fn compile_function(class: &mut Class, func: IRFunction) -> Result<MethodInfo, String> {
    let mut body: Vec<Instruction> = Vec::new();
    body.extend(compile_expression(func.body, class)?);
    match &func.ret {
        Some(ret) => body.push(Instruction::Return(ret.to_primitive())),
        None => body.push(Instruction::ReturnVoid),
    }
    println!("{body:?}");

    let mut code = Vec::new();

    // max stack
    code.extend(u16::MAX.to_be_bytes());
    // max locals
    code.extend(
        (func
            .locals
            .iter()
            .map(|loc| loc.to_field_type().get_size())
            .sum::<usize>() as u16)
            .to_be_bytes(),
    );

    // code_length
    // code bytes
    let mut symbol_table: HashMap<Symbol, usize> = HashMap::new();
    let mut offset: usize = 0;
    for instr in &body {
        if let Instruction::Label(label) = instr {
            symbol_table.insert(label.clone(), offset);
        }
        offset += instr.size();
    }
    println!("{offset}");
    let mut code_bytes = Vec::new();
    let mut offset = 0;
    for instr in body {
        let instr = instr.map(|sym| *symbol_table.get(&sym).unwrap() as i16 - offset);
        offset += instr.size() as i16;
        instr.write(&mut code_bytes).map_err(|e| e.to_string())?;
        println!("{offset} {} {instr:?}", code_bytes.len());
    }
    println!("{offset} {}", code_bytes.len());
    code.extend((code_bytes.len() as u32).to_be_bytes());
    code.extend(code_bytes);
    // exception table
    code.extend([0, 0]);
    // attributes
    code.extend([0, 0]);

    Ok(MethodInfo {
        name: func.name,
        access: access!(public static),
        ty: MethodDescriptor {
            parameter_size: func
                .params
                .iter()
                .map(|a| a.to_field_type().get_size())
                .sum(),
            parameters: func.params.into_iter().map(|a| a.to_field_type()).collect(),
            return_type: func.ret.as_ref().map(IRFieldType::to_field_type),
        },
        attributes: vec![AttributeInfo {
            name: "Code".into(),
            info: code,
        }],
    })
}

#[allow(clippy::too_many_lines)]
fn compile_expression(
    expression: IRExpression,
    class: &mut Class,
) -> Result<Vec<Instruction>, String> {
    match expression {
        IRExpression::Void => Ok(Vec::new()),
        IRExpression::LocalVar(ty, local_idx) => {
            Ok(vec![Instruction::Load(ty.to_primitive(), local_idx as u8)])
        }
        IRExpression::String(s) => Ok(vec![Instruction::LoadConst(
            class.register_constant(Constant::String(s)) as u16,
        )]),
        IRExpression::Int(i) => Ok(vec![match i {
            -1 => Instruction::Push(Const::IM1),
            0 => Instruction::Push(Const::I0),
            1 => Instruction::Push(Const::I1),
            2 => Instruction::Push(Const::I2),
            3 => Instruction::Push(Const::I3),
            4 => Instruction::Push(Const::I4),
            5 => Instruction::Push(Const::I5),
            i => i16::try_from(i).map_or_else(
                |_| Instruction::LoadConst(class.register_constant(Constant::Int(i)) as u16),
                |i| Instruction::PushByte(i as u16),
            ),
        }]),
        IRExpression::Long(_) => todo!(),
        IRExpression::Float(f) => Ok(vec![if f == 0.0 {
            Instruction::Push(Const::F0)
        } else if f == 1.0 {
            Instruction::Push(Const::F1)
        } else if f == 2.0 {
            Instruction::Push(Const::F2)
        } else {
            Instruction::LoadConst(class.register_constant(Constant::Float(f)) as u16)
        }]),
        IRExpression::Double(_) => todo!(),
        IRExpression::MakeTuple(values) => {
            let mut code = compile_expression(IRExpression::Int(values.len() as i32), class)?;
            let obj_ty =
                class.register_constant(Constant::ClassRef("java/lang/Object".into())) as u16;
            code.push(Instruction::ReferenceArray(obj_ty));
            for (idx, value) in values.into_iter().enumerate() {
                code.push(Instruction::Dup);
                code.extend(compile_expression(IRExpression::Int(idx as i32), class)?);
                code.extend(compile_expression(value, class)?);
                code.push(Instruction::ArrayStore(PrimitiveType::Reference));
            }
            Ok(code)
        }
        IRExpression::BinaryOperation(
            IRFieldType {
                ty:
                    ty @ (InnerFieldType::Byte
                    | InnerFieldType::Double
                    | InnerFieldType::Float
                    | InnerFieldType::Int
                    | InnerFieldType::Long
                    | InnerFieldType::Short),
                array_depth: 0,
            },
            lhs,
            op @ (BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::Mul
            | BinaryOperator::Div
            | BinaryOperator::Mod),
            rhs,
        ) => {
            let mut code = compile_expression(*lhs, class)?;
            code.extend(compile_expression(*rhs, class)?);
            code.push(Instruction::Operate(
                IRFieldType::from(ty).to_primitive(),
                match op {
                    BinaryOperator::Add => Operator::Add,
                    BinaryOperator::Sub => Operator::Sub,
                    BinaryOperator::Mul => Operator::Mul,
                    BinaryOperator::Div => Operator::Div,
                    BinaryOperator::Mod => Operator::Rem,
                    _ => unreachable!(),
                },
            ));
            Ok(code)
        }
        IRExpression::BinaryOperation(
            IRFieldType {
                ty:
                    ty @ (InnerFieldType::Byte
                    | InnerFieldType::Double
                    | InnerFieldType::Float
                    | InnerFieldType::Int
                    | InnerFieldType::Long
                    | InnerFieldType::Short),
                array_depth: 0,
            },
            lhs,
            op @ (BinaryOperator::AddEq
            | BinaryOperator::SubEq
            | BinaryOperator::MulEq
            | BinaryOperator::DivEq
            | BinaryOperator::ModEq),
            rhs,
        ) => {
            let mut code = compile_expression(*rhs, class)?;
            code.push(Instruction::Operate(
                IRFieldType::from(ty).to_primitive(),
                match op {
                    BinaryOperator::AddEq => Operator::Add,
                    BinaryOperator::SubEq => Operator::Sub,
                    BinaryOperator::MulEq => Operator::Mul,
                    BinaryOperator::DivEq => Operator::Div,
                    BinaryOperator::ModEq => Operator::Rem,
                    _ => unreachable!(),
                },
            ));
            compile_modify(*lhs, code, class)
        }
        IRExpression::BinaryOperation(
            ty @ IRFieldType {
                array_depth: 1.., ..
            },
            lhs,
            BinaryOperator::Index,
            rhs,
        ) => {
            let mut code = compile_expression(*lhs, class)?;
            code.extend(compile_expression(*rhs, class)?);
            code.push(Instruction::ArrayLoad(ty.to_primitive()));
            Ok(code)
        }
        IRExpression::BinaryOperation(ty, lhs, op, rhs) => todo!("{ty:?} {lhs:?} {op:?} {rhs:?}"),
        IRExpression::UnaryOperation(ty, op, inner) => todo!("{op:?} {inner:?}"),
        IRExpression::Block(stmts, ret) => {
            let mut code = Vec::new();
            for stmt in stmts {
                code.extend(compile_expression(stmt, class)?);
                // TODO: Pop?
            }
            if let Some(ret) = ret {
                code.extend(compile_expression(*ret, class)?);
            }
            Ok(code)
        }
        IRExpression::If(cond, then_body, else_body) => {
            let end_label = Symbol::new("if.end".into());
            let else_label = Symbol::new("if.else".into());

            let mut out = compile_branch(cond.negative(), else_label.clone(), class)?;
            out.extend(compile_expression(*then_body, class)?);
            out.push(Instruction::Goto(end_label.clone()));
            out.push(Instruction::Label(else_label));
            out.extend(compile_expression(*else_body, class)?);
            out.push(Instruction::Label(end_label));

            Ok(out)
        }
        IRExpression::For {
            init,
            inc,
            body,
            condition,
        } => {
            let test_symbol = Symbol::new("loop.test".into());
            let body_symbol = Symbol::new("loop.body".into());

            let mut code = compile_expression(*init, class)?;
            // goto test
            code.push(Instruction::Goto(test_symbol.clone()));
            // body
            code.push(Instruction::Label(body_symbol.clone()));
            code.extend(compile_expression(*body, class)?);
            // inc
            code.extend(compile_expression(*inc, class)?);
            // test
            code.push(Instruction::Label(test_symbol));
            code.extend(compile_branch(*condition, body_symbol, class)?);
            // end
            Ok(code)
        }
        IRExpression::SetLocal(ty, idx, expr) => {
            let mut code = compile_expression(*expr, class)?;
            code.push(Instruction::Store(ty.to_primitive(), idx as u8));
            Ok(code)
        }
        IRExpression::Invoke(func, args) => {
            let func_args: Vec<_> = func
                .params
                .iter()
                .map(|param| param.to_field_type())
                .collect();
            let method_type = MethodDescriptor {
                parameter_size: func_args.iter().map(FieldType::get_size).sum(),
                parameters: func_args,
                return_type: if func.ret.is_void() {
                    None
                } else {
                    Some(func.ret.to_field_type())
                },
            };
            let method_ref = Constant::MethodRef {
                class: func.class.clone(),
                name: func.name.clone(),
                method_type,
            };
            let method_ref = class.register_constant(method_ref) as u16;

            let mut code = Vec::new();
            for arg in args {
                code.extend(compile_expression(arg, class)?);
            }
            if func.access.is_static() {
                code.push(Instruction::InvokeStatic(method_ref));
            } else {
                code.push(Instruction::InvokeVirtual(method_ref));
            }
            Ok(code)
        }
    }
}

fn compile_branch(
    condition: IRExpression,
    branch_label: Symbol,
    class: &mut Class,
) -> Result<Vec<Instruction>, String> {
    match condition {
        IRExpression::BinaryOperation(
            IRFieldType {
                ty: ty @ (InnerFieldType::Float | InnerFieldType::Double),
                array_depth: 0,
            },
            lhs,
            op @ (BinaryOperator::Lt
            | BinaryOperator::Le
            | BinaryOperator::Gt
            | BinaryOperator::Ge
            | BinaryOperator::Eq
            | BinaryOperator::Ne),
            rhs,
        ) => {
            // put the floats on the stack
            let mut code = compile_expression(*lhs, class)?;
            code.extend(compile_expression(*rhs, class)?);
            // get the comparison value for the floats
            let is_lt = matches!(op, BinaryOperator::Le | BinaryOperator::Lt);
            code.push(match ty {
                InnerFieldType::Float => Instruction::FloatCmp(is_lt),
                InnerFieldType::Double => Instruction::DoubleCmp(is_lt),
                _ => unreachable!(),
            });
            code.push(Instruction::IfZcmp(
                ICmp::try_from(op).unwrap(),
                branch_label,
            ));
            Ok(code)
        }
        IRExpression::BinaryOperation(
            IRFieldType {
                ty: InnerFieldType::Int | InnerFieldType::Byte | InnerFieldType::Short,
                array_depth: 0,
            },
            lhs,
            op @ (BinaryOperator::Lt
            | BinaryOperator::Le
            | BinaryOperator::Gt
            | BinaryOperator::Ge
            | BinaryOperator::Eq
            | BinaryOperator::Ne),
            rhs,
        ) => {
            // put the first int on the stack
            let mut code = compile_expression(*lhs, class)?;
            // either put the second one on the stack or use a zero comparison
            if *rhs == IRExpression::Int(0) {
                code.push(Instruction::IfZcmp(
                    ICmp::try_from(op).unwrap(),
                    branch_label,
                ));
            } else {
                code.extend(compile_expression(*rhs, class)?);
                code.push(Instruction::IfIcmp(
                    ICmp::try_from(op).unwrap(),
                    branch_label,
                ));
            }
            Ok(code)
        }
        condition => {
            let mut code = compile_expression(condition, class)?;
            code.push(Instruction::IfZcmp(ICmp::Ne, branch_label));
            Ok(code)
        }
    }
}

/// returns Ok((get, set))
fn compile_modify(
    expression: IRExpression,
    modification: Vec<Instruction>,
    class: &mut Class,
) -> Result<Vec<Instruction>, String> {
    match expression {
        IRExpression::LocalVar(ty, idx) => {
            let mut code = vec![Instruction::Load(ty.to_primitive(), idx as u8)];
            code.extend(modification);
            code.push(Instruction::Store(ty.to_primitive(), idx as u8));
            Ok(code)
        }
        other => Err(format!("{other:?} isn't a data location")),
    }
}
