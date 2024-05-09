use std::collections::HashMap;

use jvmrs_lib::{access, Constant, FieldType, MethodDescriptor};

use crate::{
    interpreter::{
        context::FunctionInfo,
        ir::{IRFunction, IRLocation, IRStatement, Symbol},
    },
    types::IRFieldType,
};

use self::{
    class::{Class, MethodInfo},
    instruction::{Const, Instruction},
};

mod class;
mod instruction;

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
    let mut body = Vec::new();
    let mut symbol_table: HashMap<Symbol, usize> = HashMap::new();

    for instr in func.body {
        match instr {
            IRStatement::Push(item) => match item {
                // TODO: ahhh typing constants hard
                // IRLocation::Float(f) if f == 0.0 => body.push(Instruction::Push(Const::F0)),
                // IRLocation::Int(0) => body.push(Instruction::Push(Const::I0)),
                IRLocation::String(s) => {
                    let s_index = class.register_constant(Constant::StringRef(s));
                    body.push(Instruction::LoadConst(s_index as u16));
                }
                IRLocation::Void => return Err(String::from("Can't push `void` onto the stack")),
                IRLocation::Stack => {}
                _ => todo!(),
            },
            IRStatement::Pop => todo!(),
            IRStatement::Invoke(_) => todo!(),
            IRStatement::Branch(_, _) => todo!(),
            IRStatement::Jump(_) => todo!(),
            IRStatement::Label(_) => todo!(),
            IRStatement::Move(_, _) => todo!(),
            // IRStatement::BinaryOperation(lhs, op, rhs) => {
                
            // },
            IRStatement::UnaryOperation(_, _) => todo!(),
            IRStatement::MakeTuple(_) => todo!(),
            IRStatement::Return => todo!(),
            other => return Err(format!("Unimplemented instruction: {other:?}"))
        }
    }

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
        body,
    })
}
