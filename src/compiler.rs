use std::collections::HashMap;

use jvmrs_lib::{access, Constant, FieldType, MethodDescriptor};

use crate::{
    interpreter::{
        context::FunctionInfo,
        ir::{IRExpression, IRFunction, IRStatement, Symbol},
    },
    types::IRFieldType,
};

use self::{
    class::{AttributeInfo, Class, MethodInfo},
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
    let mut body: Vec<Instruction> = Vec::new();
    let mut symbol_table: HashMap<Symbol, usize> = HashMap::new();

    let mut code = Vec::new();

    // max stack
    code.extend(0u16.to_be_bytes());
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
    let mut code_bytes = Vec::new();
    for instr in body {
        instr.write(&mut code_bytes);
    }
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
