use jvmrs_lib::{access, FieldType, MethodDescriptor};

use crate::{
    interpreter::{context::FunctionInfo, ir::IRFunction},
    types::IRFieldType,
};

use self::class::{Class, MethodInfo};

mod class;
mod instruction;

pub fn compile(syn: Vec<IRFunction>) -> Result<Class, String> {
    let mut class = Class::new("Chai".into(), "java/lang/Object".into());

    for func in syn {
        let func = compile_function(&mut class, func)?;
        class.register_method(func);
    }

    Ok(class)
}

fn compile_function(class: &mut Class, func: IRFunction) -> Result<MethodInfo, String> {
    let mut function = MethodInfo {
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
        body: Vec::new(),
    };
    Ok(function)
}
