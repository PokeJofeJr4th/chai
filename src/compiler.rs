use std::{collections::HashMap, sync::Arc};

use jvmrs_lib::{access, method, Constant, FieldType, MethodDescriptor};

use crate::{
    interpreter::{
        context::FunctionInfo,
        ir::{IRExpression, IRFunction, Symbol},
    },
    parser::syntax::{BinaryOperator, UnaryOperator},
    types::{IRFieldType, InnerFieldType},
};

use self::{
    class::{AttributeInfo, Class, MethodInfo},
    instruction::{Const, ICmp, Operator, PrimitiveType, VerificationType},
};

mod class;
pub mod instruction;

type Instruction = instruction::Instruction<Symbol>;

/// # Errors
pub fn compile(syn: Vec<IRFunction>, class_name: Arc<str>) -> Result<Class, String> {
    let mut class = Class::new(class_name, "java/lang/Object".into());

    for func in syn {
        let func = compile_function(&mut class, func)?;
        class.register_method(func);
    }

    Ok(class)
}

fn write_verification_type(ty: &VerificationType, class: &mut Class, bytes: &mut Vec<u8>) {
    match ty {
        VerificationType::Top => bytes.push(0),
        VerificationType::Int => bytes.push(1),
        VerificationType::Float => bytes.push(2),
        VerificationType::Long => bytes.push(4),
        VerificationType::Double => bytes.push(3),
        VerificationType::Null => bytes.push(5),
        VerificationType::UninitializedThis => bytes.push(6),
        VerificationType::Object(obj) => {
            bytes.push(7);
            bytes.extend((class.register_constant(Constant::ClassRef(obj.clone()))).to_be_bytes());
        }
        VerificationType::UninitializedVar(i) => {
            bytes.push(8);
            bytes.extend(i.to_be_bytes());
        }
    }
}

#[allow(clippy::too_many_lines)]
fn compile_function(class: &mut Class, func: IRFunction) -> Result<MethodInfo, String> {
    let mut body: Vec<Instruction> = Vec::new();
    body.extend(compile_expression(func.body, class)?);
    if !matches!(body.last(), Some(Instruction::Goto(_))) {
        match &func.ret {
            Some(ret) => body.push(Instruction::Return(ret.to_primitive())),
            None => body.push(Instruction::ReturnVoid),
        }
    }
    println!("{body:?}");
    println!("{:?}", func.locals);

    let mut code = Vec::new();

    // max stack
    code.extend(u16::MAX.to_be_bytes());
    // max locals
    #[allow(clippy::cast_possible_truncation)]
    code.extend(
        (func
            .locals
            .iter()
            .map(|loc| loc.to_field_type().get_size())
            .sum::<usize>() as u16
            + 1)
        .to_be_bytes(),
    );

    // code_length
    // code bytes
    let mut symbol_table: HashMap<Symbol, usize> = HashMap::new();
    let mut required_symbols: Vec<usize> = Vec::new();
    let mut offset: usize = 0;
    let verification_type_map = generate_stack_map(&body);
    for instr in &body {
        if let Instruction::Label(label) = instr {
            symbol_table.insert(label.clone(), offset);
            required_symbols.push(offset);
        }
        offset += instr.size(class);
    }
    let verification_type_map: HashMap<usize, Vec<VerificationType>> = verification_type_map
        .into_iter()
        .map(|(k, v)| (*symbol_table.get(&k).unwrap(), v))
        .collect();
    println!("{offset}");
    let mut code_bytes = Vec::new();
    let mut offset = 0;
    #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    for instr in body {
        let instr = instr.map(|sym| *symbol_table.get(&sym).unwrap() as i16 - offset);
        println!("{offset} {} {instr:?}", code_bytes.len());
        offset += instr.size(class) as i16;
        instr
            .write(class, &mut code_bytes)
            .map_err(|e| e.to_string())?;
    }
    println!("{offset} {}", code_bytes.len());
    #[allow(clippy::cast_possible_truncation)]
    code.extend((code_bytes.len() as u32).to_be_bytes());
    code.extend(code_bytes);
    // exception table
    code.extend([0, 0]);
    // attributes
    code.extend(1u16.to_be_bytes());
    // stack map attribute
    let mut last_pc: usize = 0;
    let mut stack_map = Vec::new();
    required_symbols.dedup();
    required_symbols.retain(|x| *x != 0);
    let local_types: Vec<_> = func
        .locals
        .into_iter()
        .map(|v| VerificationType::from(v.to_field_type()))
        .collect();
    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    if required_symbols.first() == Some(&0) {
        stack_map.push(255);
        stack_map.extend((offset as u16).to_be_bytes());
        stack_map.extend((local_types.len() as u16).to_be_bytes());
        for t in &local_types {
            write_verification_type(t, class, &mut stack_map);
        }
        stack_map.extend((0u16).to_be_bytes());
        required_symbols.remove(0);
    }
    #[allow(clippy::cast_possible_truncation)]
    for (idx, &pc) in required_symbols.iter().enumerate() {
        let offset = if idx == 0 {
            pc - last_pc
        } else {
            pc - last_pc - 1
        };
        if let Some(stack_types) = verification_type_map.get(&pc) {
            stack_map.push(255);
            stack_map.extend((offset as u16).to_be_bytes());
            stack_map.extend((local_types.len() as u16).to_be_bytes());
            for t in &local_types {
                write_verification_type(t, class, &mut stack_map);
            }
            stack_map.extend((stack_types.len() as u16).to_be_bytes());
            for t in stack_types {
                write_verification_type(t, class, &mut stack_map);
            }
        } else if offset <= 63 {
            stack_map.push(offset as u8);
        } else {
            stack_map.push(251);
            stack_map.extend((offset as u16).to_be_bytes());
        }
        last_pc = pc;
    }
    // write in the stack map
    code.extend((class.register_constant(Constant::String("StackMapTable".into()))).to_be_bytes());
    #[allow(clippy::cast_possible_truncation)]
    code.extend((stack_map.len() as u32 + 2).to_be_bytes());
    #[allow(clippy::cast_possible_truncation)]
    code.extend((required_symbols.len() as u16).to_be_bytes());
    code.extend(stack_map);

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
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
fn compile_expression(
    expression: IRExpression,
    class: &mut Class,
) -> Result<Vec<Instruction>, String> {
    match expression {
        IRExpression::Void => Ok(Vec::new()),
        IRExpression::LocalVar(ty, local_idx) => {
            Ok(vec![Instruction::Load(ty.to_primitive(), local_idx as u8)])
        }
        IRExpression::String(s) => Ok(vec![Instruction::LoadConst(Constant::StringRef(s))]),
        IRExpression::Int(i) => Ok(vec![match i {
            -1 => Instruction::Push(Const::IM1),
            0 => Instruction::Push(Const::I0),
            1 => Instruction::Push(Const::I1),
            2 => Instruction::Push(Const::I2),
            3 => Instruction::Push(Const::I3),
            4 => Instruction::Push(Const::I4),
            5 => Instruction::Push(Const::I5),
            i => i16::try_from(i).map_or_else(
                |_| Instruction::LoadConst(Constant::Int(i)),
                |i| Instruction::PushByte(i as u16),
            ),
        }]),
        IRExpression::Long(_) => todo!(),
        IRExpression::Float(f) => Ok(vec![if f == 0.0 {
            Instruction::Push(Const::F0)
        } else if (f - 1.0).abs() < f32::EPSILON {
            Instruction::Push(Const::F1)
        } else if (f - 2.0).abs() < f32::EPSILON {
            Instruction::Push(Const::F2)
        } else {
            Instruction::LoadConst(Constant::Float(f))
        }]),
        IRExpression::Double(_) => todo!(),
        IRExpression::MakeTuple(values) => {
            #[allow(clippy::cast_possible_wrap)]
            let mut code = compile_expression(IRExpression::Int(values.len() as i32), class)?;
            code.push(Instruction::ReferenceArray(FieldType::Object(
                "java/lang/Object".into(),
            )));
            for (idx, value) in values.into_iter().enumerate() {
                code.push(Instruction::Dup);
                #[allow(clippy::cast_possible_wrap)]
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
            compile_modify(*lhs, code)
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
        IRExpression::BinaryOperation(ty, lhs, BinaryOperator::Set, rhs) => {
            let mut code = compile_expression(*rhs, class)?;
            let IRExpression::LocalVar(_, idx) = &*lhs else {
                return Err(format!("Expected a local variable; got `{lhs:?}`"));
            };
            code.push(Instruction::Store(ty.to_primitive(), *idx as u8));
            Ok(code)
        }
        IRExpression::BinaryOperation(ty, lhs, op, rhs) => todo!("{lhs:?} {op:?}<{ty:?}> {rhs:?}"),
        IRExpression::UnaryOperation(ty, UnaryOperator::Unbox, inner) => {
            let InnerFieldType::Object { base, generics: _ } = ty.ty else {
                panic!("Can't unbox `{ty:?}`")
            };
            let primitive = match &*base {
                "java/lang/Boolean" => InnerFieldType::Boolean,
                "java/lang/Byte" => InnerFieldType::Byte,
                "java/lang/Short" => InnerFieldType::Short,
                "java/lang/Integer" => InnerFieldType::Int,
                "java/lang/Long" => InnerFieldType::Long,
                "java/lang/Float" => InnerFieldType::Float,
                "java/lang/Double" => InnerFieldType::Double,
                "java/lang/Character" => InnerFieldType::Char,
                other => panic!("Can't unbox `{other}`"),
            };
            let mut code = compile_expression(*inner, class)?;
            code.push(Instruction::InvokeVirtual(
                base,
                format!("{primitive:?}Value").into(),
                MethodDescriptor {
                    parameter_size: 0,
                    parameters: Vec::new(),
                    return_type: Some(primitive.to_field_type()),
                },
            ));
            Ok(code)
        }
        IRExpression::UnaryOperation(ty, UnaryOperator::Box, inner) => {
            let class_name: Arc<str> = Arc::from(match ty.ty {
                InnerFieldType::Boolean => "java/lang/Boolean",
                InnerFieldType::Byte => "java/lang/Byte",
                InnerFieldType::Short => "java/lang/Short",
                InnerFieldType::Int => "java/lang/Integer",
                InnerFieldType::Long => "java/lang/Long",
                InnerFieldType::Float => "java/lang/Float",
                InnerFieldType::Double => "java/lang/Double",
                InnerFieldType::Char => "java/lang/Character",
                InnerFieldType::Object { base, generics } => {
                    unreachable!("Can't box object {base}<{generics:?}>")
                }
                InnerFieldType::Tuple(ty) => unreachable!("Can't box tuple {ty:?}"),
            });
            let mut code = compile_expression(*inner, class)?;
            code.push(Instruction::InvokeStatic(
                class_name.clone(),
                Arc::from("valueOf"),
                MethodDescriptor {
                    parameter_size: ty.get_size(),
                    parameters: vec![ty.to_field_type()],
                    return_type: Some(FieldType::Object(class_name)),
                },
            ));
            Ok(code)
        }
        IRExpression::UnaryOperation(ty, op, inner) => todo!("{op:?} <{ty:?}> {inner:?}"),
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
        IRExpression::Invoke(func, args) if func.class.split('/').next() == Some("chai") => {
            match (&*func, &args[..]) {
                (FunctionInfo { name, .. }, []) if &**name == "none" => {
                    Ok(vec![Instruction::InvokeStatic(
                        "java/util/Optional".into(),
                        "empty".into(),
                        method!(() -> Object("java/util/Optional".into())),
                    )])
                }
                (FunctionInfo { name, .. }, [one]) if &**name == "some" => {
                    let mut code = compile_expression(one.clone(), class)?;
                    code.push(Instruction::InvokeStatic("java/util/Optional".into(), "of".into(), method!(((Object("java/lang/Object".into()))) -> Object("java/util/Optional".into()))));
                    Ok(code)
                }
                (FunctionInfo { name, params, .. }, [one]) if &**name == "print" => {
                    let mut code = vec![Instruction::GetStatic(
                        "java/lang/System".into(),
                        "out".into(),
                        FieldType::Object("java/io/PrintStream".into()),
                    )];
                    code.extend(compile_expression(one.clone(), class)?);
                    let print_ty = params[0].to_field_type();
                    code.push(Instruction::InvokeVirtual(
                        "java/io/PrintStream".into(),
                        "println".into(),
                        {
                            MethodDescriptor {
                                parameter_size: print_ty.get_size(),
                                parameters: vec![print_ty],
                                return_type: None,
                            }
                        },
                    ));
                    Ok(code)
                }
                (func, args) => todo!("{func:?} {args:?}"),
            }
        }
        IRExpression::Invoke(func, args) => {
            let func_args: Vec<_> = func.params.iter().map(IRFieldType::to_field_type).collect();
            let method_type = MethodDescriptor {
                parameter_size: func_args.iter().map(FieldType::get_size).sum(),
                parameters: func_args,
                return_type: if func.ret.is_void() {
                    None
                } else {
                    Some(func.ret.to_field_type())
                },
            };

            let mut code = Vec::new();
            for arg in args {
                code.extend(compile_expression(arg, class)?);
            }
            if func.access.is_static() {
                code.push(Instruction::InvokeStatic(
                    func.class.clone(),
                    func.name.clone(),
                    method_type,
                ));
            } else {
                code.push(Instruction::InvokeVirtual(
                    func.class.clone(),
                    func.name.clone(),
                    method_type,
                ));
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
        IRExpression::Void => Ok(vec![Instruction::Goto(branch_label)]),
        condition => {
            let mut code = compile_expression(condition, class)?;
            code.push(Instruction::IfZcmp(ICmp::Ne, branch_label));
            Ok(code)
        }
    }
}

/// returns Ok((get, set))
#[allow(clippy::cast_possible_truncation)]
fn compile_modify(
    expression: IRExpression,
    modification: Vec<Instruction>,
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

#[allow(clippy::too_many_lines)]
fn generate_stack_map(code: &[Instruction]) -> HashMap<Symbol, Vec<VerificationType>> {
    fn find_symbol(code: &[Instruction], sym: &Symbol) -> usize {
        code.iter()
            .position(|i| matches!(i, Instruction::Label(l) if l == sym))
            .unwrap()
    }

    fn visit_stack_map(
        code: &[Instruction],
        index: u16,
        current_stack: &mut Vec<VerificationType>,
        finished_stacks: &mut HashMap<Symbol, Vec<VerificationType>>,
    ) {
        for i in index.. {
            #[allow(clippy::cast_possible_truncation)]
            match &code[i as usize] {
                Instruction::Goto(label) => {
                    if !finished_stacks.contains_key(label) {
                        finished_stacks.insert(label.clone(), current_stack.clone());
                        visit_stack_map(
                            code,
                            find_symbol(code, label) as u16,
                            current_stack,
                            finished_stacks,
                        );
                    }
                    return;
                }
                Instruction::IfACmp(_, sym) | Instruction::IfIcmp(_, sym) => {
                    current_stack.pop();
                    current_stack.pop();
                    if !finished_stacks.contains_key(sym) {
                        finished_stacks.insert(sym.clone(), current_stack.clone());
                        visit_stack_map(
                            code,
                            find_symbol(code, sym) as u16,
                            &mut current_stack.clone(),
                            finished_stacks,
                        );
                    }
                }
                Instruction::IfZcmp(_, sym) => {
                    current_stack.pop();
                    if !finished_stacks.contains_key(sym) {
                        finished_stacks.insert(sym.clone(), current_stack.clone());
                        visit_stack_map(
                            code,
                            find_symbol(code, sym) as u16,
                            &mut current_stack.clone(),
                            finished_stacks,
                        );
                    }
                }
                Instruction::Operate(
                    _,
                    Operator::Add
                    | Operator::And
                    | Operator::Div
                    | Operator::Mul
                    | Operator::Rem
                    | Operator::Shl
                    | Operator::Shr
                    | Operator::Sub
                    | Operator::Ushr
                    | Operator::Xor,
                )
                | Instruction::Store(..) => {
                    current_stack.pop();
                }
                Instruction::Load(ty, _) => {
                    current_stack.push(VerificationType::from(*ty));
                }
                Instruction::Push(c) => {
                    current_stack.push(VerificationType::from(PrimitiveType::from(*c)));
                }
                Instruction::FloatCmp(_) | Instruction::DoubleCmp(_) | Instruction::LCmp => {
                    current_stack.pop();
                    current_stack.pop();
                    current_stack.push(VerificationType::Int);
                }
                Instruction::Return(_) | Instruction::ReturnVoid => return,
                Instruction::Label(sym) => {
                    if !finished_stacks.contains_key(sym) {
                        finished_stacks.insert(sym.clone(), current_stack.clone());
                    }
                }
                Instruction::InvokeStatic(_, _, descriptor) => {
                    for _ in &descriptor.parameters {
                        // TODO: verify the types?
                        current_stack.pop();
                    }
                    if let Some(ret) = descriptor.return_type.clone() {
                        current_stack.push(VerificationType::from(ret));
                    }
                }
                Instruction::InvokeVirtual(_, _, descriptor)
                | Instruction::InvokeInterface(_, _, descriptor, _) => {
                    for _ in &descriptor.parameters {
                        // TODO: verify the types?
                        current_stack.pop();
                    }
                    current_stack.pop();
                    if let Some(ret) = descriptor.return_type.clone() {
                        current_stack.push(VerificationType::from(ret));
                    }
                }
                Instruction::ReferenceArray(a) => {
                    current_stack.push(VerificationType::from(a.clone()));
                }
                Instruction::ArrayStore(_) => {
                    current_stack.pop();
                    current_stack.pop();
                    current_stack.pop();
                }
                Instruction::ArrayLoad(ty) => {
                    current_stack.pop();
                    current_stack.pop();
                    current_stack.push(VerificationType::from(*ty));
                }
                Instruction::Dup | Instruction::Dup2 => {
                    current_stack.push(current_stack.last().unwrap().clone());
                }
                Instruction::PushByte(_) => current_stack.push(VerificationType::Int),
                Instruction::LoadConst(c) | Instruction::LoadConst2(c) => match c {
                    Constant::Int(_) => current_stack.push(VerificationType::Int),
                    Constant::Long(_) => current_stack.push(VerificationType::Long),
                    Constant::Float(_) => current_stack.push(VerificationType::Float),
                    Constant::Double(_) => current_stack.push(VerificationType::Double),
                    Constant::StringRef(_) | Constant::String(_) => {
                        current_stack.push(VerificationType::Object("java/lang/String".into()));
                    }
                    other => todo!("Load Const {other:?}"),
                },
                Instruction::GetStatic(_, _, ty) => {
                    current_stack.push(VerificationType::from(ty.clone()));
                }
                other => todo!("Visit StackMap {other:?}"),
            }
        }
    }

    let mut map = HashMap::new();
    visit_stack_map(code, 0, &mut Vec::new(), &mut map);
    println!("Stack Map: {map:?}");
    map
}
