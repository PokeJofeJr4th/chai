use std::{
    io::{Error, Write},
    sync::Arc,
};

use jvmrs_lib::{Constant, FieldType, MethodDescriptor};

use crate::parser::syntax::BinaryOperator;

use super::class::Class;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    Boolean,
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Char,
    Reference,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    Neg,
    Shl,
    Shr,
    Ushr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Const {
    D0,
    D1,
    F0,
    F1,
    F2,
    IM1,
    I0,
    I1,
    I2,
    I3,
    I4,
    I5,
    L0,
    L1,
    Null,
}

impl From<Const> for PrimitiveType {
    fn from(value: Const) -> Self {
        match value {
            Const::D0 | Const::D1 => Self::Double,
            Const::F0 | Const::F1 | Const::F2 => Self::Float,
            Const::IM1 | Const::I0 | Const::I1 | Const::I2 | Const::I3 | Const::I4 | Const::I5 => {
                Self::Int
            }
            Const::L0 | Const::L1 => Self::Long,
            Const::Null => Self::Reference,
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ICmp {
    Eq = 0,
    Ne = 1,
    Lt = 2,
    Ge = 3,
    Gt = 4,
    Le = 5,
}

impl TryFrom<BinaryOperator> for ICmp {
    type Error = ();
    fn try_from(value: BinaryOperator) -> Result<Self, Self::Error> {
        match value {
            BinaryOperator::Eq => Ok(Self::Eq),
            BinaryOperator::Ne => Ok(Self::Ne),
            BinaryOperator::Lt => Ok(Self::Lt),
            BinaryOperator::Le => Ok(Self::Le),
            BinaryOperator::Gt => Ok(Self::Gt),
            BinaryOperator::Ge => Ok(Self::Ge),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction<Label = i16> {
    Nop,
    Pop,
    Pop2,
    NewArray(PrimitiveType),
    MultiANewArray(FieldType, u8),
    Convert(PrimitiveType, PrimitiveType),
    ReferenceArray(FieldType),
    ArrayLoad(PrimitiveType),
    ArrayStore(PrimitiveType),
    Load(PrimitiveType, u8),
    Store(PrimitiveType, u8),
    Return(PrimitiveType),
    ReturnVoid,
    Operate(PrimitiveType, Operator),
    PushByte(u16),
    Cast(FieldType),
    /// false -> g, true -> l
    DoubleCmp(bool),
    /// false -> g, true -> l
    FloatCmp(bool),
    Push(Const),
    Arraylength,
    GetField(Arc<str>, Arc<str>, FieldType),
    GetStatic(Arc<str>, Arc<str>, FieldType),
    Goto(Label),
    Label(Label),
    IfACmp(bool, Label),
    IfIcmp(ICmp, Label),
    LCmp,
    IfZcmp(ICmp, Label),
    IfNull(Label),
    IfNonNull(Label),
    Dup,
    IInc(u8, u8),
    Instanceof(Arc<str>),
    InvokeDynamic(u16),
    InvokeInterface(Arc<str>, Arc<str>, MethodDescriptor, u8),
    InvokeSpecial(Arc<str>, Arc<str>, MethodDescriptor),
    InvokeVirtual(Arc<str>, Arc<str>, MethodDescriptor),
    InvokeStatic(Arc<str>, Arc<str>, MethodDescriptor),
    PutField(Arc<str>, Arc<str>, FieldType),
    PutStatic(Arc<str>, Arc<str>, FieldType),
    LoadConst(Constant),
    LoadConst2(Constant),
    New(Arc<str>),
    Dup2,
    Throw,
}

impl Instruction {
    /// # Errors
    #[allow(clippy::too_many_lines)]
    pub fn write(&self, class: &mut Class, writer: &mut impl Write) -> Result<(), Error> {
        match self {
            Self::Label(_) => Ok(()),
            Self::Nop => writer.write_all(&[0]),
            Self::Push(Const::Null) => writer.write_all(&[0x1]),
            Self::Push(Const::IM1) => writer.write_all(&[0x2]),
            Self::Push(Const::I0) => writer.write_all(&[0x3]),
            Self::Push(Const::I1) => writer.write_all(&[0x4]),
            Self::Push(Const::I2) => writer.write_all(&[0x5]),
            Self::Push(Const::I3) => writer.write_all(&[0x6]),
            Self::Push(Const::I4) => writer.write_all(&[0x7]),
            Self::Push(Const::I5) => writer.write_all(&[0x8]),
            Self::Push(Const::L0) => writer.write_all(&[0x9]),
            Self::Push(Const::L1) => writer.write_all(&[0xA]),
            Self::Push(Const::F0) => writer.write_all(&[0xB]),
            Self::Push(Const::F1) => writer.write_all(&[0xC]),
            Self::Push(Const::F2) => writer.write_all(&[0xD]),
            Self::Push(Const::D0) => writer.write_all(&[0xE]),
            Self::Push(Const::D1) => writer.write_all(&[0xF]),
            Self::PushByte(b) => {
                if let Ok(b) = u8::try_from(*b) {
                    writer.write_all(&[0x10, b])
                } else {
                    writer.write_all(&[0x11])?;
                    writer.write_all(&b.to_be_bytes())
                }
            }
            Self::LoadConst(c) => {
                let i = class.register_constant(c.clone());
                if let Ok(i) = u8::try_from(i) {
                    writer.write_all(&[0x12, i])
                } else {
                    writer.write_all(&[0x13])?;
                    writer.write_all(&i.to_be_bytes())
                }
            }
            Self::LoadConst2(c) => {
                let i = class.register_constant(c.clone());
                writer.write_all(&[0x14])?;
                writer.write_all(&i.to_be_bytes())
            }
            Self::Load(PrimitiveType::Int, x @ 0..=3) => writer.write_all(&[0x1A + *x]),
            Self::Load(PrimitiveType::Int, x) => writer.write_all(&[0x15, *x]),
            Self::Load(PrimitiveType::Long, x @ 0..=3) => writer.write_all(&[0x1E + *x]),
            Self::Load(PrimitiveType::Long, x) => writer.write_all(&[0x16, *x]),
            Self::Load(PrimitiveType::Float, x @ 0..=3) => writer.write_all(&[0x22 + *x]),
            Self::Load(PrimitiveType::Float, x) => writer.write_all(&[0x17, *x]),
            Self::Load(PrimitiveType::Double, x @ 0..=3) => writer.write_all(&[0x26 + *x]),
            Self::Load(PrimitiveType::Double, x) => writer.write_all(&[0x18, *x]),
            Self::Load(PrimitiveType::Reference, x @ 0..=3) => writer.write_all(&[0x2A + *x]),
            Self::Load(PrimitiveType::Reference, x) => writer.write_all(&[0x19, *x]),
            Self::ArrayLoad(PrimitiveType::Int) => writer.write_all(&[0x2E]),
            Self::ArrayLoad(PrimitiveType::Long) => writer.write_all(&[0x2F]),
            Self::ArrayLoad(PrimitiveType::Float) => writer.write_all(&[0x30]),
            Self::ArrayLoad(PrimitiveType::Double) => writer.write_all(&[0x31]),
            Self::ArrayLoad(PrimitiveType::Reference) => writer.write_all(&[0x32]),
            Self::ArrayLoad(PrimitiveType::Byte | PrimitiveType::Boolean) => {
                writer.write_all(&[0x33])
            }
            Self::ArrayLoad(PrimitiveType::Char) => writer.write_all(&[0x34]),
            Self::ArrayLoad(PrimitiveType::Short) => writer.write_all(&[0x35]),
            Self::Store(PrimitiveType::Int, x @ 0..=3) => writer.write_all(&[0x3B + *x]),
            Self::Store(PrimitiveType::Int, x) => writer.write_all(&[0x36, *x]),
            Self::Store(PrimitiveType::Long, x @ 0..=3) => writer.write_all(&[0x3F + *x]),
            Self::Store(PrimitiveType::Long, x) => writer.write_all(&[0x37, *x]),
            Self::Store(PrimitiveType::Float, x @ 0..=3) => writer.write_all(&[0x43 + *x]),
            Self::Store(PrimitiveType::Float, x) => writer.write_all(&[0x38, *x]),
            Self::Store(PrimitiveType::Double, x @ 0..=3) => writer.write_all(&[0x49 + *x]),
            Self::Store(PrimitiveType::Double, x) => writer.write_all(&[0x39, *x]),
            Self::Store(PrimitiveType::Reference, x @ 0..=3) => writer.write_all(&[0x4B + *x]),
            Self::Store(PrimitiveType::Reference, x) => writer.write_all(&[0x3A, *x]),
            Self::ArrayStore(PrimitiveType::Int) => writer.write_all(&[0x4F]),
            Self::ArrayStore(PrimitiveType::Long) => writer.write_all(&[0x50]),
            Self::ArrayStore(PrimitiveType::Float) => writer.write_all(&[0x51]),
            Self::ArrayStore(PrimitiveType::Double) => writer.write_all(&[0x52]),
            Self::ArrayStore(PrimitiveType::Reference) => writer.write_all(&[0x53]),
            Self::ArrayStore(PrimitiveType::Byte | PrimitiveType::Boolean) => {
                writer.write_all(&[0x54])
            }
            Self::ArrayStore(PrimitiveType::Char) => writer.write_all(&[0x55]),
            Self::ArrayStore(PrimitiveType::Short) => writer.write_all(&[0x56]),
            Self::Pop => writer.write_all(&[0x57]),
            Self::Pop2 => writer.write_all(&[0x58]),
            Self::Dup => writer.write_all(&[0x59]),
            Self::Dup2 => writer.write_all(&[0x5C]),
            Self::Operate(PrimitiveType::Int, Operator::Add) => writer.write_all(&[0x60]),
            Self::Operate(PrimitiveType::Long, Operator::Add) => writer.write_all(&[0x61]),
            Self::Operate(PrimitiveType::Float, Operator::Add) => writer.write_all(&[0x62]),
            Self::Operate(PrimitiveType::Double, Operator::Add) => writer.write_all(&[0x63]),
            Self::Operate(PrimitiveType::Int, Operator::Sub) => writer.write_all(&[0x64]),
            Self::Operate(PrimitiveType::Long, Operator::Sub) => writer.write_all(&[0x65]),
            Self::Operate(PrimitiveType::Float, Operator::Sub) => writer.write_all(&[0x66]),
            Self::Operate(PrimitiveType::Double, Operator::Sub) => writer.write_all(&[0x67]),
            Self::Operate(PrimitiveType::Int, Operator::Mul) => writer.write_all(&[0x68]),
            Self::Operate(PrimitiveType::Long, Operator::Mul) => writer.write_all(&[0x69]),
            Self::Operate(PrimitiveType::Float, Operator::Mul) => writer.write_all(&[0x6A]),
            Self::Operate(PrimitiveType::Double, Operator::Mul) => writer.write_all(&[0x6B]),
            Self::Operate(PrimitiveType::Int, Operator::Div) => writer.write_all(&[0x6C]),
            Self::Operate(PrimitiveType::Long, Operator::Div) => writer.write_all(&[0x6D]),
            Self::Operate(PrimitiveType::Float, Operator::Div) => writer.write_all(&[0x6E]),
            Self::Operate(PrimitiveType::Double, Operator::Div) => writer.write_all(&[0x6F]),
            Self::Operate(PrimitiveType::Int, Operator::Rem) => writer.write_all(&[0x70]),
            Self::Operate(PrimitiveType::Long, Operator::Rem) => writer.write_all(&[0x71]),
            Self::Operate(PrimitiveType::Float, Operator::Rem) => writer.write_all(&[0x72]),
            Self::Operate(PrimitiveType::Double, Operator::Rem) => writer.write_all(&[0x73]),
            Self::Operate(PrimitiveType::Int, Operator::Neg) => writer.write_all(&[0x74]),
            Self::Operate(PrimitiveType::Long, Operator::Neg) => writer.write_all(&[0x75]),
            Self::Operate(PrimitiveType::Float, Operator::Neg) => writer.write_all(&[0x76]),
            Self::Operate(PrimitiveType::Double, Operator::Neg) => writer.write_all(&[0x77]),
            Self::Operate(PrimitiveType::Int, Operator::Shl) => writer.write_all(&[0x78]),
            Self::Operate(PrimitiveType::Long, Operator::Shl) => writer.write_all(&[0x79]),
            Self::Operate(PrimitiveType::Int, Operator::Shr) => writer.write_all(&[0x7A]),
            Self::Operate(PrimitiveType::Long, Operator::Shr) => writer.write_all(&[0x7B]),
            Self::Operate(PrimitiveType::Int, Operator::Ushr) => writer.write_all(&[0x7C]),
            Self::Operate(PrimitiveType::Long, Operator::Ushr) => writer.write_all(&[0x7D]),
            Self::Operate(PrimitiveType::Int, Operator::And) => writer.write_all(&[0x7E]),
            Self::Operate(PrimitiveType::Long, Operator::And) => writer.write_all(&[0x7F]),
            Self::Operate(PrimitiveType::Int, Operator::Or) => writer.write_all(&[0x80]),
            Self::Operate(PrimitiveType::Long, Operator::Or) => writer.write_all(&[0x81]),
            Self::Operate(PrimitiveType::Int, Operator::Xor) => writer.write_all(&[0x82]),
            Self::Operate(PrimitiveType::Long, Operator::Xor) => writer.write_all(&[0x83]),
            Self::IInc(i, c) => writer.write_all(&[0x84, *i, *c]),
            Self::Convert(PrimitiveType::Int, PrimitiveType::Long) => writer.write_all(&[0x85]),
            Self::Convert(PrimitiveType::Int, PrimitiveType::Float) => writer.write_all(&[0x86]),
            Self::Convert(PrimitiveType::Int, PrimitiveType::Double) => writer.write_all(&[0x87]),
            Self::Convert(PrimitiveType::Long, PrimitiveType::Int) => writer.write_all(&[0x88]),
            Self::Convert(PrimitiveType::Long, PrimitiveType::Float) => writer.write_all(&[0x89]),
            Self::Convert(PrimitiveType::Long, PrimitiveType::Double) => writer.write_all(&[0x8A]),
            Self::Convert(PrimitiveType::Float, PrimitiveType::Int) => writer.write_all(&[0x8B]),
            Self::Convert(PrimitiveType::Float, PrimitiveType::Long) => writer.write_all(&[0x8C]),
            Self::Convert(PrimitiveType::Float, PrimitiveType::Double) => writer.write_all(&[0x8D]),
            Self::Convert(PrimitiveType::Double, PrimitiveType::Int) => writer.write_all(&[0x8E]),
            Self::Convert(PrimitiveType::Double, PrimitiveType::Long) => writer.write_all(&[0x8F]),
            Self::Convert(PrimitiveType::Double, PrimitiveType::Float) => writer.write_all(&[0x90]),
            Self::Convert(PrimitiveType::Int, PrimitiveType::Byte) => writer.write_all(&[0x91]),
            Self::Convert(PrimitiveType::Int, PrimitiveType::Char) => writer.write_all(&[0x92]),
            Self::Convert(PrimitiveType::Int, PrimitiveType::Short) => writer.write_all(&[0x93]),
            Self::LCmp => writer.write_all(&[0x94]),
            Self::FloatCmp(is_g) => writer.write_all(&[0x95 + u8::from(*is_g)]),
            Self::DoubleCmp(is_g) => writer.write_all(&[0x97 + u8::from(*is_g)]),
            Self::IfZcmp(c, b) => {
                writer.write_all(&[0x99 + *c as u8])?;
                writer.write_all(&b.to_be_bytes())
            }
            Self::IfIcmp(c, b) => {
                writer.write_all(&[0x9F + *c as u8])?;
                writer.write_all(&b.to_be_bytes())
            }
            Self::IfACmp(is_ne, b) => {
                writer.write_all(&[0xA5 + u8::from(*is_ne)])?;
                writer.write_all(&b.to_be_bytes())
            }
            Self::Goto(i) => {
                writer.write_all(&[0xA7])?;
                writer.write_all(&i.to_be_bytes())
            }
            Self::Return(PrimitiveType::Int) => writer.write_all(&[0xAC]),
            Self::Return(PrimitiveType::Long) => writer.write_all(&[0xAD]),
            Self::Return(PrimitiveType::Float) => writer.write_all(&[0xAE]),
            Self::Return(PrimitiveType::Double) => writer.write_all(&[0xAF]),
            Self::Return(PrimitiveType::Reference) => writer.write_all(&[0xB0]),
            Self::ReturnVoid => writer.write_all(&[0xB1]),
            Self::GetStatic(c, f, t) => {
                writer.write_all(&[0xB2])?;
                let i = class.register_constant(Constant::FieldRef {
                    class: c.clone(),
                    name: f.clone(),
                    field_type: t.clone(),
                });
                writer.write_all(&i.to_be_bytes())
            }
            Self::PutStatic(c, f, t) => {
                writer.write_all(&[0xB3])?;
                let i = class.register_constant(Constant::FieldRef {
                    class: c.clone(),
                    name: f.clone(),
                    field_type: t.clone(),
                });
                writer.write_all(&i.to_be_bytes())
            }
            Self::GetField(c, f, t) => {
                writer.write_all(&[0xB4])?;
                let i = class.register_constant(Constant::FieldRef {
                    class: c.clone(),
                    name: f.clone(),
                    field_type: t.clone(),
                });
                writer.write_all(&i.to_be_bytes())
            }
            Self::PutField(c, f, t) => {
                writer.write_all(&[0xB5])?;
                let i = class.register_constant(Constant::FieldRef {
                    class: c.clone(),
                    name: f.clone(),
                    field_type: t.clone(),
                });
                writer.write_all(&i.to_be_bytes())
            }
            Self::InvokeVirtual(c, m, d) => {
                writer.write_all(&[0xB6])?;
                let i = class.register_constant(Constant::MethodRef {
                    class: c.clone(),
                    name: m.clone(),
                    method_type: d.clone(),
                });
                writer.write_all(&i.to_be_bytes())
            }
            Self::InvokeSpecial(c, m, d) => {
                writer.write_all(&[0xB7])?;
                let i = class.register_constant(Constant::MethodRef {
                    class: c.clone(),
                    name: m.clone(),
                    method_type: d.clone(),
                });
                writer.write_all(&i.to_be_bytes())
            }
            Self::InvokeStatic(c, m, d) => {
                writer.write_all(&[0xB8])?;
                let i = class.register_constant(Constant::MethodRef {
                    class: c.clone(),
                    name: m.clone(),
                    method_type: d.clone(),
                });
                writer.write_all(&i.to_be_bytes())
            }
            Self::InvokeInterface(class_name, method_name, method_descriptor, n_args) => {
                writer.write_all(&[0xB9])?;
                let i = class.register_constant(Constant::MethodRef {
                    class: class_name.clone(),
                    name: method_name.clone(),
                    method_type: method_descriptor.clone(),
                });
                writer.write_all(&i.to_be_bytes())?;
                writer.write_all(&[*n_args, 0])
            }
            Self::InvokeDynamic(i) => {
                writer.write_all(&[0xBA])?;
                writer.write_all(&i.to_be_bytes())?;
                writer.write_all(&[0, 0])
            }
            Self::New(i) => {
                writer.write_all(&[0xBB])?;
                let i = class.register_constant(Constant::ClassRef(i.clone()));
                writer.write_all(&i.to_be_bytes())
            }
            Self::ReferenceArray(r) => {
                writer.write_all(&[0xBD])?;
                let i = class.register_constant(if let FieldType::Object(c) = r {
                    Constant::ClassRef(c.clone())
                } else {
                    Constant::ClassRef(r.repr())
                });
                writer.write_all(&i.to_be_bytes())
            }
            Self::NewArray(t) => writer.write_all(&[
                0xBC,
                match t {
                    PrimitiveType::Boolean => 4,
                    PrimitiveType::Byte => 8,
                    PrimitiveType::Short => 9,
                    PrimitiveType::Int => 10,
                    PrimitiveType::Long => 11,
                    PrimitiveType::Float => 6,
                    PrimitiveType::Double => 7,
                    PrimitiveType::Char => 5,
                    PrimitiveType::Reference => todo!(),
                },
            ]),
            Self::Arraylength => writer.write_all(&[0xBE]),
            Self::Throw => writer.write_all(&[0xBF]),
            Self::Cast(i) => {
                writer.write_all(&[0xC0])?;
                let i = class.register_constant(Constant::ClassRef(i.repr()));
                writer.write_all(&i.to_be_bytes())
            }
            Self::Instanceof(i) => {
                writer.write_all(&[0xC1])?;
                let i = class.register_constant(Constant::ClassRef(i.clone()));
                writer.write_all(&i.to_be_bytes())
            }
            Self::MultiANewArray(i, d) => {
                writer.write_all(&[0xC5])?;
                let i = class.register_constant(Constant::ClassRef(i.repr()));
                writer.write_all(&i.to_be_bytes())?;
                writer.write_all(&[*d])
            }
            Self::IfNull(branch) => {
                writer.write_all(&[0xC6])?;
                writer.write_all(&branch.to_be_bytes())
            }
            Self::IfNonNull(branch) => {
                writer.write_all(&[0xC7])?;
                writer.write_all(&branch.to_be_bytes())
            }
            _ => todo!(),
        }
    }
}

impl<T> Instruction<T> {
    pub fn size(&self, class: &mut Class) -> usize {
        match self {
            Self::Nop
            | Self::Pop
            | Self::Pop2
            | Self::ArrayLoad(_)
            | Self::ArrayStore(_)
            | Self::Push(_)
            | Self::Convert(_, _)
            | Self::Return(_)
            | Self::ReturnVoid
            | Self::Operate(_, _)
            | Self::DoubleCmp(_)
            | Self::FloatCmp(_)
            | Self::Arraylength
            | Self::Dup
            | Self::LCmp
            | Self::Dup2
            | Self::Throw => 1,
            Self::MultiANewArray(_, _) => 4,
            Self::Load(_, i) | Self::Store(_, i) => {
                if *i <= 3 {
                    1
                } else {
                    2
                }
            }
            Self::PushByte(i) => {
                if u8::try_from(*i).is_ok() {
                    2
                } else {
                    3
                }
            }
            Self::LoadConst(i) => {
                let i = class.register_constant(i.clone());
                if u8::try_from(i).is_ok() {
                    2
                } else {
                    3
                }
            }
            Self::Label(_) => 0,
            Self::NewArray(_)
            | Self::ReferenceArray(_)
            | Self::Cast(_)
            | Self::IfNull(_)
            | Self::IfNonNull(_)
            | Self::GetField(..)
            | Self::GetStatic(..)
            | Self::Goto(_)
            | Self::IfACmp(_, _)
            | Self::IfIcmp(_, _)
            | Self::IfZcmp(_, _)
            | Self::IInc(_, _)
            | Self::Instanceof(_)
            | Self::InvokeSpecial(..)
            | Self::InvokeVirtual(..)
            | Self::InvokeStatic(..)
            | Self::PutField(..)
            | Self::PutStatic(..)
            | Self::LoadConst2(_)
            | Self::New(_) => 3,
            Self::InvokeDynamic(_) | Self::InvokeInterface(..) => 5,
        }
    }

    pub fn map<U>(self, mut func: impl FnMut(T) -> U) -> Instruction<U> {
        match self {
            Self::Nop => Instruction::Nop,
            Self::Pop => Instruction::Pop,
            Self::Pop2 => Instruction::Pop2,
            Self::NewArray(a) => Instruction::NewArray(a),
            Self::MultiANewArray(a, b) => Instruction::MultiANewArray(a, b),
            Self::Convert(a, b) => Instruction::Convert(a, b),
            Self::ReferenceArray(a) => Instruction::ReferenceArray(a),
            Self::ArrayLoad(a) => Instruction::ArrayLoad(a),
            Self::ArrayStore(a) => Instruction::ArrayStore(a),
            Self::Load(a, b) => Instruction::Load(a, b),
            Self::Store(a, b) => Instruction::Store(a, b),
            Self::Return(a) => Instruction::Return(a),
            Self::ReturnVoid => Instruction::ReturnVoid,
            Self::Operate(a, b) => Instruction::Operate(a, b),
            Self::PushByte(a) => Instruction::PushByte(a),
            Self::Cast(a) => Instruction::Cast(a),
            Self::DoubleCmp(a) => Instruction::DoubleCmp(a),
            Self::FloatCmp(a) => Instruction::FloatCmp(a),
            Self::Push(a) => Instruction::Push(a),
            Self::Arraylength => Instruction::Arraylength,
            Self::GetField(a, b, c) => Instruction::GetField(a, b, c),
            Self::GetStatic(a, b, c) => Instruction::GetStatic(a, b, c),
            Self::Goto(a) => Instruction::Goto(func(a)),
            Self::Label(a) => Instruction::Label(func(a)),
            Self::IfACmp(a, b) => Instruction::IfACmp(a, func(b)),
            Self::IfIcmp(a, b) => Instruction::IfIcmp(a, func(b)),
            Self::LCmp => Instruction::LCmp,
            Self::IfZcmp(a, b) => Instruction::IfZcmp(a, func(b)),
            Self::IfNull(a) => Instruction::IfNull(func(a)),
            Self::IfNonNull(a) => Instruction::IfNonNull(func(a)),
            Self::Dup => Instruction::Dup,
            Self::IInc(a, b) => Instruction::IInc(a, b),
            Self::Instanceof(a) => Instruction::Instanceof(a),
            Self::InvokeDynamic(a) => Instruction::InvokeDynamic(a),
            Self::InvokeInterface(a, b, c, d) => Instruction::InvokeInterface(a, b, c, d),
            Self::InvokeSpecial(a, b, c) => Instruction::InvokeSpecial(a, b, c),
            Self::InvokeVirtual(a, b, c) => Instruction::InvokeVirtual(a, b, c),
            Self::InvokeStatic(a, b, c) => Instruction::InvokeStatic(a, b, c),
            Self::PutField(a, b, c) => Instruction::PutField(a, b, c),
            Self::PutStatic(a, b, c) => Instruction::PutStatic(a, b, c),
            Self::LoadConst(a) => Instruction::LoadConst(a),
            Self::LoadConst2(a) => Instruction::LoadConst2(a),
            Self::New(a) => Instruction::New(a),
            Self::Dup2 => Instruction::Dup2,
            Self::Throw => Instruction::Throw,
        }
    }
}

#[derive(Clone, Debug)]
pub enum VerificationType {
    Top,
    Int,
    Float,
    Long,
    Double,
    Null,
    UninitializedThis,
    /// Class info constant pointer
    Object(Arc<str>),
    /// Uninitialized variable; offset is the `new` instruction
    UninitializedVar(u16),
}

impl From<PrimitiveType> for VerificationType {
    fn from(value: PrimitiveType) -> Self {
        match value {
            PrimitiveType::Boolean
            | PrimitiveType::Byte
            | PrimitiveType::Short
            | PrimitiveType::Int
            | PrimitiveType::Char => Self::Int,
            PrimitiveType::Long => Self::Long,
            PrimitiveType::Float => Self::Float,
            PrimitiveType::Double => Self::Double,
            PrimitiveType::Reference => Self::Null,
        }
    }
}

impl From<FieldType> for VerificationType {
    fn from(value: FieldType) -> Self {
        match value {
            FieldType::Byte
            | FieldType::Char
            | FieldType::Int
            | FieldType::Boolean
            | FieldType::Short => Self::Int,
            FieldType::Double => Self::Double,
            FieldType::Float => Self::Float,
            FieldType::Long => Self::Long,
            FieldType::Object(o) => Self::Object(o),
            FieldType::Array(t) => Self::Object(format!("[{}", t.repr()).into()),
        }
    }
}
