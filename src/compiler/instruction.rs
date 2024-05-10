use std::io::{Error, Write};

use crate::parser::syntax::BinaryOperator;

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instruction<Label = i16> {
    Nop,
    Pop,
    Pop2,
    NewArray(PrimitiveType),
    MultiANewArray(u16, u8),
    Convert(PrimitiveType, PrimitiveType),
    ReferenceArray(u16),
    ArrayLoad(PrimitiveType),
    ArrayStore(PrimitiveType),
    Load(PrimitiveType, u8),
    Store(PrimitiveType, u8),
    Return(PrimitiveType),
    ReturnVoid,
    Operate(PrimitiveType, Operator),
    PushByte(u16),
    Cast(u16),
    /// false -> g, true -> l
    DoubleCmp(bool),
    /// false -> g, true -> l
    FloatCmp(bool),
    Push(Const),
    Arraylength,
    GetField(u16),
    GetStatic(u16),
    Goto(Label),
    Label(Label),
    IfACmp(bool, Label),
    IfIcmp(ICmp, Label),
    LCmp,
    IfZcmp(ICmp, Label),
    IfNull,
    IfNonNull,
    Dup,
    IInc(u8, u8),
    Instanceof(u16),
    InvokeDynamic(u16),
    InvokeInterface(u16, u8),
    InvokeSpecial(u16),
    InvokeVirtual(u16),
    InvokeStatic(u16),
    PutField(u16),
    PutStatic(u16),
    LoadConst(u16),
    LoadConst2(u16),
    New(u16),
    Dup2,
    Throw,
}

impl Instruction {
    /// # Errors
    #[allow(clippy::too_many_lines)]
    pub fn write(&self, writer: &mut impl Write) -> Result<(), Error> {
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
            Self::LoadConst(i) => {
                if let Ok(i) = u8::try_from(*i) {
                    writer.write_all(&[0x12, i])
                } else {
                    writer.write_all(&[0x13])?;
                    writer.write_all(&i.to_be_bytes())
                }
            }
            Self::LoadConst2(i) => {
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
            Self::GetStatic(i) => {
                writer.write_all(&[0xB2])?;
                writer.write_all(&i.to_be_bytes())
            }
            Self::PutStatic(i) => {
                writer.write_all(&[0xB3])?;
                writer.write_all(&i.to_be_bytes())
            }
            Self::GetField(i) => {
                writer.write_all(&[0xB4])?;
                writer.write_all(&i.to_be_bytes())
            }
            Self::PutField(i) => {
                writer.write_all(&[0xB5])?;
                writer.write_all(&i.to_be_bytes())
            }
            Self::InvokeVirtual(i) => {
                writer.write_all(&[0xB6])?;
                writer.write_all(&i.to_be_bytes())
            }
            Self::InvokeSpecial(i) => {
                writer.write_all(&[0xB7])?;
                writer.write_all(&i.to_be_bytes())
            }
            Self::InvokeStatic(i) => {
                writer.write_all(&[0xB8])?;
                writer.write_all(&i.to_be_bytes())
            }
            Self::InvokeInterface(i, c) => {
                writer.write_all(&[0xB9])?;
                writer.write_all(&i.to_be_bytes())?;
                writer.write_all(&[*c, 0])
            }
            Self::InvokeDynamic(i) => {
                writer.write_all(&[0xBA])?;
                writer.write_all(&i.to_be_bytes())?;
                writer.write_all(&[0, 0])
            }
            Self::New(i) => {
                writer.write_all(&[0xBB])?;
                writer.write_all(&i.to_be_bytes())
            }
            Self::ReferenceArray(r) => {
                writer.write_all(&[0xBD])?;
                writer.write_all(&r.to_be_bytes())
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
                writer.write_all(&i.to_be_bytes())
            }
            Self::Instanceof(i) => {
                writer.write_all(&[0xC1])?;
                writer.write_all(&i.to_be_bytes())
            }
            Self::MultiANewArray(i, d) => {
                writer.write_all(&[0xC5])?;
                writer.write_all(&i.to_be_bytes())?;
                writer.write_all(&[*d])
            }
            Self::IfNull => writer.write_all(&[0xC6]),
            Self::IfNonNull => writer.write_all(&[0xC7]),
            _ => todo!(),
        }
    }
}

impl<T> Instruction<T> {
    pub fn size(&self) -> usize {
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
            Self::Label(_) => 0,
            Self::NewArray(_)
            | Self::ReferenceArray(_)
            | Self::Cast(_)
            | Self::IfNull
            | Self::IfNonNull
            | Self::GetField(_)
            | Self::GetStatic(_)
            | Self::Goto(_)
            | Self::IfACmp(_, _)
            | Self::IfIcmp(_, _)
            | Self::IfZcmp(_, _)
            | Self::IInc(_, _)
            | Self::Instanceof(_)
            | Self::InvokeSpecial(_)
            | Self::InvokeVirtual(_)
            | Self::InvokeStatic(_)
            | Self::PutField(_)
            | Self::PutStatic(_)
            | Self::LoadConst(_)
            | Self::LoadConst2(_)
            | Self::New(_) => 3,
            Self::InvokeDynamic(_) | Self::InvokeInterface(_, _) => 5,
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
            Self::GetField(a) => Instruction::GetField(a),
            Self::GetStatic(a) => Instruction::GetStatic(a),
            Self::Goto(a) => Instruction::Goto(func(a)),
            Self::Label(a) => Instruction::Label(func(a)),
            Self::IfACmp(a, b) => Instruction::IfACmp(a, func(b)),
            Self::IfIcmp(a, b) => Instruction::IfIcmp(a, func(b)),
            Self::LCmp => Instruction::LCmp,
            Self::IfZcmp(a, b) => Instruction::IfZcmp(a, func(b)),
            Self::IfNull => Instruction::IfNull,
            Self::IfNonNull => Instruction::IfNonNull,
            Self::Dup => Instruction::Dup,
            Self::IInc(a, b) => Instruction::IInc(a, b),
            Self::Instanceof(a) => Instruction::Instanceof(a),
            Self::InvokeDynamic(a) => Instruction::InvokeDynamic(a),
            Self::InvokeInterface(a, b) => Instruction::InvokeInterface(a, b),
            Self::InvokeSpecial(a) => Instruction::InvokeSpecial(a),
            Self::InvokeVirtual(a) => Instruction::InvokeVirtual(a),
            Self::InvokeStatic(a) => Instruction::InvokeStatic(a),
            Self::PutField(a) => Instruction::PutField(a),
            Self::PutStatic(a) => Instruction::PutStatic(a),
            Self::LoadConst(a) => Instruction::LoadConst(a),
            Self::LoadConst2(a) => Instruction::LoadConst2(a),
            Self::New(a) => Instruction::New(a),
            Self::Dup2 => Instruction::Dup2,
            Self::Throw => Instruction::Throw,
        }
    }
}
