use std::{
    fmt::{Debug, Display},
    sync::Arc,
};

use jvmrs_lib::AccessFlags;

pub struct ImportTree {
    pub current: Arc<str>,
    pub children: Vec<ImportTree>,
}

impl Debug for ImportTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.current)?;
        match &self.children[..] {
            [] => Ok(()),
            [child] => write!(f, ".{child:?}"),
            children => {
                write!(f, ".{{{:?}", children[0])?;
                for child in children.iter().skip(1) {
                    write!(f, ", {child:?}")?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug)]
pub struct TypeExpr {
    pub ty: InnerTypeExpr,
    pub array_depth: usize,
}

#[derive(Debug)]
pub enum InnerTypeExpr {
    Ident(Arc<str>, Vec<TypeExpr>),
    Tuple(Vec<TypeExpr>),
    Boolean,
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
    Character,
}

pub type GenericBound = Arc<str>;

#[derive(Debug)]
pub enum TopLevel {
    Import(ImportTree),
    Function {
        name: Arc<str>,
        generics: Vec<GenericBound>,
        access: AccessFlags,
        params: Vec<(TypeExpr, Arc<str>)>,
        return_type: Option<TypeExpr>,
        body: Expression,
    },
    Class {
        class_name: Arc<str>,
        generics: Vec<GenericBound>,
        body: Vec<TopLevel>,
    },
}

#[derive(Debug)]
pub enum Expression {
    Ident(Arc<str>, Vec<TypeExpr>),
    Int(i64),
    Float(f64),
    String(Arc<str>),
    Block {
        statements: Vec<Expression>,
        ret: Option<Box<Expression>>,
    },
    Tuple(Vec<Expression>),
    UnaryOperation(UnaryOperator, Box<Expression>),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
    FunctionCall {
        function: Box<Expression>,
        args: Vec<Expression>,
    },
    If {
        condition: Box<Expression>,
        body: Box<Expression>,
        else_body: Option<Box<Expression>>,
    },
    Let {
        ty: TypeExpr,
        var: Arc<str>,
        value: Box<Expression>,
    },
    /// represents `while` and `loop`
    Loop {
        body: Box<Expression>,
        condition: Option<Box<Expression>>,
    },
    For {
        ty: TypeExpr,
        var: Arc<str>,
        range: Box<Expression>,
        body: Box<Expression>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Not,
    Minus,
    Try,
    /// Convert a primitive type to an Object type (e.g. int -> Integer)
    Box,
    /// Convert an Object type to a primitive type (e.g. Integer -> int)
    Unbox,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Dot,
    Add,
    AddEq,
    Sub,
    SubEq,
    Mul,
    MulEq,
    Div,
    DivEq,
    Mod,
    ModEq,
    And,
    BitAnd,
    BitAndEq,
    Or,
    BitOr,
    BitOrEq,
    Xor,
    XorEq,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    Set,
    Index,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Dot => ".",
                Self::Add => "+",
                Self::AddEq => "+=",
                Self::Sub => "-",
                Self::SubEq => "-=",
                Self::Mul => "*",
                Self::MulEq => "*=",
                Self::Div => "/",
                Self::DivEq => "/=",
                Self::Mod => "%",
                Self::ModEq => "%=",
                Self::And => "&&",
                Self::BitAnd => "&",
                Self::BitAndEq => "&=",
                Self::Or => "||",
                Self::BitOr => "|",
                Self::BitOrEq => "|=",
                Self::Xor => "^",
                Self::XorEq => "^=",
                Self::Lt => "<",
                Self::Le => "<=",
                Self::Gt => ">",
                Self::Ge => ">=",
                Self::Eq => "==",
                Self::Ne => "!=",
                Self::Set => "=",
                Self::Index => "[",
            }
        )
    }
}
