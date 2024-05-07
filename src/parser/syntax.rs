use std::{fmt::Debug, sync::Arc};

use crate::types::IRFieldType;

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
pub enum TopLevel {
    Import(ImportTree),
    Function {
        name: Arc<str>,
        params: Vec<(IRFieldType, Arc<str>)>,
        return_type: Option<IRFieldType>,
        body: Expression,
    },
}

#[derive(Debug)]
pub enum Expression {
    Ident(Arc<str>),
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
        ty: IRFieldType,
        var: Arc<str>,
        value: Box<Expression>,
    },
    /// represents `while` and `loop`
    Loop {
        body: Box<Expression>,
        condition: Option<Box<Expression>>,
    },
    For {
        ty: IRFieldType,
        var: Arc<str>,
        range: Box<Expression>,
        body: Box<Expression>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Not,
    Minus,
    Inc,
    Dec,
    Try,
}

#[derive(Debug, Clone, Copy)]
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
