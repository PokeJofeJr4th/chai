use std::sync::Arc;

use crate::types::FieldType;

#[derive(Debug)]
pub struct ImportTree {
    pub current: Arc<str>,
    pub children: Vec<ImportTree>,
}

#[derive(Debug)]
pub enum TopLevel {
    Import(ImportTree),
    Function {
        name: Arc<str>,
        params: Vec<(FieldType, Arc<str>)>,
        return_type: Option<FieldType>,
    },
}

#[derive(Debug)]
pub enum Expression {
    Ident(Arc<str>),
    Int(i64),
    Float(f64),
    String(Arc<str>),
    Block(Vec<Expression>),
    UnaryOperation(UnaryOperator, Box<Expression>),
    BinaryOperation(Box<Expression>, BinaryOperator, Box<Expression>),
    TernaryOperation {
        condition: Box<Expression>,
        if_true: Box<Expression>,
        if_false: Box<Expression>,
    },
    FunctionCall {
        function: Box<Expression>,
        args: Vec<Expression>,
    },
    If {
        condition: Box<Expression>,
        body: Box<Expression>,
        else_body: Option<Box<Expression>>,
    },
    /// represents while, for, and loop
    Loop {
        init: Option<Box<Expression>>,
        body: Box<Expression>,
        step: Option<Box<Expression>>,
        condition: Option<Box<Expression>>,
    },
}

#[derive(Debug)]
pub enum UnaryOperator {
    Not,
    Minus,
}

#[derive(Debug)]
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
    Set,
}
