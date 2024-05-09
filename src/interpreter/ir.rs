use std::{
    fmt::Debug,
    hash::Hash,
    sync::{Arc, Mutex},
};

use crate::{
    parser::syntax::{BinaryOperator, UnaryOperator},
    types::IRFieldType,
};

use super::context::FunctionInfo;

#[derive(Clone, Eq)]
pub struct Symbol(usize, Arc<str>);

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}#{}", self.0, self.1)
    }
}

impl Symbol {
    /// # Panics
    pub fn new(name: Arc<str>) -> Self {
        static ID: Mutex<usize> = Mutex::new(0);

        let mut id_borrow = ID.lock().unwrap();
        let id = *id_borrow;
        *id_borrow += 1;
        drop(id_borrow);

        Self(id, name)
    }

    #[must_use]
    pub const fn id(&self) -> usize {
        self.0
    }
}

#[derive(Debug)]
pub struct IRFunction {
    pub name: Arc<str>,
    pub params: Vec<IRFieldType>,
    pub locals: Vec<IRFieldType>,
    pub ret: Option<IRFieldType>,
    pub body: IRExpression,
}

#[derive(Debug)]
pub enum IRStatement {
    Push(IRExpression),
    Pop,
    Invoke(Arc<FunctionInfo>),
    Branch(IRExpression, Symbol),
    Jump(Symbol),
    Label(Symbol),
    Move(IRExpression, IRExpression),
    Return,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IRExpression {
    Stack,
    Void,
    LocalVar(usize),
    String(Arc<str>),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    MakeTuple(Vec<IRExpression>),
    BinaryOperation(Box<IRExpression>, BinaryOperator, Box<IRExpression>),
    UnaryOperation(UnaryOperator, Box<IRExpression>),
    Block(Vec<IRExpression>, Option<Box<IRExpression>>),
    If(Box<IRExpression>, Box<IRExpression>, Box<IRExpression>),
    For {
        init: Box<IRExpression>,
        inc: Box<IRExpression>,
        body: Box<IRExpression>,
        condition: Box<IRExpression>,
    },
    SetLocal(usize, Box<IRExpression>),
    Invoke(Arc<FunctionInfo>, Vec<IRExpression>),
}
