use std::{
    fmt::Debug,
    hash::Hash,
    sync::{Arc, Mutex},
};

use crate::{
    parser::syntax::{BinaryOperator, UnaryOperator},
    types::FieldType,
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
    pub params: Vec<(FieldType, Arc<str>)>,
    pub ret: Option<FieldType>,
    pub body: Vec<IRStatement>,
}

#[derive(Debug)]
pub enum IRStatement {
    Push(IRLocation),
    Pop,
    Invoke(Arc<FunctionInfo>),
    Branch(IRLocation, Symbol),
    Jump(Symbol),
    Label(Symbol),
    Move(IRLocation, IRLocation),
    BinaryOperation(IRLocation, BinaryOperator, IRLocation),
    UnaryOperation(UnaryOperator, IRLocation),
    MakeTuple(usize),
    Return,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IRLocation {
    Stack,
    Void,
    LocalVar(usize),
    String(Arc<str>),
    Int(i64),
    Float(f64),
}
