use std::{
    collections::HashMap,
    hash::Hash,
    sync::{Arc, Mutex},
};

use jvmrs_lib::AccessFlags;

use crate::types::IRFieldType;

use super::types::TypeHint;

#[derive(Clone, PartialEq, Debug)]
pub enum CtxItem {
    Function(Vec<Arc<FunctionInfo>>),
    Class(ClassInfo),
    Variable(usize, IRFieldType),
    Field(FieldInfo),
    Module(HashMap<Arc<str>, CtxItem>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ClassInfo {
    pub name: Arc<str>,
    pub superclass: Arc<str>,
    pub fields: Vec<FieldInfo>,
    pub methods: HashMap<Arc<str>, Vec<Arc<FunctionInfo>>>,
    pub inner_classes: Vec<ClassInfo>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct FunctionInfo {
    pub class: Arc<str>,
    pub access: AccessFlags,
    pub name: Arc<str>,
    pub params: Vec<IRFieldType>,
    pub generics: Vec<Arc<str>>,
    pub ret: IRFieldType,
}

impl FunctionInfo {
    pub fn apply_generics(&self, generics: &[IRFieldType]) -> Self {
        todo!()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct FieldInfo {
    pub class: Arc<str>,
    pub access: AccessFlags,
    pub name: Arc<str>,
    pub ty: TypeHint,
}

impl Hash for ClassInfo {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

pub type Context = Arc<Mutex<Ctx>>;

pub trait CtxExt {
    fn get(&self, k: &str) -> Option<CtxItem>;
    fn insert(&mut self, k: Arc<str>, v: CtxItem) -> Option<CtxItem>;
    fn new() -> Self;
    fn child(&self) -> Context;
}

#[derive(Debug)]
pub struct Ctx {
    map: HashMap<Arc<str>, CtxItem>,
}

impl CtxExt for Context {
    fn insert(&mut self, k: Arc<str>, v: CtxItem) -> Option<CtxItem> {
        self.lock().unwrap().map.insert(k, v)
    }

    #[must_use]
    fn get(&self, k: &str) -> Option<CtxItem> {
        self.lock().unwrap().map.get(k).cloned()
    }

    #[must_use]
    fn new() -> Self {
        Arc::new(Mutex::new(Ctx {
            map: HashMap::new(),
        }))
    }

    #[must_use]
    fn child(&self) -> Self {
        self.clone()
    }
}

impl Default for Ctx {
    fn default() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
}
