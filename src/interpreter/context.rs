use std::{collections::HashMap, hash::Hash, sync::Arc};

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
    pub ret: IRFieldType,
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

#[derive(Debug)]
pub struct Context {
    map: HashMap<Arc<str>, CtxItem>,
}

impl Context {
    pub fn insert(&mut self, k: Arc<str>, v: CtxItem) -> Option<CtxItem> {
        self.map.insert(k, v)
    }

    #[must_use]
    pub fn get(&self, k: &str) -> Option<&CtxItem> {
        self.map.get(k)
    }

    #[must_use]
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    #[must_use]
    pub fn child(&self) -> Self {
        Self {
            map: self.map.clone(),
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}
