use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    sync::Arc,
};

use jvmrs_lib::MethodDescriptor;

use crate::types::FieldType;

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum CtxItem {
    Function(MethodDescriptor),
    Class(ClassInfo),
    Variable(usize, FieldType),
    Field,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ClassInfo {
    pub name: Arc<str>,
    pub superclass: Arc<str>,
    pub fields: Vec<(Arc<str>, FieldType)>,
    pub methods: HashMap<Arc<str>, Vec<MethodDescriptor>>,
}

impl Hash for ClassInfo {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

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
