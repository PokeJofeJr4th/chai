use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use jvmrs_lib::MethodDescriptor;

use crate::types::FieldType;

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum CtxItem {
    Function(MethodDescriptor),
    Class,
    Variable(usize, FieldType),
    Field,
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
