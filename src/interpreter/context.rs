use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use jvmrs_lib::MethodDescriptor;

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum CtxItem {
    Function(MethodDescriptor),
    Class,
    Variable(usize),
    Field,
}

pub struct Context {
    map: HashMap<Arc<str>, HashSet<CtxItem>>,
}

impl Context {
    pub fn insert(&mut self, k: Arc<str>, v: CtxItem) -> bool {
        self.map.entry(k).or_default().insert(v)
    }

    #[must_use]
    pub fn get(&self, k: &str) -> Option<&HashSet<CtxItem>> {
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
