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
        return_type: Option<FieldType>
    },
}
