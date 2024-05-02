use std::sync::Arc;

#[derive(Debug)]
pub enum InnerFieldType {
    Boolean,
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Char,
    Object {
        base: Arc<str>,
        generics: Vec<FieldType>,
    },
}

#[derive(Debug)]
pub struct FieldType {
    pub ty: InnerFieldType,
    pub array_depth: usize,
}

impl From<InnerFieldType> for FieldType {
    fn from(value: InnerFieldType) -> Self {
        Self {
            ty: value,
            array_depth: 0,
        }
    }
}
