use std::sync::Arc;

#[derive(Debug)]
pub enum FieldType {
    Boolean,
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Char,
    Object(Arc<str>),
    Array(Box<FieldType>),
}
