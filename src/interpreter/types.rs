use crate::types::FieldType;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum TypeHint {
    Any,
    Void,
    Integral,
    Floating,
    Concrete(FieldType),
    Tuple(Vec<TypeHint>)
}
