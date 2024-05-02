use std::{fmt::Debug, sync::Arc};

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
    Tuple(Vec<FieldType>),
}

impl Debug for InnerFieldType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean => write!(f, "boolean"),
            Self::Byte => write!(f, "byte"),
            Self::Short => write!(f, "short"),
            Self::Int => write!(f, "int"),
            Self::Long => write!(f, "long"),
            Self::Float => write!(f, "float"),
            Self::Double => write!(f, "double"),
            Self::Char => write!(f, "char"),
            Self::Object { base, generics } => {
                write!(f, "{base}")?;
                if generics.is_empty() {
                    return Ok(());
                }
                write!(f, "<{:?}", generics[0])?;
                for generic in generics.iter().skip(1) {
                    write!(f, ",{generic:?}")?;
                }
                write!(f, ">")
            }
            Self::Tuple(parts) => {
                let mut tup = f.debug_tuple("");
                for part in parts {
                    tup.field(part);
                }
                tup.finish()
            }
        }
    }
}

pub struct FieldType {
    pub ty: InnerFieldType,
    pub array_depth: usize,
}

impl Debug for FieldType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.ty)?;
        for _ in 0..self.array_depth {
            write!(f, "[]")?;
        }
        Ok(())
    }
}

impl From<InnerFieldType> for FieldType {
    fn from(value: InnerFieldType) -> Self {
        Self {
            ty: value,
            array_depth: 0,
        }
    }
}
