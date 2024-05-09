use std::{fmt::Debug, sync::Arc};

use jvmrs_lib::FieldType;

#[derive(Clone, PartialEq, Hash, Eq)]
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
        generics: Vec<IRFieldType>,
    },
    Tuple(Vec<IRFieldType>),
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

#[derive(Clone, PartialEq, Hash, Eq)]
pub struct IRFieldType {
    pub ty: InnerFieldType,
    pub array_depth: usize,
}

impl IRFieldType {
    pub const VOID: Self = Self {
        ty: InnerFieldType::Tuple(Vec::new()),
        array_depth: 0,
    };

    pub fn string() -> Self {
        Self {
            ty: InnerFieldType::Object {
                base: "java/lang/String".into(),
                generics: Vec::new(),
            },
            array_depth: 0,
        }
    }

    #[must_use]
    pub fn to_field_type(&self) -> FieldType {
        let mut ty = match &self.ty {
            InnerFieldType::Boolean => FieldType::Boolean,
            InnerFieldType::Byte => FieldType::Byte,
            InnerFieldType::Short => FieldType::Short,
            InnerFieldType::Int => FieldType::Int,
            InnerFieldType::Long => FieldType::Long,
            InnerFieldType::Float => FieldType::Float,
            InnerFieldType::Double => FieldType::Double,
            InnerFieldType::Char => FieldType::Char,
            InnerFieldType::Object { base, generics: _ } => FieldType::Object(base.clone()),
            InnerFieldType::Tuple(_) => {
                FieldType::Array(Box::new(FieldType::Object("java/lang/Object".into())))
            }
        };
        for _ in 0..self.array_depth {
            ty = FieldType::Array(Box::new(ty));
        }
        ty
    }

    pub fn is_void(&self) -> bool {
        self.array_depth == 0
            && match &self.ty {
                InnerFieldType::Tuple(e) if e.is_empty() => true,
                _ => false,
            }
    }
}

impl Debug for IRFieldType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.ty)?;
        for _ in 0..self.array_depth {
            write!(f, "[]")?;
        }
        Ok(())
    }
}

impl From<InnerFieldType> for IRFieldType {
    fn from(value: InnerFieldType) -> Self {
        Self {
            ty: value,
            array_depth: 0,
        }
    }
}
