use std::{fmt::Debug, sync::Arc};

use jvmrs_lib::FieldType;

use crate::{
    compiler::instruction::PrimitiveType, interpreter::types::TypeHint,
    parser::syntax::GenericBound,
};

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
    GenericPlaceholder {
        generic: GenericBound,
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
            Self::GenericPlaceholder { generic } => write!(f, "{generic}"),
        }
    }
}

impl InnerFieldType {
    #[must_use]
    pub fn class_name(&self) -> &str {
        match self {
            Self::Boolean => "java/lang/Boolean",
            Self::Byte => "java/lang/Byte",
            Self::Char => "java/lang/Character",
            Self::Double => "java/lang/Double",
            Self::Float => "java/lang/Float",
            Self::Int => "java/lang/Integer",
            Self::Long => "java/lang/Long",
            Self::Short => "java/lang/Short",
            Self::Object { base, generics: _ } => base,
            Self::Tuple(_) => "java/lang/Object",
            Self::GenericPlaceholder { generic } => generic,
        }
    }

    #[must_use]
    /// # Panics
    pub fn to_field_type(&self) -> FieldType {
        match self {
            Self::Boolean => FieldType::Boolean,
            Self::Byte => FieldType::Byte,
            Self::Char => FieldType::Char,
            Self::Double => FieldType::Double,
            Self::Float => FieldType::Float,
            Self::Int => FieldType::Int,
            Self::Long => FieldType::Long,
            Self::Short => FieldType::Short,
            Self::Object { base, generics: _ } => FieldType::Object(base.clone()),
            Self::Tuple(t) if t.is_empty() => {
                FieldType::Array(Box::new(FieldType::Object(Arc::from("java/lang/Object"))))
            }
            Self::Tuple(tys) => {
                let mut ts = tys[0].clone();
                for t in tys.iter().skip(1) {
                    let TypeHint::Concrete(t) =
                        TypeHint::Concrete(t.clone()).intersect(&TypeHint::Concrete(ts))
                    else {
                        panic!()
                    };
                    ts = t;
                }
                let mut field_type = ts.to_field_type();
                for _ in 0..=ts.array_depth {
                    field_type = FieldType::Array(Box::new(field_type));
                }
                field_type
            }
            Self::GenericPlaceholder { generic } => FieldType::Object("java/lang/Object".into()),
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

    #[must_use]
    pub fn object() -> Self {
        Self {
            ty: InnerFieldType::Object {
                base: "java/lang/Object".into(),
                generics: Vec::new(),
            },
            array_depth: 0,
        }
    }

    #[must_use]
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
        let mut ty = self.ty.to_field_type();
        for _ in 0..self.array_depth {
            ty = FieldType::Array(Box::new(ty));
        }
        ty
    }

    #[must_use]
    pub const fn to_primitive(&self) -> PrimitiveType {
        if self.array_depth > 0 {
            PrimitiveType::Reference
        } else {
            match &self.ty {
                InnerFieldType::Boolean => PrimitiveType::Boolean,
                InnerFieldType::Byte => PrimitiveType::Byte,
                InnerFieldType::Short => PrimitiveType::Short,
                InnerFieldType::Int => PrimitiveType::Int,
                InnerFieldType::Long => PrimitiveType::Long,
                InnerFieldType::Float => PrimitiveType::Float,
                InnerFieldType::Double => PrimitiveType::Double,
                InnerFieldType::Char => PrimitiveType::Char,
                InnerFieldType::Object { .. }
                | InnerFieldType::Tuple(_)
                | InnerFieldType::GenericPlaceholder { .. } => PrimitiveType::Reference,
            }
        }
    }

    #[must_use]
    pub const fn is_primitive(&self) -> bool {
        self.array_depth == 0
            && matches!(
                self.ty,
                InnerFieldType::Boolean
                    | InnerFieldType::Byte
                    | InnerFieldType::Char
                    | InnerFieldType::Double
                    | InnerFieldType::Float
                    | InnerFieldType::Int
                    | InnerFieldType::Long
                    | InnerFieldType::Short
            )
    }

    #[must_use]
    pub fn is_void(&self) -> bool {
        self.array_depth == 0 && matches!(&self.ty, InnerFieldType::Tuple(e) if e.is_empty())
    }

    #[must_use]
    pub const fn get_size(&self) -> usize {
        if self.array_depth == 0 {
            match self.ty {
                InnerFieldType::Double | InnerFieldType::Long => 2,
                _ => 1,
            }
        } else {
            1
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
