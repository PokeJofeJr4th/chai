use crate::{
    parser::syntax::BinaryOperator,
    types::{FieldType, InnerFieldType},
};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum TypeHint {
    Any,
    Void,
    Integral,
    Floating,
    Concrete(FieldType),
    Tuple(Vec<TypeHint>),
}

impl TypeHint {
    #[must_use]
    pub fn is_subtype(&self, super_ty: &Self) -> bool {
        if super_ty == &Self::Any || self == super_ty {
            return true;
        }
        if let Self::Concrete(FieldType {
            ty: InnerFieldType::Object { base, generics },
            array_depth: 0,
        }) = super_ty
        {
            if &**base == "java/lang/Object" {
                return true;
            }
        }
        if self == &Self::Any {
            return false;
        }
        if super_ty == &Self::Integral {
            return self.is_integral();
        }
        if self == &Self::Integral {
            return super_ty.is_integral();
        }
        if super_ty == &Self::Floating {
            return self.is_floating();
        }
        if self == &Self::Floating {
            return super_ty.is_floating();
        }
        if let (Self::Tuple(this), Self::Tuple(other)) = (self, super_ty) {
            return this.len() == other.len()
                && this
                    .iter()
                    .zip(other)
                    .all(|(this, other)| this.is_subtype(other));
        }
        false
    }

    #[must_use]
    pub fn is_integral(&self) -> bool {
        matches!(
            self,
            Self::Integral
                | Self::Concrete(FieldType {
                    ty: InnerFieldType::Byte
                        | InnerFieldType::Short
                        | InnerFieldType::Int
                        | InnerFieldType::Long,
                    array_depth: 0
                })
        ) || {
            if let Self::Concrete(FieldType {
                ty: InnerFieldType::Object { base, generics: _ },
                array_depth: 0,
            }) = self
            {
                &**base == "java/lang/Integer"
                    || &**base == "java/lang/Byte"
                    || &**base == "java/lang/Short"
                    || &**base == "java/lang/Long"
            } else {
                false
            }
        }
    }

    #[must_use]
    pub fn is_floating(&self) -> bool {
        matches!(
            self,
            Self::Floating
                | Self::Concrete(FieldType {
                    ty: InnerFieldType::Float | InnerFieldType::Double,
                    array_depth: 0
                })
        ) || {
            if let Self::Concrete(FieldType {
                ty: InnerFieldType::Object { base, generics: _ },
                array_depth: 0,
            }) = self
            {
                &**base == "java/lang/Float" || &**base == "java/lang/Double"
            } else {
                false
            }
        }
    }

    /// # Errors
    pub fn intersect(&self, other: &Self) -> Result<Self, String> {
        if self == other {
            return Ok(self.clone());
        }
        if self == &Self::Integral && other.is_integral() {
            return Ok(other.clone());
        }
        if other == &Self::Integral && self.is_integral() {
            return Ok(self.clone());
        }
        if self == &Self::Floating && other.is_floating() {
            return Ok(other.clone());
        }
        if other == &Self::Floating && self.is_floating() {
            return Ok(self.clone());
        }
        Err(format!(
            "Types `{self:?}` and `{other:?}` are not compatible"
        ))
    }

    #[must_use]
    pub fn is_string(&self) -> bool {
        let Self::Concrete(FieldType {
            ty: InnerFieldType::Object { base, generics },
            array_depth: 0,
        }) = self
        else {
            return false;
        };
        generics.is_empty() && &**base == "java/lang/String"
    }
}

/// # Errors
pub fn operate_types(
    lhs: &TypeHint,
    op: BinaryOperator,
    rhs: &TypeHint,
) -> Result<TypeHint, String> {
    match (lhs, op, rhs) {
        (
            lhs,
            BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::Mul
            | BinaryOperator::Div
            | BinaryOperator::Mod,
            rhs,
        ) if (lhs.is_subtype(rhs) || rhs.is_subtype(lhs))
            && (lhs.is_floating() || lhs.is_integral()) =>
        {
            Ok(lhs.intersect(rhs)?)
        }
        (lhs, BinaryOperator::Eq, rhs) if lhs.is_subtype(rhs) || rhs.is_subtype(lhs) => {
            Ok(TypeHint::Concrete(InnerFieldType::Boolean.into()))
        }
        (
            TypeHint::Concrete(FieldType {
                ty,
                array_depth: array_depth @ 1..,
            }),
            BinaryOperator::Index,
            rhs,
        ) if rhs.is_integral() => Ok(TypeHint::Concrete(FieldType {
            ty: ty.clone(),
            array_depth: *array_depth - 1,
        })),
        (lhs, BinaryOperator::Add, rhs) if lhs.is_string() || rhs.is_string() => {
            Ok(TypeHint::Concrete(
                InnerFieldType::Object {
                    base: "java/lang/String".into(),
                    generics: Vec::new(),
                }
                .into(),
            ))
        }
        (lhs, op, rhs) => Err(format!("Can't make operation `{lhs:?} {op:?} {rhs:?}`")),
    }
}
