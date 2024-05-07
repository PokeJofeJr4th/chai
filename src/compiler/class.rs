use std::{
    io::{Error, Write},
    sync::Arc,
};

use jvmrs_lib::{access, AccessFlags, ClassVersion, Constant, MethodDescriptor};

use super::instruction::Instruction;

#[derive(Debug)]
pub struct FieldInfo {}

#[derive(Debug)]
pub struct MethodInfo {
    pub name: Arc<str>,
    pub ty: MethodDescriptor,
    pub body: Vec<Instruction>,
    pub access: AccessFlags,
}

#[derive(Debug)]
pub enum AttributeInfo {}

#[derive(Debug)]
pub struct Class {
    access_flags: AccessFlags,
    constant_pool: Vec<Constant>,
    version: ClassVersion,
    this_class: Arc<str>,
    super_class: Arc<str>,
    interfaces: Vec<Arc<str>>,
    fields: Vec<FieldInfo>,
    methods: Vec<MethodInfo>,
    attributes: Vec<AttributeInfo>,
}

impl Class {
    pub fn new(this_class: Arc<str>, super_class: Arc<str>) -> Self {
        Self {
            access_flags: access!(public),
            constant_pool: Vec::new(),
            version: ClassVersion {
                minor_version: 65,
                major_version: 0,
            },
            this_class,
            super_class,
            interfaces: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            attributes: Vec::new(),
        }
    }

    pub fn register_method(&mut self, method: MethodInfo) {
        self.register_constant(Constant::String(method.name.clone()));
        self.register_constant(Constant::String(method.ty.repr()));
        self.methods.push(method);
    }

    #[allow(clippy::too_many_lines)]
    pub fn write(&mut self, writer: &mut impl Write) -> Result<(), Error> {
        let this_class = self.register_constant(Constant::ClassRef(self.this_class.clone())) as u16;
        let super_class =
            self.register_constant(Constant::ClassRef(self.super_class.clone())) as u16;
        let interfaces: Vec<u16> = self
            .interfaces
            .clone()
            .into_iter()
            .map(|int| {
                Self::insert_constant(&mut self.constant_pool, Constant::ClassRef(int)) as u16
            })
            .collect();
        let mut methods: Vec<(AccessFlags, u16, u16, Vec<()>)> = self
            .methods
            .iter()
            .map(|method| {
                (
                    method.access,
                    Self::insert_constant(
                        &mut self.constant_pool,
                        Constant::String(method.name.clone()),
                    ) as u16,
                    Self::insert_constant(
                        &mut self.constant_pool,
                        Constant::String(method.ty.repr()),
                    ) as u16,
                    Vec::new(),
                )
            })
            .collect();

        let x = &self;

        writer.write_all(&[0xCA, 0xFE, 0xBA, 0xBE])?;
        writer.write_all(&self.version.major_version.to_be_bytes())?;
        writer.write_all(&self.version.minor_version.to_be_bytes())?;

        writer.write_all(&(self.constant_pool.len() as u16 + 1).to_be_bytes())?;
        for constant in &self.constant_pool {
            self.write_constant(writer, constant)?;
        }

        writer.write_all(&self.access_flags.0.to_be_bytes())?;
        writer.write_all(&this_class.to_be_bytes())?;
        writer.write_all(&super_class.to_be_bytes())?;
        writer.write_all(&(self.interfaces.len() as u16).to_be_bytes())?;
        for interface in &interfaces {
            writer.write_all(&interface.to_be_bytes())?;
        }
        writer.write_all(&(self.fields.len() as u16).to_be_bytes())?;
        // TODO: write the fields
        writer.write_all(&(methods.len() as u16).to_be_bytes())?;

        for (access, name, ty, attrs) in methods {
            writer.write_all(&access.0.to_be_bytes())?;
            writer.write_all(&name.to_be_bytes())?;
            writer.write_all(&ty.to_be_bytes())?;
            writer.write_all(&(attrs.len() as u16).to_be_bytes())?;
            // TODO: write method attributes
        }

        writer.write_all(&(self.attributes.len() as u16).to_be_bytes())?;
        // TODO: write all the attributes
        // forbid self-modification for the writing stretch
        let _ = x;
        Ok(())
    }

    pub fn register_constant(&mut self, constant: Constant) -> usize {
        Self::insert_constant(&mut self.constant_pool, constant)
    }

    pub fn insert_constant(constant_pool: &mut Vec<Constant>, constant: Constant) -> usize {
        if let Some(idx) = constant_pool.iter().position(|c| c == &constant) {
            return idx + 1;
        }
        match &constant {
            Constant::ClassRef(class_name) => {
                Self::insert_constant(constant_pool, Constant::String(class_name.clone()));
            }
            Constant::StringRef(str) => {
                Self::insert_constant(constant_pool, Constant::String(str.clone()));
            }
            Constant::FieldRef {
                class,
                name,
                field_type,
            } => {
                Self::insert_constant(constant_pool, Constant::ClassRef(class.clone()));
                Self::insert_constant(
                    constant_pool,
                    Constant::NameTypeDescriptor {
                        name: name.clone(),
                        type_descriptor: field_type.repr(),
                    },
                );
            }
            Constant::NameTypeDescriptor {
                name,
                type_descriptor,
            } => {
                Self::insert_constant(constant_pool, Constant::String(name.clone()));
                Self::insert_constant(constant_pool, Constant::String(type_descriptor.clone()));
            }
            Constant::InterfaceRef {
                class,
                name,
                interface_type: method_type,
            }
            | Constant::MethodRef {
                class,
                name,
                method_type,
            } => {
                Self::insert_constant(constant_pool, Constant::ClassRef(class.clone()));
                Self::insert_constant(
                    constant_pool,
                    Constant::NameTypeDescriptor {
                        name: name.clone(),
                        type_descriptor: method_type.repr(),
                    },
                );
            }
            Constant::MethodType(ty) => {
                Self::insert_constant(constant_pool, Constant::String(ty.repr()));
            }
            _ => {}
        }
        constant_pool.push(constant);
        constant_pool.len()
    }

    fn get_constant(&self, constant: &Constant) -> Option<usize> {
        self.constant_pool
            .iter()
            .position(|x| x == constant)
            .map(|x| x + 1)
    }

    fn write_constant(&self, writer: &mut impl Write, constant: &Constant) -> Result<(), Error> {
        match constant {
            Constant::String(s) => {
                writer.write_all(&[1])?;
                writer.write_all(&(s.as_bytes().len() as u16).to_be_bytes())?;
                writer.write_all(s.as_bytes())?;
            }
            Constant::Int(i) => {
                writer.write_all(&[3])?;
                writer.write_all(&i.to_be_bytes())?;
            }
            Constant::Float(f) => {
                writer.write_all(&[4])?;
                writer.write_all(&f.to_bits().to_be_bytes())?;
            }
            Constant::Long(l) => {
                writer.write_all(&[5])?;
                writer.write_all(&l.to_be_bytes())?;
            }
            Constant::Double(d) => {
                writer.write_all(&[6])?;
                writer.write_all(&d.to_bits().to_be_bytes())?;
            }
            Constant::ClassRef(c) => {
                let c_idx = self.get_constant(&Constant::String(c.clone())).unwrap() as u16;
                writer.write_all(&[7])?;
                writer.write_all(&c_idx.to_be_bytes())?;
            }
            Constant::StringRef(s) => {
                let s_idx = self.get_constant(&Constant::String(s.clone())).unwrap() as u16;
                writer.write_all(&[8])?;
                writer.write_all(&s_idx.to_be_bytes())?;
            }
            Constant::FieldRef {
                class,
                name,
                field_type,
            } => todo!(),
            Constant::MethodRef {
                class,
                name,
                method_type,
            } => todo!(),
            Constant::InterfaceRef {
                class,
                name,
                interface_type,
            } => todo!(),
            Constant::NameTypeDescriptor {
                name,
                type_descriptor,
            } => todo!(),
            Constant::MethodHandle(_) => todo!(),
            Constant::MethodType(_) => todo!(),
            Constant::InvokeDynamic {
                bootstrap_index,
                method_name,
                method_type,
            } => todo!(),
            Constant::Placeholder => todo!(),
        }
        Ok(())
    }
}
