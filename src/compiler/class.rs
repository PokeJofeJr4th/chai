use std::{
    io::{Error, Write},
    sync::Arc,
};

use jvmrs_lib::{AccessFlags, ClassVersion, Constant, FieldType, MethodDescriptor};

pub struct FieldInfo {}

pub struct MethodInfo {}

pub enum AttributeInfo {}

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
    pub fn write(&mut self, writer: &mut impl Write) -> Result<(), Error> {
        let this_class = self.register_constant(Constant::ClassRef(self.this_class.clone())) as u16;
        let super_class =
            self.register_constant(Constant::ClassRef(self.super_class.clone())) as u16;
        let interfaces: Vec<u16> = self
            .interfaces
            .clone()
            .into_iter()
            .map(|int| self.register_constant(Constant::ClassRef(int)) as u16)
            .collect();

        writer.write_all(&[0xCA, 0xFE, 0xBA, 0xBE])?;
        writer.write_all(&self.version.major_version.to_be_bytes())?;
        writer.write_all(&self.version.minor_version.to_be_bytes())?;

        writer.write_all(&(self.constant_pool.len() as u16 + 1).to_be_bytes())?;
        // TODO: write the constants
        writer.write_all(&self.access_flags.0.to_be_bytes())?;
        writer.write_all(&this_class.to_be_bytes())?;
        writer.write_all(&super_class.to_be_bytes())?;
        writer.write_all(&(self.interfaces.len() as u16).to_be_bytes())?;
        for interface in &interfaces {
            writer.write_all(&interface.to_be_bytes())?;
        }
        writer.write_all(&(self.fields.len() as u16).to_be_bytes())?;
        // TODO: write the fields
        writer.write_all(&(self.methods.len() as u16).to_be_bytes())?;
        // TODO: write the methods
        writer.write_all(&(self.attributes.len() as u16).to_be_bytes())?;
        // TODO: write all the attributes
        Ok(())
    }

    pub fn register_constant(&mut self, constant: Constant) -> usize {
        if let Some(idx) = self.constant_pool.iter().position(|c| c == &constant) {
            return idx;
        }
        let idx = self.constant_pool.len();
        match &constant {
            Constant::ClassRef(class_name) => {
                self.register_constant(Constant::String(class_name.clone()));
            }
            Constant::StringRef(str) => {
                self.register_constant(Constant::String(str.clone()));
            }
            Constant::FieldRef {
                class,
                name,
                field_type,
            } => {
                self.register_constant(Constant::ClassRef(class.clone()));
                self.register_constant(Constant::NameTypeDescriptor {
                    name: name.clone(),
                    type_descriptor: field_type.repr(),
                });
            }
            Constant::NameTypeDescriptor {
                name,
                type_descriptor,
            } => {
                self.register_constant(Constant::String(name.clone()));
                self.register_constant(Constant::String(type_descriptor.clone()));
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
                self.register_constant(Constant::ClassRef(class.clone()));
                self.register_constant(Constant::NameTypeDescriptor {
                    name: name.clone(),
                    type_descriptor: method_type.repr(),
                });
            }
            Constant::MethodType(ty) => {
                self.register_constant(Constant::String(ty.repr()));
            }
            _ => {}
        }
        self.constant_pool.push(constant);
        idx
    }

    fn get_constant(&self, constant: &Constant) -> Option<usize> {
        self.constant_pool.iter().position(|x| x == constant)
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
            } => {
                
            },
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
