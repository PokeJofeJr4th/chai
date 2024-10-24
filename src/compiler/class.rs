use std::{error::Error, io::Write, sync::Arc};

use jvmrs_lib::{access, AccessFlags, ClassVersion, Constant, MethodDescriptor, MethodHandle};

#[derive(Debug)]
pub struct FieldInfo {}

#[derive(Debug)]
pub struct MethodInfo {
    pub name: Arc<str>,
    pub ty: MethodDescriptor,
    pub access: AccessFlags,
    pub attributes: Vec<AttributeInfo>,
}

#[derive(Debug, Clone)]
pub struct AttributeInfo {
    pub name: Arc<str>,
    pub info: Vec<u8>,
}

impl AttributeInfo {
    #[allow(clippy::cast_possible_truncation)]
    pub fn write(&self, writer: &mut impl Write, class: &Class) -> Result<(), Box<dyn Error>> {
        writer.write_all(
            &(class
                .get_constant(&Constant::String(self.name.clone()))
                .unwrap() as u16)
                .to_be_bytes(),
        )?;
        writer.write_all(&(self.info.len() as u32).to_be_bytes())?;
        writer.write_all(&self.info)?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct BootstrapInfo {
    pub name: Arc<str>,
    pub ty: MethodDescriptor,
    pub bootstrap_arguments: Vec<Constant>,
    pub method_handle: MethodHandle,
}

#[derive(Debug)]
#[allow(clippy::struct_field_names)]
pub struct Class {
    access_flags: AccessFlags,
    constant_pool: Vec<Constant>,
    version: ClassVersion,
    this_class: Arc<str>,
    super_class: Arc<str>,
    interfaces: Vec<Arc<str>>,
    fields: Vec<FieldInfo>,
    methods: Vec<MethodInfo>,
    bootstrap_methods: Vec<BootstrapInfo>,
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
            bootstrap_methods: Vec::new(),
            attributes: Vec::new(),
        }
    }

    pub fn register_method(&mut self, method: MethodInfo) {
        self.register_constant(Constant::String(method.name.clone()));
        self.register_constant(Constant::String(method.ty.repr()));
        for attr in &method.attributes {
            self.register_constant(Constant::String(attr.name.clone()));
        }
        self.methods.push(method);
    }

    pub fn register_bootstrap(&mut self, bootstrap_method: BootstrapInfo) -> u16 {
        let invoke_d = self.register_constant(Constant::InvokeDynamic {
            bootstrap_index: u16::try_from(self.bootstrap_methods.len()).unwrap(),
            method_name: bootstrap_method.name.clone(),
            method_type: bootstrap_method.ty.clone(),
        });
        self.register_constant(Constant::MethodHandle(
            bootstrap_method.method_handle.clone(),
        ));
        for bootstrap_arg in &bootstrap_method.bootstrap_arguments {
            self.register_constant(bootstrap_arg.clone());
        }
        self.bootstrap_methods.push(bootstrap_method);
        invoke_d
    }

    #[allow(clippy::too_many_lines)]
    pub fn write(&mut self, writer: &mut impl Write) -> Result<(), Box<dyn Error>> {
        let this_class = self.register_constant(Constant::ClassRef(self.this_class.clone()));
        let super_class = self.register_constant(Constant::ClassRef(self.super_class.clone()));
        let interfaces: Vec<u16> = self
            .interfaces
            .clone()
            .into_iter()
            .map(|int| Self::insert_constant(&mut self.constant_pool, Constant::ClassRef(int)))
            .collect();
        let methods: Vec<(AccessFlags, u16, u16, Vec<AttributeInfo>)> = self
            .methods
            .iter()
            .map(|method| {
                (
                    method.access,
                    Self::insert_constant(
                        &mut self.constant_pool,
                        Constant::String(method.name.clone()),
                    ),
                    Self::insert_constant(
                        &mut self.constant_pool,
                        Constant::String(method.ty.repr()),
                    ),
                    method.attributes.clone(),
                )
            })
            .collect();
        if !self.bootstrap_methods.is_empty() {
            let mut info = Vec::new();
            info.extend(
                u16::try_from(self.bootstrap_methods.len())
                    .unwrap()
                    .to_be_bytes(),
            );
            for BootstrapInfo {
                name: _,
                ty: _,
                bootstrap_arguments,
                method_handle,
            } in &self.bootstrap_methods
            {
                info.extend(
                    u16::try_from(
                        self.get_constant(&Constant::MethodHandle(method_handle.clone()))
                            .unwrap(),
                    )
                    .unwrap()
                    .to_be_bytes(),
                );
                info.extend(
                    u16::try_from(bootstrap_arguments.len())
                        .unwrap()
                        .to_be_bytes(),
                );
                for arg in bootstrap_arguments {
                    info.extend(
                        u16::try_from(self.get_constant(arg).unwrap())
                            .unwrap()
                            .to_be_bytes(),
                    );
                }
            }
            self.attributes.push(AttributeInfo {
                name: "BootstrapMethods".into(),
                info,
            });
        }
        for attr in &self.attributes {
            Self::insert_constant(&mut self.constant_pool, Constant::String(attr.name.clone()));
        }

        let x = &self;

        writer.write_all(&[0xCA, 0xFE, 0xBA, 0xBE])?;
        writer.write_all(&self.version.major_version.to_be_bytes())?;
        writer.write_all(&self.version.minor_version.to_be_bytes())?;

        writer.write_all(&(u16::try_from(self.constant_pool.len() + 1)?).to_be_bytes())?;
        for constant in &self.constant_pool {
            self.write_constant(writer, constant)?;
        }

        writer.write_all(&self.access_flags.0.to_be_bytes())?;
        writer.write_all(&this_class.to_be_bytes())?;
        writer.write_all(&super_class.to_be_bytes())?;
        writer.write_all(&(u16::try_from(self.interfaces.len())?).to_be_bytes())?;
        for interface in &interfaces {
            writer.write_all(&interface.to_be_bytes())?;
        }
        writer.write_all(&(u16::try_from(self.fields.len())?).to_be_bytes())?;
        // TODO: write the fields
        writer.write_all(&(u16::try_from(methods.len())?).to_be_bytes())?;

        for (access, name, ty, attrs) in methods {
            writer.write_all(&access.0.to_be_bytes())?;
            writer.write_all(&name.to_be_bytes())?;
            writer.write_all(&ty.to_be_bytes())?;
            writer.write_all(&(u16::try_from(attrs.len())?).to_be_bytes())?;
            for attr in attrs {
                attr.write(writer, self)?;
            }
        }

        writer.write_all(&(u16::try_from(self.attributes.len())?).to_be_bytes())?;
        for attr in &self.attributes {
            attr.write(writer, self)?;
        }
        // forbid self-modification for the writing stretch
        let _ = x;
        Ok(())
    }

    pub fn register_constant(&mut self, constant: Constant) -> u16 {
        Self::insert_constant(&mut self.constant_pool, constant)
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn insert_constant(constant_pool: &mut Vec<Constant>, constant: Constant) -> u16 {
        if let Some(idx) = constant_pool.iter().position(|c| c == &constant) {
            return (idx + 1) as u16;
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
            Constant::InvokeDynamic {
                bootstrap_index: _,
                method_name,
                method_type,
            } => {
                Self::insert_constant(
                    constant_pool,
                    Constant::NameTypeDescriptor {
                        name: method_name.clone(),
                        type_descriptor: method_type.repr(),
                    },
                );
            }
            Constant::MethodHandle(MethodHandle::InvokeStatic {
                class,
                name,
                method_type,
            }) => {
                Self::insert_constant(
                    constant_pool,
                    Constant::MethodRef {
                        class: class.clone(),
                        name: name.clone(),
                        method_type: method_type.clone(),
                    },
                );
            }
            _ => {}
        }
        constant_pool.push(constant);
        constant_pool.len() as u16
    }

    fn get_constant(&self, constant: &Constant) -> Option<usize> {
        self.constant_pool
            .iter()
            .position(|x| x == constant)
            .map(|x| x + 1)
    }

    #[allow(clippy::too_many_lines)]
    fn write_constant(
        &self,
        writer: &mut impl Write,
        constant: &Constant,
    ) -> Result<(), Box<dyn Error>> {
        match constant {
            Constant::String(s) => {
                writer.write_all(&[1])?;
                writer.write_all(&(u16::try_from(s.as_bytes().len())?.to_be_bytes()))?;
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
                let c_idx =
                    u16::try_from(self.get_constant(&Constant::String(c.clone())).unwrap())?;
                writer.write_all(&[7])?;
                writer.write_all(&c_idx.to_be_bytes())?;
            }
            Constant::StringRef(s) => {
                let s_idx =
                    u16::try_from(self.get_constant(&Constant::String(s.clone())).unwrap())?;
                writer.write_all(&[8])?;
                writer.write_all(&s_idx.to_be_bytes())?;
            }
            Constant::FieldRef {
                class,
                name,
                field_type,
            } => {
                let class_idx = u16::try_from(
                    self.get_constant(&Constant::ClassRef(class.clone()))
                        .unwrap(),
                )?;
                let name_ty_idx = u16::try_from(
                    self.get_constant(&Constant::NameTypeDescriptor {
                        name: name.clone(),
                        type_descriptor: field_type.repr(),
                    })
                    .unwrap(),
                )?;
                writer.write_all(&[9])?;
                writer.write_all(&class_idx.to_be_bytes())?;
                writer.write_all(&name_ty_idx.to_be_bytes())?;
            }
            Constant::MethodRef {
                class,
                name,
                method_type,
            } => {
                let class_idx = u16::try_from(
                    self.get_constant(&Constant::ClassRef(class.clone()))
                    .ok_or_else(|| format!("Couldn't find class reference for Constant::MethodRef: {method_type:?} {class}.{name}"))?,
                )?;
                let name_ty_idx = u16::try_from(
                    self.get_constant(&Constant::NameTypeDescriptor {
                        name: name.clone(),
                        type_descriptor: method_type.repr(),
                    })
                    .ok_or_else(|| format!("Couldn't find name and type descriptor for Constant::MethodRef: {method_type:?} {class}.{name}"))?,
                )?;
                writer.write_all(&[10])?;
                writer.write_all(&class_idx.to_be_bytes())?;
                writer.write_all(&name_ty_idx.to_be_bytes())?;
            }
            Constant::InterfaceRef {
                class,
                name,
                interface_type,
            } => {
                let class_idx = u16::try_from(
                    self.get_constant(&Constant::ClassRef(class.clone()))
                    .ok_or_else(|| format!("Couldn't find class reference for Constant::InterfaceRef: {interface_type:?} {class}.{name}"))?,
                )?;
                let name_ty_idx = u16::try_from(
                    self.get_constant(&Constant::NameTypeDescriptor {
                        name: name.clone(),
                        type_descriptor: interface_type.repr(),
                    })
                    .ok_or_else(|| format!("Couldn't find name and type descriptor for Constant::InterfaceRef: {interface_type:?} {class}.{name}"))?,
                )?;
                writer.write_all(&[11])?;
                writer.write_all(&class_idx.to_be_bytes())?;
                writer.write_all(&name_ty_idx.to_be_bytes())?;
            }
            Constant::NameTypeDescriptor {
                name,
                type_descriptor,
            } => {
                let name_idx =
                    u16::try_from(self.get_constant(&Constant::String(name.clone())).ok_or_else(|| format!("Couldn't find name for Constant::NameTypeDescriptor: {type_descriptor} {name}"))?)?;
                let descriptor_idx = u16::try_from(
                    self.get_constant(&Constant::String(type_descriptor.clone()))
                        .ok_or_else(|| format!("Couldn't find type descriptor for Constant::NameTypeDescriptor: {type_descriptor} {name}"))?,
                )?;
                writer.write_all(&[12])?;
                writer.write_all(&name_idx.to_be_bytes())?;
                writer.write_all(&descriptor_idx.to_be_bytes())?;
            }
            Constant::MethodHandle(mh) => match mh {
                MethodHandle::InvokeStatic {
                    class,
                    name,
                    method_type,
                } => {
                    let method_idx = u16::try_from(
                        self.get_constant(&Constant::MethodRef {
                            class: class.clone(),
                            name: name.clone(),
                            method_type: method_type.clone(),
                        })
                        .ok_or_else(|| format!("Couldn't find method reference for Constant::MethodHandle::InvokeStatic: {method_type:?} {class}.{name}"))?,
                    )?;
                    writer.write_all(&[15, 6])?;
                    writer.write_all(&method_idx.to_be_bytes())?;
                }
                _ => todo!(),
            },
            Constant::MethodType(_) => todo!(),
            Constant::InvokeDynamic {
                bootstrap_index,
                method_name,
                method_type,
            } => {
                let name_ty_idx = u16::try_from(
                    self.get_constant(&Constant::NameTypeDescriptor {
                        name: method_name.clone(),
                        type_descriptor: method_type.repr(),
                    }).ok_or_else(|| format!("Couldn't find name and type descriptor for Constant::InvokeDynamic: {method_type:?} {method_name}"))?,
                )?;
                writer.write_all(&[18])?;
                writer.write_all(&bootstrap_index.to_be_bytes())?;
                writer.write_all(&name_ty_idx.to_be_bytes())?;
            }
            Constant::Placeholder => todo!(),
        }
        Ok(())
    }
}
