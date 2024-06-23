use jvmrs_lib::access;
use std::{collections::HashMap, sync::Arc};

use super::context::{ClassInfo, CtxItem, FunctionInfo};
use crate::types::{IRFieldType, InnerFieldType};

macro_rules! header {
    (@mod $module:ident $class:ident mod $name:ident {$($toks:tt)*} $($rest:tt)*) => {
        let mut inner_module: HashMap<Arc<str>, CtxItem> = HashMap::new();
        header!{@mod inner_module $class $($toks)*}
        $module.insert(Arc::from(stringify!($name)), CtxItem::Module(inner_module));
        header!{@mod $module $class $($rest)*}
    };
    (@mod $module:ident $class:ident fn $name:ident ($($args:tt),*) $return:tt $($rest:tt)*) => {
        let CtxItem::Function(ref mut entry) = $module.entry(Arc::from(stringify!($name))).or_insert(CtxItem::Function(Vec::new())) else {
            panic!()
        };
        entry.push(Arc::new(FunctionInfo {
            class: stringify!($class).into(),
            access: access!(public static),
            name: stringify!($name).into(),
            params: vec![$(ty!($args)),*],
            ret: ty!($return),
        }));
        header!{@mod $module $class $($rest)*}
    };
    (@mod $module:ident $class:ident class $name:literal $(: $super:literal)? {$($body:tt)*} $($rest:tt)*) => {
        let mut super_class = "java/lang/Object";
        $(super_class = $super;)?
        let mut class_obj = ClassInfo {
            name: $name.into(),
            superclass: super_class.into(),
            fields: Vec::new(), methods: HashMap::new()
        };
        header!{@class class_obj $($body)*}
        let class_part = $name.split("/").last().unwrap();
        $module.insert(class_part.into(), CtxItem::Class(class_obj));
        header!{@mod $module $class $($rest)*}
    };
    (@mod $_mod:ident $_class:ident) => {};
    (@mod $_mod:ident $_class:ident $($toks:tt)*) => {
        todo!("Invalid Module Header Syntax: `{}`", stringify!(Invalid Module Header Syntax: $($toks)*))
    };
    (@class $class:ident) => {};
    (@class $class:ident $($toks:tt)*) => {
        todo!("Invalid Class Header Syntax: `{}`", stringify!(Invalid Module Header Syntax: $($toks)*))
    };
    ($($toks:tt)*) => {{
        let mut main_module: HashMap<Arc<str>, CtxItem> = HashMap::new();
        header!{
            @mod main_module __chai__ $($toks)*
        }
        main_module
    }};
}

macro_rules! ty {
    (void) => {
        IRFieldType::VOID
    };
    (int) => {
        InnerFieldType::Int.into()
    };
    ($class:literal) => {
        InnerFieldType::Object {
            base: $class.into(),
            generics: Vec::new(),
        }
        .into()
    };
}

#[must_use]
pub fn make_headers() -> HashMap<Arc<str>, CtxItem> {
    header! {
        mod chai {
            fn print("java/lang/String") void
            fn print(int) void
        }
        mod java {
            mod lang {
                class "java/lang/Integer" {

                }

                class "java/lang/System" {
                    static out "java/io/PrintStream"
                }
            }

            mod io {
                class "java/io/PrintStream" {
                    fn println("java/lang/String") void
                    fn println(int) void
                }
            }
        }
    }
}
