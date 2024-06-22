use jvmrs_lib::access;
use std::{collections::HashMap, sync::Arc};

use super::context::{CtxItem, FunctionInfo};
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
    // (@mod $module:ident $_:ident class $name:lit $(: $super:tt) {$($body:tt)*} $($next:tt)*) => {
    //     let mut class_info =
    // };
    (@mod $_:ident $class:ident) => {};
    (@mod $_:ident $class:ident $($toks:tt)*) => {
        todo!("Invalid Module Header Syntax: `{}`", stringify!(Invalid Module Header Syntax: $($toks)*))
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
    (($first:ident$(.$rest:ident)*)) => {
        {
            let mut class_buf = String::from(stringify!($first));
            $(
                class_buf.push('/');
                class_buf.push_str(stringify!($rest));
            )*
            InnerFieldType::Object {
                base: class_buf.into(),
                generics: Vec::new(),
            }.into()
        }
    };
}

#[must_use]
pub fn make_headers() -> HashMap<Arc<str>, CtxItem> {
    header! {
        mod chai {
            fn print((java.lang.String)) void
            fn print(int) void
        }
        mod java {
            mod lang {
                class Integer {

                }
            }
        }
    }
}
