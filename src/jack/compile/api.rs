use std::{
    collections::HashMap,
    convert::{TryFrom, TryInto},
};

use crate::{
    jack::subroutine::{ReturnType, SubroutineBody, SubroutineDec, SubroutineKind},
    vm::{Segment, VMLine},
};

use super::{
    super::{
        class::{Class, ClassVarKind},
        id::Id,
        keyword::Keyword,
        typea::Type,
    },
    CompileError,
};

pub struct SubApi {
    name: String,
    kind: SubroutineKind,
}

impl SubApi {
    pub fn new(sub_dec: &SubroutineDec) -> Self {
        Self {
            name: sub_dec.name().as_str().to_owned(),
            kind: sub_dec.kind().clone(),
        }
    }
}

pub struct ClassApi {
    name: String,
    subs: HashMap<String, SubApi>,
}

impl ClassApi {
    pub fn new(class: &Class) -> Self {
        let subs: HashMap<_, _> = class
            .subs()
            .iter()
            .map(|sub_dec| (sub_dec.name().as_str().to_owned(), SubApi::new(sub_dec)))
            .collect();
        Self {
            name: class.name().to_owned(),
            subs,
        }
    }
}

pub struct Api {
    classes: HashMap<String, ClassApi>,
}

impl Api {
    pub fn new(class_path: &[Class]) -> Self {
        let classes: HashMap<_, _> = class_path
            .iter()
            .map(|class| (class.name().to_owned(), ClassApi::new(class)))
            .collect();
        Self { classes }
    }

    pub fn resolve_sub_api(&self, class_name: &Id, sub_name: &Id) -> Result<&SubApi, CompileError> {
        if let Some(sub_api) = self
            .classes
            .get(class_name.as_str())
            .and_then(|class| class.subs.get(sub_name.as_str()))
        {
            Ok(sub_api)
        } else {
            Err(format!(
                "subroutine {} not found in class {}",
                sub_name.as_str(),
                class_name.as_str()
            ))
        }
    }

    pub fn resolve_sub_kind(
        &self,
        class_name: &Id,
        sub_name: &Id,
    ) -> Result<SubroutineKind, CompileError> {
        self.resolve_sub_api(class_name, sub_name)
            .map(|sub| sub.kind.clone())
    }

    pub fn has_class(&self, class_name: &Id) -> bool {
        self.classes.contains_key(class_name.as_str())
    }

    pub fn os_classes() -> Vec<Class> {
        vec![
            Class::new(
                Id::from("Math"),
                Vec::new(),
                vec![
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("abs"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("multiply"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("divide"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("min"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("max"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("sqrt"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                ],
            ),
            Class::new(
                Id::from("String"),
                Vec::new(),
                vec![
                    SubroutineDec::new(
                        SubroutineKind::Constructor,
                        ReturnType::Void,
                        Id::from("new"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Method,
                        ReturnType::Void,
                        Id::from("dispose"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Method,
                        ReturnType::Void,
                        Id::from("length"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Method,
                        ReturnType::Void,
                        Id::from("charAt"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Method,
                        ReturnType::Void,
                        Id::from("setCharAt"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Method,
                        ReturnType::Void,
                        Id::from("appendChar"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Method,
                        ReturnType::Void,
                        Id::from("eraseLastChar"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Method,
                        ReturnType::Void,
                        Id::from("intValue"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Method,
                        ReturnType::Void,
                        Id::from("setInt"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("backSpace"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("doubleQuote"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("newLine"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                ],
            ),
            Class::new(
                Id::from("Array"),
                Vec::new(),
                vec![
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("new"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Method,
                        ReturnType::Void,
                        Id::from("dispose"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                ],
            ),
            Class::new(
                Id::from("Output"),
                Vec::new(),
                vec![
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("moveCursor"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("printChar"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("printString"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("printInt"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("println"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("backspace"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                ],
            ),
            Class::new(
                Id::from("Screen"),
                Vec::new(),
                vec![
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("clearScreen"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("setColor"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("drawPixel"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("drawLine"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("drawRectangle"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("drawCircle"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                ],
            ),
            Class::new(
                Id::from("Keyboard"),
                Vec::new(),
                vec![
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("keyPressed"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("readChar"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("readLine"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("readInt"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                ],
            ),
            Class::new(
                Id::from("Memory"),
                Vec::new(),
                vec![
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("peek"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("poke"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("alloc"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("deAlloc"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                ],
            ),
            Class::new(
                Id::from("Sys"),
                Vec::new(),
                vec![
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("halt"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("error"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                    SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("wait"),
                        Vec::new().into(),
                        SubroutineBody::new(Vec::new(), vec![].into()),
                    ),
                ],
            ),
        ]
    }
}
