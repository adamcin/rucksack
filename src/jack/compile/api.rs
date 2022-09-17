use std::collections::HashMap;

use crate::jack::subroutine::{SubroutineDec, SubroutineKind};

use super::{
    super::{class::Class, id::Id},
    CompileError,
};

pub struct SubApi {
    kind: SubroutineKind,
}

impl SubApi {
    pub fn new(sub_dec: &SubroutineDec) -> Self {
        Self {
            kind: sub_dec.kind().clone(),
        }
    }
}

pub struct ClassApi {
    subs: HashMap<String, SubApi>,
}

impl ClassApi {
    pub fn new(class: &Class) -> Self {
        let subs: HashMap<_, _> = class
            .subs()
            .iter()
            .map(|sub_dec| (sub_dec.name().as_str().to_owned(), SubApi::new(sub_dec)))
            .collect();
        Self { subs }
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
            Class::api(
                "Math",
                vec![
                    (SubroutineKind::Function, "abs").into(),
                    (SubroutineKind::Function, "multiply").into(),
                    (SubroutineKind::Function, "divide").into(),
                    (SubroutineKind::Function, "min").into(),
                    (SubroutineKind::Function, "max").into(),
                    (SubroutineKind::Function, "sqrt").into(),
                ],
            ),
            Class::api(
                "String",
                vec![
                    (SubroutineKind::Constructor, "new").into(),
                    (SubroutineKind::Method, "dispose").into(),
                    (SubroutineKind::Method, "length").into(),
                    (SubroutineKind::Method, "charAt").into(),
                    (SubroutineKind::Method, "setCharAt").into(),
                    (SubroutineKind::Method, "appendChar").into(),
                    (SubroutineKind::Method, "eraseLastChar").into(),
                    (SubroutineKind::Method, "intValue").into(),
                    (SubroutineKind::Method, "setInt").into(),
                    (SubroutineKind::Function, "backSpace").into(),
                    (SubroutineKind::Function, "doubleQuote").into(),
                    (SubroutineKind::Function, "newLine").into(),
                ],
            ),
            Class::api(
                "Array",
                vec![
                    (SubroutineKind::Function, "new").into(),
                    (SubroutineKind::Method, "dispose").into(),
                ],
            ),
            Class::api(
                "Output",
                vec![
                    (SubroutineKind::Function, "moveCursor").into(),
                    (SubroutineKind::Function, "printChar").into(),
                    (SubroutineKind::Function, "printString").into(),
                    (SubroutineKind::Function, "printChar").into(),
                    (SubroutineKind::Function, "printInt").into(),
                    (SubroutineKind::Function, "println").into(),
                    (SubroutineKind::Function, "backspace").into(),
                ],
            ),
            Class::api(
                "Screen",
                vec![
                    (SubroutineKind::Function, "clearScreen").into(),
                    (SubroutineKind::Function, "setColor").into(),
                    (SubroutineKind::Function, "drawPixel").into(),
                    (SubroutineKind::Function, "drawLine").into(),
                    (SubroutineKind::Function, "drawRectangle").into(),
                    (SubroutineKind::Function, "drawCircle").into(),
                ],
            ),
            Class::api(
                "Keyboard",
                vec![
                    (SubroutineKind::Function, "keyPressed").into(),
                    (SubroutineKind::Function, "readChar").into(),
                    (SubroutineKind::Function, "readLine").into(),
                    (SubroutineKind::Function, "readInt").into(),
                ],
            ),
            Class::api(
                "Memory",
                vec![
                    (SubroutineKind::Function, "peek").into(),
                    (SubroutineKind::Function, "poke").into(),
                    (SubroutineKind::Function, "alloc").into(),
                    (SubroutineKind::Function, "deAlloc").into(),
                ],
            ),
            Class::api(
                "Sys",
                vec![
                    (SubroutineKind::Function, "halt").into(),
                    (SubroutineKind::Function, "error").into(),
                    (SubroutineKind::Function, "wait").into(),
                ],
            ),
        ]
    }
}
