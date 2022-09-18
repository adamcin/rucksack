use std::collections::HashMap;

use crate::jack::subroutine::SubroutineKind::*;
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
    os_classes: Vec<String>,
    classes: HashMap<String, ClassApi>,
}

impl Api {
    pub fn new(class_path: &[Class]) -> Result<Self, CompileError> {
        let os_api = Self::new_os();
        for class in class_path {
            os_api.check_class(class)?;
        }

        let os_classes = os_api.classes.keys().cloned().collect();
        let mut classes: HashMap<_, _> = os_api.into_iter().collect();

        classes.extend(
            class_path
                .iter()
                .map(|class| (class.name().to_owned(), ClassApi::new(class))),
        );

        Ok(Self {
            os_classes,
            classes,
        })
    }

    pub fn new_os() -> Self {
        let class_path = Self::os_classes();
        let classes: HashMap<_, _> = class_path
            .iter()
            .map(|class| (class.name().to_owned(), ClassApi::new(class)))
            .collect();
        Self {
            os_classes: classes.keys().cloned().collect(),
            classes,
        }
    }

    fn check_class(&self, class: &Class) -> Result<(), CompileError> {
        if let Some(class_api) = self.classes.get(class.name()) {
            for (sub_name, my_sub) in class_api.subs.iter() {
                if let Some(sub) = class.sub(&Id::from(sub_name.as_str())) {
                    self.check_sub(class.name(), sub_name.as_str(), my_sub, sub)?;
                } else {
                    return Err(format!(
                        "class {} must implement subroutine {}",
                        class.name(),
                        sub_name
                    ));
                }
            }
        }
        Ok(())
    }

    fn check_sub(
        &self,
        class_name: &str,
        sub_name: &str,
        my_sub: &SubApi,
        sub: &SubroutineDec,
    ) -> Result<(), CompileError> {
        if &my_sub.kind != sub.kind() {
            return Err(format!(
                "subroutine {} in class {} must be of kind {}",
                sub_name, class_name, &my_sub.kind
            ));
        }
        Ok(())
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

    pub fn is_os_class(&self, class_name: &Id) -> bool {
        self.os_classes.contains(&class_name.as_str().to_owned())
    }

    pub fn has_class(&self, class_name: &Id) -> bool {
        self.classes.contains_key(class_name.as_str())
    }

    pub fn os_classes() -> Vec<Class> {
        vec![
            Class::api(
                "Math",
                vec![
                    (Function, "abs").into(),
                    (Function, "multiply").into(),
                    (Function, "divide").into(),
                    (Function, "min").into(),
                    (Function, "max").into(),
                    (Function, "sqrt").into(),
                ],
            ),
            Class::api(
                "String",
                vec![
                    (Constructor, "new").into(),
                    (Method, "dispose").into(),
                    (Method, "length").into(),
                    (Method, "charAt").into(),
                    (Method, "setCharAt").into(),
                    (Method, "appendChar").into(),
                    (Method, "eraseLastChar").into(),
                    (Method, "intValue").into(),
                    (Method, "setInt").into(),
                    (Function, "backSpace").into(),
                    (Function, "doubleQuote").into(),
                    (Function, "newLine").into(),
                ],
            ),
            Class::api(
                "Array",
                vec![(Function, "new").into(), (Method, "dispose").into()],
            ),
            Class::api(
                "Output",
                vec![
                    (Function, "moveCursor").into(),
                    (Function, "printChar").into(),
                    (Function, "printString").into(),
                    (Function, "printChar").into(),
                    (Function, "printInt").into(),
                    (Function, "println").into(),
                    (Function, "backSpace").into(),
                ],
            ),
            Class::api(
                "Screen",
                vec![
                    (Function, "clearScreen").into(),
                    (Function, "setColor").into(),
                    (Function, "drawPixel").into(),
                    (Function, "drawLine").into(),
                    (Function, "drawRectangle").into(),
                    (Function, "drawCircle").into(),
                ],
            ),
            Class::api(
                "Keyboard",
                vec![
                    (Function, "keyPressed").into(),
                    (Function, "readChar").into(),
                    (Function, "readLine").into(),
                    (Function, "readInt").into(),
                ],
            ),
            Class::api(
                "Memory",
                vec![
                    (Function, "peek").into(),
                    (Function, "poke").into(),
                    (Function, "alloc").into(),
                    (Function, "deAlloc").into(),
                ],
            ),
            Class::api(
                "Sys",
                vec![
                    (Function, "halt").into(),
                    (Function, "error").into(),
                    (Function, "wait").into(),
                ],
            ),
        ]
    }
}
use std::collections::hash_map::IntoIter;
impl IntoIterator for Api {
    type IntoIter = IntoIter<String, ClassApi>;
    type Item = (String, ClassApi);

    fn into_iter(self) -> Self::IntoIter {
        self.classes.into_iter()
    }
}
