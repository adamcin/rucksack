mod api;
mod class;
mod expression;
mod statement;
mod sub;
mod vartable;

use std::{convert::TryFrom, io::Error};

use crate::{common::err_invalid_input, vm::VMParsed};

use self::api::Api;

use super::class::Class;

pub type CompileError = String;
pub fn comberr(wrap: String, prev: &CompileError) -> CompileError {
    format!("{}\n* {}", wrap, prev)
}

pub struct JackCompiler {}

impl JackCompiler {
    pub fn compile(&self, classes: Vec<Class>) -> Result<Vec<VMParsed>, Error> {
        let class_path = vec![Api::os_classes(), classes.to_vec()].concat();
        let api = Api::new(class_path.as_slice());
        classes
            .into_iter()
            .map(|class| {
                let name = class.name().to_owned();
                VMParsed::try_from((&api, class))
                    .map_err(|err| format!("class {} compile error {}", name, err))
            })
            .map(|r| r.map_err(err_invalid_input))
            .collect()
    }
}
