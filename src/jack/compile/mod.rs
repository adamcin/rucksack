mod class;
mod expression;
mod statement;
mod sub;
mod vartable;

use std::{convert::TryFrom, io::Error};

use crate::{common::err_invalid_input, vm::VMParsed};

use self::vartable::Api;

use super::class::Class;

pub type CompileError = String;

pub struct JackCompiler {}

impl JackCompiler {
    pub fn compile(&self, classes: Vec<Class>) -> Result<Vec<VMParsed>, Error> {
        let api = Api::new(classes.as_slice());
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
