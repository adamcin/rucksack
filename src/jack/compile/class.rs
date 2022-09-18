use std::convert::TryFrom;

use super::api::Api;
use super::sub::compile_sub;
use super::vartable::*;
use super::CompileError;
use crate::jack::id::Id;
use crate::jack::subroutine::SubroutineDec;
use crate::jack::subroutine::SubroutineKind;
use crate::{jack::class::Class, vm::VMParsed};

impl TryFrom<(&Api, Class)> for VMParsed {
    type Error = CompileError;

    fn try_from(value: (&Api, Class)) -> Result<Self, Self::Error> {
        let (api, class) = value;
        let class_vars: ClassVarTable = (api, &class).try_into()?;
        let lines = class.subs().iter().fold(Ok(Vec::new()), |acc, sub| {
            acc.and_then(|prev| compile_sub(&class_vars, sub).map(|add| vec![prev, add].concat()))
        })?;
        if api.is_os_class(class.id()) && class.sub(&Id::from("init")).is_none() {
            let init_stub: SubroutineDec = (SubroutineKind::Function, "init").into();
            compile_sub(&class_vars, &init_stub)
                .map(|last| VMParsed::new(class.name().to_owned(), vec![lines, last].concat()))
        } else {
            Ok(VMParsed::new(class.name().to_owned(), lines))
        }
    }
}
