use std::{collections::HashMap, convert::TryFrom, io::Error};

use crate::{
    jack::subroutine::{SubroutineDec, SubroutineKind},
    vm::VMParsed,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarKind {
    Static,
    Field,
    Arg,
    Local,
}

pub struct Var {
    name: String,
    kind: VarKind,
    typa: Type,
    index: usize,
}

pub struct ClassVarTable {
    name: String,
    vars: HashMap<String, Var>,
    n_static: usize,
    n_field: usize,
}

impl ClassVarTable {
    fn new(name: Id) -> Self {
        Self {
            name: name.to_string(),
            vars: HashMap::new(),
            n_static: 0,
            n_field: 0,
        }
    }

    pub fn name<'a>(&'a self) -> &'a str {
        &self.name
    }

    pub fn var<'a>(&'a self, name: Id) -> Result<&'a Var, CompileError> {
        self.vars
            .get(name.as_str())
            .map(|var| Ok(var))
            .unwrap_or_else(|| Err(format!("var {} not defined", name.as_str())))
    }

    fn add_field(&mut self, name: &Id, typa: Type) -> Result<(), CompileError> {
        self.vars.insert(
            name.to_string(),
            Var {
                name: name.to_string(),
                kind: VarKind::Field,
                typa,
                index: self.n_field,
            },
        );
        self.n_field = self.n_field + 1;
        Ok(())
    }

    fn add_static(&mut self, name: &Id, typa: Type) -> Result<(), CompileError> {
        self.vars.insert(
            name.to_string(),
            Var {
                name: name.to_string(),
                kind: VarKind::Static,
                typa,
                index: self.n_static,
            },
        );
        self.n_static = self.n_static + 1;
        Ok(())
    }
}

impl TryFrom<&Class> for ClassVarTable {
    type Error = CompileError;
    fn try_from(value: &Class) -> Result<Self, Self::Error> {
        let mut class_vars = ClassVarTable::new(Id::from(value.name()));
        for var in value.vars() {
            match var.var_kind() {
                ClassVarKind::Field => {
                    for var_name in var.var_names() {
                        class_vars.add_field(var_name, var.var_type().copy())?;
                    }
                }
                ClassVarKind::Static => {
                    for var_name in var.var_names() {
                        class_vars.add_static(var_name, var.var_type().copy())?;
                    }
                }
            }
        }
        Ok(class_vars)
    }
}

pub struct SubVarTable<'a> {
    class_vars: &'a ClassVarTable,
    vars: HashMap<String, Var>,
    n_arg: usize,
    n_local: usize,
}

impl<'a> SubVarTable<'a> {
    pub fn new(class_vars: &'a ClassVarTable) -> Self {
        Self {
            class_vars,
            vars: HashMap::new(),
            n_arg: 0,
            n_local: 0,
        }
    }

    pub fn var(&'a self, name: Id) -> Result<&'a Var, CompileError> {
        self.vars
            .get(name.as_str())
            .map(|var| Ok(var))
            .unwrap_or_else(|| self.class_vars.var(name))
    }

    pub fn this(&'a self) -> Result<&'a Var, CompileError> {
        self.vars
            .get(Keyword::This.as_str())
            .filter(|var| &var.index == &0)
            .map(|var| Ok(var))
            .unwrap_or_else(|| Err("this not defined".to_owned()))
    }

    pub fn add_this(&mut self) -> Result<(), CompileError> {
        if self.n_arg > 0 {
            return Err("this must be arg0".to_owned());
        }
        self.vars.insert(
            Keyword::This.as_str().to_owned(),
            Var {
                name: Keyword::This.as_str().to_owned(),
                kind: VarKind::Arg,
                typa: Type::ClassName(Id::from(self.class_vars.name())),
                index: self.n_arg,
            },
        );
        self.n_arg = self.n_arg + 1;
        Ok(())
    }

    pub fn add_arg(&mut self, name: &Id, typa: Type) -> Result<(), CompileError> {
        self.vars.insert(
            name.to_string(),
            Var {
                name: name.to_string(),
                kind: VarKind::Arg,
                typa,
                index: self.n_arg,
            },
        );
        self.n_arg = self.n_arg + 1;
        Ok(())
    }

    fn add_local(&mut self, name: &Id, typa: Type) -> Result<(), CompileError> {
        self.vars.insert(
            name.to_string(),
            Var {
                name: name.to_string(),
                kind: VarKind::Local,
                typa,
                index: self.n_local,
            },
        );
        self.n_local = self.n_local + 1;
        Ok(())
    }
}

impl<'a, 'b> TryFrom<(&'a ClassVarTable, &SubroutineDec)> for SubVarTable<'b>
where
    'a: 'b,
{
    type Error = CompileError;
    fn try_from(value: (&'a ClassVarTable, &SubroutineDec)) -> Result<Self, Self::Error> {
        let (class_vars, sub) = value;
        let mut sub_vars = SubVarTable::new(class_vars);
        match sub.kind() {
            SubroutineKind::Method => sub_vars.add_this()?,
            _ => {}
        };
        for var in sub.params() {
            sub_vars.add_arg(var.var_name(), var.var_type().copy())?;
        }
        for var in sub.body().vars() {
            for name in var.var_names() {
                sub_vars.add_local(name, var.var_type().copy())?;
            }
        }
        Ok(sub_vars)
    }
}

#[cfg(test)]
mod tests {
    use crate::jack::{
        common::testutil::read_class,
        compile::{
            vartable::{Var, VarKind},
            CompileError,
        },
        id::Id,
        typea::Type,
    };

    use super::ClassVarTable;

    #[test]
    fn from_class() {
        let class = read_class("data/11/Square/SquareGame.jack").expect("");
        let r_class_vars: Result<ClassVarTable, CompileError> = (&class).try_into();
        assert!(r_class_vars.is_ok());
        if let Ok(class_vars) = r_class_vars.as_ref() {
            assert_eq!("SquareGame", class_vars.name());
            let r_var_square = class_vars.var(Id::from("square"));
            assert!((r_var_square).is_ok());
            if let Ok(var) = r_var_square {
                assert_eq!(&0, &var.index);
                assert_eq!(&VarKind::Field, &var.kind);
                assert_eq!(&Type::ClassName(Id::from("Square")), &var.typa);
                assert_eq!("square", &var.name);
            }

            let r_var_direction = class_vars.var(Id::from("direction"));
            assert!((r_var_direction).is_ok());
            if let Ok(var) = r_var_direction {
                assert_eq!(&1, &var.index);
                assert_eq!(&VarKind::Field, &var.kind);
                assert_eq!(&Type::Int, &var.typa);
                assert_eq!("direction", &var.name);
            }
        }
    }
}
