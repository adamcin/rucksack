use std::{
    collections::HashMap,
    convert::{TryFrom, TryInto},
};

use crate::{
    jack::subroutine::{SubroutineDec, SubroutineKind},
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

    pub fn is_method(&self, class_name: &Id, sub_name: &Id) -> bool {
        self.classes
            .get(class_name.as_str())
            .and_then(|class| class.subs.get(sub_name.as_str()))
            .filter(|sub| sub.kind == SubroutineKind::Method)
            .is_some()
    }

    // pub fn os_classes() -> Vec<Class> {

    // }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarKind {
    Static,
    Field,
    Arg,
    Local,
    Pointer,
}

impl VarKind {
    pub fn as_segment(&self) -> Segment {
        match self {
            Self::Static => Segment::Static,
            Self::Field => Segment::This,
            Self::Arg => Segment::Argument,
            Self::Local => Segment::Local,
            Self::Pointer => Segment::Pointer,
        }
    }
}

#[derive(Debug)]
pub struct Var {
    name: String,
    kind: VarKind,
    typa: Type,
    index: i16,
}

impl Var {
    pub fn push(&self) -> Vec<VMLine> {
        vec![VMLine::Push(self.kind.as_segment(), self.index)]
    }
    pub fn pop(&self) -> Vec<VMLine> {
        vec![VMLine::Pop(self.kind.as_segment(), self.index)]
    }
    pub fn var_type(&self) -> &Type {
        &self.typa
    }
    pub fn index(&self) -> i16 {
        self.index
    }
}

pub struct ClassVarTable<'a> {
    api: &'a Api,
    name: String,
    vars: HashMap<String, Var>,
    n_static: usize,
    n_field: usize,
}

impl<'a> ClassVarTable<'a> {
    fn new(api: &'a Api, name: Id) -> Self {
        Self {
            api,
            name: name.to_string(),
            vars: HashMap::new(),
            n_static: 0,
            n_field: 0,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn var(&self, name: &Id) -> Result<&Var, CompileError> {
        self.vars
            .get(name.as_str())
            .map(Ok)
            .unwrap_or_else(|| Err(format!("var {} not defined", name.as_str())))
    }

    fn add_field(&mut self, name: &Id, typa: Type) -> Result<(), CompileError> {
        self.vars.insert(
            name.to_string(),
            Var {
                name: name.to_string(),
                kind: VarKind::Field,
                typa,
                index: self.n_field.try_into().map_err(|err| {
                    format!("failed to construct field var {:?} error: {}", name, err)
                })?,
            },
        );
        self.n_field += 1;
        Ok(())
    }

    fn add_static(&mut self, name: &Id, typa: Type) -> Result<(), CompileError> {
        self.vars.insert(
            name.to_string(),
            Var {
                name: name.to_string(),
                kind: VarKind::Static,
                typa,
                index: self.n_static.try_into().map_err(|err| {
                    format!("failed to construct static var {:?} error: {}", name, err)
                })?,
            },
        );
        self.n_static += 1;
        Ok(())
    }

    pub fn var_count(&self, kind: VarKind) -> Result<i16, CompileError> {
        match kind {
            VarKind::Field => self
                .n_field
                .try_into()
                .map_err(|err| format!("failed to convert field var count to i16: {}", err)),
            VarKind::Static => self
                .n_static
                .try_into()
                .map_err(|err| format!("failed to convert static var count to i16: {}", err)),
            _ => Ok(0),
        }
    }

    pub fn api(&self) -> &Api {
        self.api
    }

    pub fn is_this_method(&self, sub_name: &Id) -> bool {
        self.api.is_method(&Id::from(self.name()), sub_name)
    }
}

impl<'a> TryFrom<(&'a Api, &Class)> for ClassVarTable<'a> {
    type Error = CompileError;
    fn try_from(value: (&'a Api, &Class)) -> Result<Self, Self::Error> {
        let (api, class) = value;
        let mut class_vars = ClassVarTable::new(api, Id::from(class.name()));
        for var in class.vars() {
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

pub struct SubVarTable<'a, 'b> {
    class_vars: &'b ClassVarTable<'a>,
    vars: HashMap<String, Var>,
    n_arg: usize,
    n_local: usize,
}

impl<'a, 'b> SubVarTable<'a, 'b> {
    pub fn new(class_vars: &'b ClassVarTable<'a>) -> Self {
        Self {
            class_vars,
            vars: HashMap::new(),
            n_arg: 0,
            n_local: 0,
        }
    }

    pub fn var(&'a self, name: &Id) -> Result<&'a Var, CompileError> {
        self.vars
            .get(name.as_str())
            .map(Ok)
            .unwrap_or_else(|| self.class_vars.var(name))
    }

    pub fn this(&'a self) -> Result<&'a Var, CompileError> {
        self.vars
            .get(Keyword::This.as_str())
            .filter(|var| var.index == 0)
            .map(Ok)
            .unwrap_or_else(|| Err("this not defined".to_owned()))
    }

    pub fn this_type(&'a self) -> Id {
        Id::from(self.class_vars.name())
    }

    pub fn add_this(&mut self, sub_kind: &SubroutineKind) -> Result<(), CompileError> {
        let var_kind = match sub_kind {
            SubroutineKind::Function => {
                Err("'this' var not allowed for 'function' kind subroutine".to_owned())
            }
            SubroutineKind::Method => {
                if self.n_arg > 0 {
                    Err("this must be arg0".to_owned())
                } else {
                    Ok(VarKind::Arg)
                }
            }
            SubroutineKind::Constructor => Ok(VarKind::Pointer),
        }?;
        self.vars.insert(
            Keyword::This.as_str().to_owned(),
            Var {
                name: Keyword::This.as_str().to_owned(),
                kind: var_kind,
                typa: Type::ClassName(Id::from(self.class_vars.name())),
                index: self
                    .n_arg
                    .try_into()
                    .map_err(|err| format!("failed to construct this var error: {}", err))?,
            },
        );
        self.n_arg += 1;
        Ok(())
    }

    pub fn add_arg(&mut self, name: &Id, typa: Type) -> Result<(), CompileError> {
        self.vars.insert(
            name.to_string(),
            Var {
                name: name.to_string(),
                kind: VarKind::Arg,
                typa,
                index: self.n_arg.try_into().map_err(|err| {
                    format!("failed to construct arg var {:?} error: {}", name, err)
                })?,
            },
        );
        self.n_arg += 1;
        Ok(())
    }

    fn add_local(&mut self, name: &Id, typa: Type) -> Result<(), CompileError> {
        self.vars.insert(
            name.to_string(),
            Var {
                name: name.to_string(),
                kind: VarKind::Local,
                typa,
                index: self.n_local.try_into().map_err(|err| {
                    format!("failed to construct local var {:?} error: {}", name, err)
                })?,
            },
        );
        self.n_local += 1;
        Ok(())
    }

    pub fn var_count(&self, kind: VarKind) -> Result<i16, CompileError> {
        match kind {
            VarKind::Arg => self
                .n_arg
                .try_into()
                .map_err(|err| format!("failed to convert arg var count to i16: {}", err)),
            VarKind::Local => self
                .n_local
                .try_into()
                .map_err(|err| format!("failed to convert local var count to i16: {}", err)),
            _ => self.class_vars.var_count(kind),
        }
    }

    pub fn is_method(&self, qualifier: Option<&Id>, name: &Id) -> bool {
        if let Some(qual) = qualifier {
            if let Ok(var) = self.var(qual) {
                match &var.typa {
                    Type::ClassName(class_name) => {
                        self.class_vars.api().is_method(class_name, name)
                    }
                    _ => false,
                }
            } else {
                self.class_vars.api().is_method(qual, name)
            }
        } else {
            self.class_vars.is_this_method(name)
        }
    }
}

impl<'a, 'b, 'c> TryFrom<(&'b ClassVarTable<'a>, &SubroutineDec)> for SubVarTable<'b, 'c>
where
    'a: 'b,
    'b: 'c,
{
    type Error = CompileError;
    fn try_from(value: (&'b ClassVarTable<'a>, &SubroutineDec)) -> Result<Self, Self::Error> {
        let (class_vars, sub) = value;
        let mut sub_vars = SubVarTable::new(class_vars);
        if !matches!(sub.kind(), SubroutineKind::Function) {
            sub_vars.add_this(sub.kind())?;
        }
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
    use std::convert::TryInto;

    use crate::jack::{
        common::testutil::read_class,
        compile::{
            vartable::{Api, SubVarTable, VarKind},
            CompileError,
        },
        id::Id,
        typea::Type,
    };

    use super::ClassVarTable;

    #[test]
    fn try_from_square_game() {
        let class = read_class("data/11/Square/SquareGame.jack").expect("");
        let classes = vec![class];
        let api = Api::new(&classes);
        let r_class_vars: Result<ClassVarTable, CompileError> =
            (&api, classes.first().unwrap()).try_into();
        assert!(r_class_vars.is_ok());
        if let Ok(class_vars) = r_class_vars.as_ref() {
            assert_eq!("SquareGame", class_vars.name());
            assert_eq!(Ok(0), class_vars.var_count(VarKind::Arg));
            assert_eq!(Ok(0), class_vars.var_count(VarKind::Local));
            assert_eq!(Ok(0), class_vars.var_count(VarKind::Static));
            assert_eq!(Ok(2), class_vars.var_count(VarKind::Field));
            let r_var_square = class_vars.var(&Id::from("square"));
            assert!((r_var_square).is_ok());
            if let Ok(var) = r_var_square {
                assert_eq!(&0, &var.index);
                assert_eq!(&VarKind::Field, &var.kind);
                assert_eq!(&Type::ClassName(Id::from("Square")), &var.typa);
                assert_eq!("square", &var.name);
            }

            let r_var_direction = class_vars.var(&Id::from("direction"));
            assert!((r_var_direction).is_ok());
            if let Ok(var) = r_var_direction {
                assert_eq!(&1, &var.index);
                assert_eq!(&VarKind::Field, &var.kind);
                assert_eq!(&Type::Int, &var.typa);
                assert_eq!("direction", &var.name);
            }

            let sub_dec_new = classes
                .first()
                .unwrap()
                .subs()
                .iter()
                .find(|dec| dec.name() == &Id::from("new"))
                .expect("");
            let r_sub_vars_new: Result<SubVarTable, CompileError> =
                (class_vars, sub_dec_new).try_into();
            assert!((r_sub_vars_new).is_ok());
            if let Ok(sub_vars) = r_sub_vars_new.as_ref() {
                let r_var_this = sub_vars.this();
                assert!(r_var_this.is_err());
                assert_eq!(Ok(0), sub_vars.var_count(VarKind::Arg));
                assert_eq!(Ok(0), sub_vars.var_count(VarKind::Local));
                assert_eq!(Ok(0), sub_vars.var_count(VarKind::Static));
                assert_eq!(Ok(2), sub_vars.var_count(VarKind::Field));
            }

            let sub_dec_run = classes
                .first()
                .unwrap()
                .subs()
                .iter()
                .find(|dec| dec.name() == &Id::from("run"))
                .expect("");
            let r_sub_vars: Result<SubVarTable, CompileError> =
                (class_vars, sub_dec_run).try_into();
            assert!((r_sub_vars).is_ok());
            if let Ok(sub_vars) = r_sub_vars.as_ref() {
                let r_var_this = sub_vars.this();
                assert!(r_var_this.is_ok());
                assert_eq!(Ok(1), sub_vars.var_count(VarKind::Arg));
                assert_eq!(Ok(2), sub_vars.var_count(VarKind::Local));
                assert_eq!(Ok(0), sub_vars.var_count(VarKind::Static));
                assert_eq!(Ok(2), sub_vars.var_count(VarKind::Field));
                if let Ok(var) = r_var_this {
                    assert_eq!(&0, &var.index);
                    assert_eq!(&VarKind::Arg, &var.kind);
                    assert_eq!(&Type::ClassName(Id::from(class_vars.name())), &var.typa);
                    assert_eq!("this", &var.name);
                }
                let r_var_key = sub_vars.var(&Id::from("key"));
                assert!(r_var_key.is_ok());
                if let Ok(var) = r_var_key {
                    assert_eq!(&0, &var.index);
                    assert_eq!(&VarKind::Local, &var.kind);
                    assert_eq!(&Type::Char, &var.typa);
                    assert_eq!("key", &var.name);
                }
                let r_var_exit = sub_vars.var(&Id::from("exit"));
                assert!(r_var_exit.is_ok());
                if let Ok(var) = r_var_exit {
                    assert_eq!(&1, &var.index);
                    assert_eq!(&VarKind::Local, &var.kind);
                    assert_eq!(&Type::Boolean, &var.typa);
                    assert_eq!("exit", &var.name);
                }
            }
        }
    }
}
