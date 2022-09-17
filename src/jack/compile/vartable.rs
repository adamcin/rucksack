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
    api::Api,
    CompileError,
};

pub enum ScopeId {
    Class(Id),
    ClassVar(Id, Id),
    PrimitiveVar(Id),
}

#[derive(Debug)]
pub struct ScopeCall {
    class: Id,
    var: Option<Id>,
    sub_name: Id,
    sub_kind: SubroutineKind,
}

impl ScopeCall {
    pub fn new(class: Id, var: Option<Id>, sub_name: Id, sub_kind: SubroutineKind) -> Self {
        Self {
            class,
            var,
            sub_name,
            sub_kind,
        }
    }

    pub fn class(&self) -> &Id {
        &self.class
    }
    pub fn var(&self) -> Option<&Id> {
        self.var.as_ref()
    }
    pub fn sub_name(&self) -> &Id {
        &self.sub_name
    }
    pub fn sub_kind(&self) -> &SubroutineKind {
        &self.sub_kind
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarKind {
    Static,
    Field,
    Arg,
    Local,
    This,
}

impl VarKind {
    pub fn push_segment(&self) -> Segment {
        match self {
            Self::Static => Segment::Static,
            Self::Field => Segment::This,
            Self::Arg => Segment::Argument,
            Self::Local => Segment::Local,
            Self::This => Segment::Pointer,
        }
    }

    pub fn pop_segment(&self) -> Segment {
        match self {
            Self::Static => Segment::Static,
            Self::Field => Segment::This,
            Self::Arg => Segment::Argument,
            Self::Local => Segment::Local,
            Self::This => Segment::Pointer,
        }
    }
}

#[derive(Debug)]
pub struct Var {
    kind: VarKind,
    typa: Type,
    index: i16,
}

impl Var {
    pub fn push(&self) -> Vec<VMLine> {
        vec![VMLine::Push(self.kind().push_segment(), self.index())]
    }
    pub fn pop(&self) -> Vec<VMLine> {
        vec![VMLine::Pop(self.kind().pop_segment(), self.index())]
    }
    pub fn kind(&self) -> &VarKind {
        &self.kind
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

    pub fn resolve_id(&self, name: &Id) -> Result<ScopeId, CompileError> {
        if let Some(var) = self.vars.get(name.as_str()) {
            if let Type::ClassName(class_name) = var.var_type() {
                Ok(ScopeId::ClassVar(class_name.clone(), name.clone()))
            } else {
                Ok(ScopeId::PrimitiveVar(name.clone()))
            }
        } else if self.api.has_class(name) {
            Ok(ScopeId::Class(name.clone()))
        } else {
            Err(format!("id {name} not found in scope"))
        }
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
    name: String,
    kind: SubroutineKind,
    vars: HashMap<String, Var>,
    n_arg: usize,
    n_local: usize,
}

impl<'a, 'b> SubVarTable<'a, 'b> {
    pub fn new(class_vars: &'b ClassVarTable<'a>, sub: &SubroutineDec) -> Self {
        Self {
            class_vars,
            name: format!("{}.{}", class_vars.name(), sub.name().as_str()),
            kind: sub.kind().to_owned(),
            vars: HashMap::new(),
            n_arg: 0,
            n_local: 0,
        }
    }

    pub fn resolve_id(&self, name: &Id) -> Result<ScopeId, CompileError> {
        if let Some(var) = self.vars.get(name.as_str()) {
            if let Type::ClassName(class_name) = var.var_type() {
                Ok(ScopeId::ClassVar(class_name.clone(), name.clone()))
            } else {
                Ok(ScopeId::PrimitiveVar(name.clone()))
            }
        } else {
            self.class_vars.resolve_id(name)
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
        if matches!(sub_kind, SubroutineKind::Function) {
            return Err("'this' var not allowed for 'function' kind subroutine".to_owned());
        }
        if self.n_arg > 0 {
            return Err("this must be arg0".to_owned());
        }
        self.vars.insert(
            Keyword::This.as_str().to_owned(),
            Var {
                kind: VarKind::This,
                typa: Type::ClassName(Id::from(self.class_vars.name())),
                index: self
                    .n_arg
                    .try_into()
                    .map_err(|err| format!("failed to construct this var error: {}", err))?,
            },
        );
        if matches!(sub_kind, SubroutineKind::Method) {
            self.n_arg += 1;
        }
        Ok(())
    }

    pub fn add_arg(&mut self, name: &Id, typa: Type) -> Result<(), CompileError> {
        self.vars.insert(
            name.to_string(),
            Var {
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

    pub fn sub_start(&self) -> Result<Vec<VMLine>, CompileError> {
        Ok(vec![
            vec![VMLine::Function(
                self.name.to_owned(),
                self.var_count(VarKind::Local)?,
            )],
            if matches!(self.kind, SubroutineKind::Constructor) {
                vec![
                    VMLine::Push(
                        Segment::Constant,
                        self.class_vars.var_count(VarKind::Field)?,
                    ),
                    VMLine::Call("Memory.alloc".to_owned(), 1),
                    VMLine::Pop(Segment::Pointer, 0),
                ]
            } else {
                Vec::new()
            },
            if matches!(self.kind, SubroutineKind::Method) {
                vec![
                    VMLine::Push(Segment::Argument, 0),
                    VMLine::Pop(Segment::Pointer, 0),
                ]
            } else {
                Vec::new()
            },
        ]
        .concat())
    }

    pub fn resolve_call(
        &self,
        qualifier: Option<&Id>,
        name: &Id,
    ) -> Result<ScopeCall, CompileError> {
        if let Some(qual) = qualifier {
            match self.resolve_id(qual)? {
                ScopeId::Class(class_name) => self
                    .class_vars
                    .api()
                    .resolve_sub_kind(&class_name, name)
                    .map(|kind| ScopeCall::new(class_name, None, name.clone(), kind)),
                ScopeId::ClassVar(class_name, var_name) => self
                    .class_vars
                    .api()
                    .resolve_sub_kind(&class_name, name)
                    .map(|kind| ScopeCall::new(class_name, Some(var_name), name.clone(), kind)),
                ScopeId::PrimitiveVar(var_name) => {
                    Err(format!("var {var_name} represents a primitive type"))
                }
            }
        } else {
            let sub_kind = self
                .class_vars
                .api()
                .resolve_sub_kind(&self.this_type(), name)?;
            Ok(ScopeCall::new(
                self.this_type(),
                None,
                name.clone(),
                sub_kind,
            ))
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
        let mut sub_vars = SubVarTable::new(class_vars, sub);
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
            }

            let r_var_direction = class_vars.var(&Id::from("direction"));
            assert!((r_var_direction).is_ok());
            if let Ok(var) = r_var_direction {
                assert_eq!(&1, &var.index);
                assert_eq!(&VarKind::Field, &var.kind);
                assert_eq!(&Type::Int, &var.typa);
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
                assert!(r_var_this.is_ok());
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
                    assert_eq!(&0, &var.index());
                    assert_eq!(&VarKind::This, &var.kind);
                    assert_eq!(
                        &Type::ClassName(Id::from(class_vars.name())),
                        var.var_type()
                    );
                }
                let r_var_key = sub_vars.var(&Id::from("key"));
                assert!(r_var_key.is_ok());
                if let Ok(var) = r_var_key {
                    assert_eq!(&0, &var.index());
                    assert_eq!(&VarKind::Local, var.kind());
                    assert_eq!(&Type::Char, var.var_type());
                }
                let r_var_exit = sub_vars.var(&Id::from("exit"));
                assert!(r_var_exit.is_ok());
                if let Ok(var) = r_var_exit {
                    assert_eq!(&1, &var.index());
                    assert_eq!(&VarKind::Local, var.kind());
                    assert_eq!(&Type::Boolean, var.var_type());
                }
            }
        }
    }
}
