use crate::{
    jack::{
        expression::{Expression, KeywordConst, Term},
        statement::{Statement, Statements},
        subroutine::{ReturnType, SubroutineDec, SubroutineKind},
    },
    vm::VMLine,
};

use super::{
    statement::compile_statements,
    vartable::{ClassVarTable, SubVarTable},
    CompileError,
};

/// function Main.fibonacci 0
pub fn compile_sub(
    class_vars: &ClassVarTable,
    sub: &SubroutineDec,
) -> Result<Vec<VMLine>, CompileError> {
    let sub_vars: SubVarTable = (class_vars, sub).try_into()?;
    Ok(vec![sub_vars.sub_start()?, compile_sub_body(sub, &sub_vars)?].concat())
}

fn compile_sub_body(
    sub: &SubroutineDec,
    sub_vars: &SubVarTable,
) -> Result<Vec<VMLine>, CompileError> {
    let stmts = ensure_returns(
        sub.kind(),
        sub.return_type(),
        sub_vars,
        sub.body().statements(),
    )?;
    compile_statements(sub_vars, &stmts)
}

fn ensure_returns(
    sub_kind: &SubroutineKind,
    return_type: &ReturnType,
    sub_vars: &SubVarTable,
    stmts: &Statements,
) -> Result<Statements, CompileError> {
    let returns = Branch::resolve_all(stmts, sub_vars)?;
    match (sub_kind, return_type) {
        (SubroutineKind::Constructor, _) => {
            if ResolvedReturnType::This != returns.as_type().unwrap_or(ResolvedReturnType::This) {
                return Err(format!(
                    "constructor must only return 'this': returns {:?}",
                    returns.as_type()
                ));
            }
        }
        (_, ReturnType::Void) => {
            if ResolvedReturnType::Void != returns.as_type().unwrap_or(ResolvedReturnType::Void) {
                return Err(format!(
                    "void typed subroutine must only return 'void': returns {:?}",
                    returns.as_type()
                ));
            }
        }
        _ => {}
    }
    if returns.continues() {
        match (sub_kind, return_type) {
            (SubroutineKind::Constructor, _) => Ok(stmts.append(Statement::new_return_this())),
            (_, ReturnType::Void) => Ok(stmts.append(Statement::new_return_void())),
            (_, ReturnType::Returns(typa)) => {
                Err(format!("subroutine must return a value of type {:?}", typa))
            }
        }
    } else {
        Ok(stmts.clone())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResolvedReturnType {
    Void,
    This,
    Other,
}

fn resolve_type(
    _sub_vars: &SubVarTable,
    oexpr: &Option<Expression>,
) -> Result<ResolvedReturnType, CompileError> {
    match oexpr {
        None => Ok(ResolvedReturnType::Void),
        Some(expr)
            if expr.is_single_term()
                && matches!(expr.term(), Term::KeywordConst(KeywordConst::This)) =>
        {
            Ok(ResolvedReturnType::This)
        }
        _ => Ok(ResolvedReturnType::Other),
    }
}

fn combine_types(
    ltype: &ResolvedReturnType,
    rtype: &ResolvedReturnType,
) -> Result<ResolvedReturnType, CompileError> {
    if ltype == rtype {
        Ok(*ltype)
    } else {
        Err(format!(
            "resolved types do not match: {:?} != {:?}",
            ltype, rtype
        ))
    }
}

#[derive(Debug, Copy, Clone)]
enum Branch {
    Continues,
    Returns(ResolvedReturnType),
    Splits(ResolvedReturnType),
}

impl Branch {
    fn as_type(&self) -> Option<ResolvedReturnType> {
        match self {
            Self::Returns(returns) | Self::Splits(returns) => Some(*returns),
            Self::Continues => None,
        }
    }

    fn continues(&self) -> bool {
        !matches!(self, Self::Returns(..))
    }

    fn coalesce(&self, other: &Self) -> Result<Self, CompileError> {
        match (self, other) {
            (Self::Continues, other) | (other, Self::Continues) => Ok(*other),
            (Self::Returns(ltype), Self::Returns(rtype)) => {
                combine_types(ltype, rtype).map(Self::Returns)
            }
            (Self::Splits(ltype), Self::Returns(rtype))
            | (Self::Returns(ltype), Self::Splits(rtype))
            | (Self::Splits(ltype), Self::Splits(rtype)) => {
                combine_types(ltype, rtype).map(Self::Splits)
            }
        }
    }

    ///
    fn resolve_all(stmts: &Statements, sub_vars: &SubVarTable) -> Result<Self, CompileError> {
        stmts.iter().fold(
            Ok(Self::Continues),
            |acc, stmt| -> Result<Self, CompileError> {
                acc.as_ref()
                    .map_err(|err| err.to_owned())
                    .and_then(|last| match last {
                        Self::Continues => Self::resolve(stmt, sub_vars),
                        Self::Returns(..) => Err(format!("unreachable statment {:?}", stmt)),
                        Self::Splits(..) => Self::resolve(stmt, sub_vars).and_then(|rbranch| {
                            last.coalesce(&rbranch).map_err(|err| {
                                format!(
                                    "block returns different types. error: {:?}, statement: {:?}",
                                    err, stmt
                                )
                            })
                        }),
                    })
            },
        )
    }

    fn resolve(stmt: &Statement, sub_vars: &SubVarTable) -> Result<Self, CompileError> {
        match stmt {
            Statement::Return(o_expr) => resolve_type(sub_vars, o_expr.as_ref())
                .map(Branch::Returns)
                .map_err(|err| {
                    format!(
                        "failed to resolve returned type. error: {:?}, statement: {:?}",
                        err, stmt
                    )
                }),
            Statement::If(_, if_stmts, elze_stmts) => {
                Self::resolve_all(if_stmts.as_ref(), sub_vars)?
                    .coalesce(
                        elze_stmts
                            .as_ref()
                            .as_ref()
                            .map(|stmts| Self::resolve_all(stmts, sub_vars))
                            .unwrap_or(Ok(Branch::Continues))
                            .as_ref()?,
                    )
                    .map_err(|err| {
                        format!(
                            "if branches return different types. error: {:?}, statement: {:?}",
                            err, stmt
                        )
                    })
            }
            Statement::While(_, inner_stmts) => Self::resolve_all(inner_stmts.as_ref(), sub_vars),
            _ => Ok(Self::Continues),
        }
    }
}
