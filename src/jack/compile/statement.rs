use std::fmt::Display;

use crate::{
    jack::{
        expression::Expression,
        id::Id,
        statement::{Statement, Statements},
    },
    vm::{Segment, VMLine},
};

use super::{
    expression::{compile_expression, compile_term_call},
    vartable::SubVarTable,
    CompileError,
};

#[derive(Debug, Clone)]
pub struct StatementIndex {
    parent: Vec<usize>,
    index: usize,
}

impl StatementIndex {
    fn new(parent: Vec<usize>, index: usize) -> Self {
        Self { parent, index }
    }
    fn init() -> Self {
        Self {
            parent: Vec::new(),
            index: 0,
        }
    }
    fn as_parent(&self) -> Vec<usize> {
        vec![self.parent.to_vec(), vec![self.index]].concat()
    }
    fn next(&self) -> Self {
        Self::new(self.parent.to_vec(), self.index + 1)
    }
    fn child(&self) -> Self {
        Self::new(self.as_parent(), 0)
    }
}

impl Display for StatementIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "stmt{}",
            self.as_parent()
                .iter()
                .map(|idx| idx.to_string())
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}

pub fn compile_statements(
    vars: &SubVarTable,
    stmts: &Statements,
    init_idx: Option<&StatementIndex>,
) -> Result<Vec<VMLine>, CompileError> {
    let init = (
        init_idx.cloned().unwrap_or_else(StatementIndex::init),
        Vec::new(),
    );
    stmts
        .iter()
        .fold(Ok(init), |acc, stmt| {
            acc.and_then(|(index, prev)| {
                compile_statement(&index, vars, stmt)
                    .map(|next| (index.next(), vec![prev, next].concat()))
            })
        })
        .map(|(_, lines)| lines)
}

fn compile_statement(
    index: &StatementIndex,
    vars: &SubVarTable,
    stmt: &Statement,
) -> Result<Vec<VMLine>, CompileError> {
    match stmt {
        Statement::Return(oexpr) => oexpr
            .as_ref()
            .as_ref()
            .map(|expr| compile_expression(vars, expr))
            .unwrap_or_else(|| Ok(vec![VMLine::Push(Segment::Constant, 0)]))
            .map(|lines| vec![lines, vec![VMLine::Return]].concat()),
        Statement::Do(call) => compile_term_call(vars, call.as_ref())
            .map(|lines| vec![lines, vec![VMLine::Pop(Segment::Temp, 0)]].concat()),
        Statement::Let(var_name, ovar_sub, oexpr) if ovar_sub.is_none() => {
            compile_expression(vars, oexpr.as_ref())
                .and_then(|prev| vars.var(var_name).map(|var| vec![prev, var.pop()].concat()))
        }
        Statement::Let(var, ovar_sub, oexpr) => {
            compile_let_left(vars, var, ovar_sub.as_ref().as_ref()).and_then(|left| {
                compile_expression(vars, oexpr.as_ref()).map(|right| {
                    vec![
                        left,
                        right,
                        vec![
                            VMLine::Pop(Segment::Temp, 0),
                            VMLine::Pop(Segment::Pointer, 1),
                            VMLine::Push(Segment::Temp, 0),
                            VMLine::Pop(Segment::That, 0),
                        ],
                    ]
                    .concat()
                })
            })
        }
        Statement::While(condition, block) => compile_expression(vars, condition.as_ref())
            .and_then(|prev| {
                compile_statements(vars, block.as_ref(), Some(index)).map(|next| {
                    let label_while = format!("{}.WHILE", index);
                    let label_end_while = format!("{}.END_WHILE", index);
                    vec![
                        vec![VMLine::Label(label_while.to_owned())],
                        prev,
                        vec![VMLine::Not, VMLine::IfGoto(label_end_while.to_owned())],
                        next,
                        vec![VMLine::Goto(label_while), VMLine::Label(label_end_while)],
                    ]
                    .concat()
                })
            }),
        Statement::If(condition, if_block, oelse_block) => {
            let sub_index = index.child();
            let label_end_if = format!("{}.END_IF", index);
            let label_if_false = if oelse_block.is_some() {
                format!("{}.IF_FALSE", index)
            } else {
                label_end_if.as_str().to_owned()
            };
            compile_expression(vars, condition.as_ref())
                .map(|prev| {
                    vec![
                        prev,
                        vec![VMLine::Not, VMLine::IfGoto(label_if_false.to_owned())],
                    ]
                    .concat()
                })
                .and_then(|prev| {
                    compile_statements(vars, if_block.as_ref(), Some(&sub_index)).map(|next| {
                        vec![prev, next, vec![VMLine::Goto(label_end_if.to_owned())]].concat()
                    })
                })
                .and_then(|prev| {
                    if let Some(else_block) = oelse_block.as_ref().as_ref() {
                        compile_statements(vars, else_block, Some(&sub_index.next())).map(|next| {
                            vec![prev, vec![VMLine::Label(label_if_false.to_owned())], next]
                                .concat()
                        })
                    } else {
                        Ok(prev)
                    }
                })
                .map(|prev| vec![prev, vec![VMLine::Label(label_end_if.to_owned())]].concat())
        }
    }
}

pub fn compile_let_left(
    vars: &SubVarTable,
    var_name: &Id,
    var_sub: Option<&Expression>,
) -> Result<Vec<VMLine>, CompileError> {
    // push var
    // if has var_sub, compile_expression, then VMLine::Add,
    let r_sub = if let Some(expr) = var_sub {
        compile_expression(vars, expr)
    } else {
        Ok(Vec::new())
    };
    r_sub.and_then(|prev| {
        vars.var(var_name)
            .map(|var| vec![prev, var.push(), vec![VMLine::Add]].concat())
    })
}
