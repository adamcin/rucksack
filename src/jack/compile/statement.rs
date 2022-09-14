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
struct StatementIndex {
    idx_if: usize,
    idx_while: usize,
}

impl StatementIndex {
    fn new(idx_if: usize, idx_while: usize) -> Self {
        Self { idx_if, idx_while }
    }
    fn init() -> Self {
        Self {
            idx_if: 0,
            idx_while: 0,
        }
    }

    fn on_if(&self) -> Self {
        Self::new(self.idx_if + 1, self.idx_while)
    }

    fn on_while(&self) -> Self {
        Self::new(self.idx_if, self.idx_while + 1)
    }
}

pub fn compile_statements(
    vars: &SubVarTable,
    stmts: &Statements,
) -> Result<Vec<VMLine>, CompileError> {
    compile_statements_internal(vars, stmts, StatementIndex::init()).map(|(_, lines)| lines)
}

fn compile_statements_internal(
    vars: &SubVarTable,
    stmts: &Statements,
    init_idx: StatementIndex,
) -> Result<(StatementIndex, Vec<VMLine>), CompileError> {
    let init = (init_idx, Vec::new());
    stmts.iter().fold(Ok(init), |acc, stmt| {
        acc.and_then(|(index, prev)| {
            compile_statement(&index, vars, stmt)
                .map(|(next_idx, next)| (next_idx, vec![prev, next].concat()))
        })
    })
}

fn compile_statement(
    index: &StatementIndex,
    vars: &SubVarTable,
    stmt: &Statement,
) -> Result<(StatementIndex, Vec<VMLine>), CompileError> {
    match stmt {
        Statement::Return(oexpr) => oexpr
            .as_ref()
            .as_ref()
            .map(|expr| compile_expression(vars, expr))
            .unwrap_or_else(|| Ok(vec![VMLine::Push(Segment::Constant, 0)]))
            .map(|lines| (index.clone(), vec![lines, vec![VMLine::Return]].concat())),
        Statement::Do(call) => compile_term_call(vars, call.as_ref()).map(|lines| {
            (
                index.clone(),
                vec![lines, vec![VMLine::Pop(Segment::Temp, 0)]].concat(),
            )
        }),
        Statement::Let(var_name, ovar_sub, oexpr) if ovar_sub.is_none() => {
            compile_expression(vars, oexpr.as_ref()).and_then(|prev| {
                vars.var(var_name)
                    .map(|var| (index.clone(), vec![prev, var.pop()].concat()))
            })
        }
        Statement::Let(var, ovar_sub, oexpr) => {
            compile_let_left(vars, var, ovar_sub.as_ref().as_ref()).and_then(|left| {
                compile_expression(vars, oexpr.as_ref()).map(|right| {
                    (
                        index.clone(),
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
                        .concat(),
                    )
                })
            })
        }
        Statement::While(condition, block) => compile_expression(vars, condition.as_ref())
            .and_then(|prev| {
                compile_statements_internal(vars, block.as_ref(), index.on_while()).map(
                    |(next_idx, next)| {
                        let label_while = format!("WHILE_EXP{}", index.idx_while);
                        let label_end_while = format!("WHILE_END{}", index.idx_while);
                        (
                            next_idx,
                            vec![
                                vec![VMLine::Label(label_while.to_owned())],
                                prev,
                                vec![VMLine::Not, VMLine::IfGoto(label_end_while.to_owned())],
                                next,
                                vec![VMLine::Goto(label_while), VMLine::Label(label_end_while)],
                            ]
                            .concat(),
                        )
                    },
                )
            }),
        Statement::If(condition, if_block, oelse_block) => {
            let label_if_true = format!("IF_TRUE{}", index.idx_if);
            let label_if_false = format!("IF_FALSE{}", index.idx_if);
            let label_if_end = if oelse_block.is_some() {
                format!("IF_END{}", index.idx_if)
            } else {
                label_if_false.to_owned()
            };
            compile_expression(vars, condition.as_ref())
                .map(|prev| {
                    vec![
                        prev,
                        vec![
                            VMLine::IfGoto(label_if_true.to_owned()),
                            VMLine::Goto(label_if_false.to_owned()),
                        ],
                    ]
                    .concat()
                })
                .and_then(|prev| {
                    compile_statements_internal(vars, if_block.as_ref(), index.on_if()).map(
                        |(next_idx, next)| {
                            (
                                next_idx,
                                vec![prev, vec![VMLine::Label(label_if_true.to_owned())], next]
                                    .concat(),
                            )
                        },
                    )
                })
                .and_then(|(prev_idx, prev)| {
                    if let Some(else_block) = oelse_block.as_ref().as_ref() {
                        compile_statements_internal(vars, else_block, prev_idx).map(
                            |(next_idx, next)| {
                                (
                                    next_idx,
                                    vec![
                                        prev,
                                        vec![
                                            VMLine::Goto(label_if_end.to_owned()),
                                            VMLine::Label(label_if_false.to_owned()),
                                        ],
                                        next,
                                    ]
                                    .concat(),
                                )
                            },
                        )
                    } else {
                        Ok((prev_idx, prev))
                    }
                })
                .map(|(prev_idx, prev)| {
                    (
                        prev_idx,
                        vec![prev, vec![VMLine::Label(label_if_end.to_owned())]].concat(),
                    )
                })
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
