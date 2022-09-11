use crate::{
    jack::{
        expression::{Call, Expression, KeywordConst, Op, Term, UnaryOp},
        id::Id,
        token::{IntConst, StringConst},
        typea::Type,
    },
    vm::{Segment, VMLine},
};

use super::{vartable::SubVarTable, CompileError};

pub fn compile_expression(
    vars: &SubVarTable,
    expr: &Expression,
) -> Result<Vec<VMLine>, CompileError> {
    let init = compile_term(vars, expr.term());
    expr.iter().fold(init, |acc, op| {
        acc.and_then(|prev| compile_op(vars, op).map(|next| vec![prev, next].concat()))
    })
}

pub fn compile_op(vars: &SubVarTable, op: &Op) -> Result<Vec<VMLine>, CompileError> {
    compile_term(vars, op.term()).map(|prev| vec![prev, op.operate()].concat())
}

pub fn compile_term(vars: &SubVarTable, term: &Term) -> Result<Vec<VMLine>, CompileError> {
    match term {
        Term::IntConst(value) => Ok(vec![VMLine::Push(Segment::Constant, value.value())]),
        Term::StringConst(value) => compile_term_string_const(vars, value),
        Term::KeywordConst(value) => compile_term_keyword_const(vars, value),
        Term::UnaryOp(unary_op) => compile_term_unary_op(vars, unary_op.as_ref()),
        Term::Expr(expr) => compile_expression(vars, expr.as_ref()),
        Term::SubroutineCall(call) => compile_term_call(vars, call.as_ref()),
        Term::VarSub(var_name, var_sub) => compile_term_var_sub(vars, var_name, var_sub.as_ref()),
        Term::VarName(var_name) => compile_term_var_name(vars, var_name),
    }
}

pub fn compile_term_string_const(
    vars: &SubVarTable,
    value: &StringConst,
) -> Result<Vec<VMLine>, CompileError> {
    let init = compile_term_call(
        vars,
        &Call::new(
            Some(Id::from("String")),
            Id::from("new"),
            vec![Expression::from(Term::IntConst(IntConst::zero()))].into(),
        ),
    )
    .map(|prev| vec![prev, vec![VMLine::Pop(Segment::Pointer, 1)]].concat());
    value.chars().iter().fold(init, |acc, c| {
        acc.and_then(|prev| {
            compile_term_call(
                vars,
                &Call::new(
                    Some(Id::from("String")),
                    Id::from("appendChar"),
                    vec![Expression::from(Term::IntConst(IntConst::new(2)?))].into(),
                ),
            )
            .map(|next| {
                vec![
                    prev,
                    vec![
                        VMLine::Push(Segment::Pointer, 1),
                        VMLine::Push(Segment::Constant, *c),
                    ],
                    next,
                ]
                .concat()
            })
        })
    })
}

pub fn compile_term_keyword_const(
    vars: &SubVarTable,
    value: &KeywordConst,
) -> Result<Vec<VMLine>, CompileError> {
    match value {
        KeywordConst::This => vars.this().map(|var| var.push()),
        KeywordConst::Null => Ok(vec![VMLine::Push(Segment::Constant, 0)]),
        KeywordConst::True => Ok(vec![VMLine::Push(Segment::Constant, -1)]),
        KeywordConst::False => Ok(vec![VMLine::Push(Segment::Constant, 0)]),
    }
}

pub fn compile_term_unary_op(
    vars: &SubVarTable,
    unary_op: &UnaryOp,
) -> Result<Vec<VMLine>, CompileError> {
    compile_term(vars, unary_op.term()).map(|prev| vec![prev, vec![unary_op.operate()]].concat())
}

pub fn compile_term_call(vars: &SubVarTable, call: &Call) -> Result<Vec<VMLine>, CompileError> {
    let (class_name, init, n_args) = if vars.is_method(call.qualifier(), call.name()) {
        if let Some(qual) = call.qualifier() {
            // push var, then perform call
            vars.var(qual).and_then(|var| {
                let r_class_name = match var.var_type() {
                    Type::ClassName(class_name) => Ok(class_name.clone()),
                    _ => Err(format!("var {:?} does not reference an object", var)),
                };
                r_class_name.and_then(|class_name| {
                    Ok((class_name, var.push(), call.expressions().len()? + 1))
                })
            })
        } else {
            // push this, then perform call
            vars.this()
                .and_then(|var| Ok((vars.this_type(), var.push(), call.expressions().len()? + 1)))
        }?
    } else if let Some(qual) = call.qualifier() {
        (qual.clone(), Vec::new(), call.expressions().len()?)
    } else {
        (vars.this_type(), Vec::new(), call.expressions().len()?)
    };
    call.expressions()
        .into_iter()
        .fold(Ok(init), |acc, expr| {
            acc.and_then(|prev| {
                compile_expression(vars, expr).map(|next| vec![prev, next].concat())
            })
        })
        .map(|prev| {
            vec![
                prev,
                vec![VMLine::Call(
                    format!("{}.{}", class_name.as_str(), call.name().as_str()),
                    n_args,
                )],
            ]
            .concat()
        })
}

pub fn compile_term_var_name(
    vars: &SubVarTable,
    var_name: &Id,
) -> Result<Vec<VMLine>, CompileError> {
    vars.var(var_name).map(|var| var.push())
}

pub fn compile_term_var_sub(
    vars: &SubVarTable,
    var_name: &Id,
    var_sub: &Expression,
) -> Result<Vec<VMLine>, CompileError> {
    compile_term_var_name(vars, var_name).and_then(|prev| {
        compile_expression(vars, var_sub).map(|next| {
            vec![
                prev,
                next,
                vec![
                    VMLine::Add,
                    VMLine::Pop(Segment::Pointer, 1),
                    VMLine::Push(Segment::That, 0),
                ],
            ]
            .concat()
        })
    })
}
