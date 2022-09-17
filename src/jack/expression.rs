use std::slice::Iter;

use crate::{parse::*, vm::VMLine};

use super::{
    id::Id,
    keyword::Keyword,
    sym::Sym,
    token::{IntConst, StringConst, Token},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Plus(Term),
    Minus(Term),
    Splat(Term),
    Div(Term),
    And(Term),
    Or(Term),
    Eq(Term),
    Lt(Term),
    Gt(Term),
}
impl Op {
    pub fn as_sym(&self) -> Sym {
        match self {
            Self::Plus(_) => Sym::Plus,
            Self::Minus(_) => Sym::Minus,
            Self::Splat(_) => Sym::Splat,
            Self::Div(_) => Sym::Slash,
            Self::And(_) => Sym::Amp,
            Self::Or(_) => Sym::Pipe,
            Self::Eq(_) => Sym::Equals,
            Self::Lt(_) => Sym::LAngle,
            Self::Gt(_) => Sym::RAngle,
        }
    }

    pub fn operate(&self) -> Vec<VMLine> {
        match self {
            Self::Plus(_) => vec![VMLine::Add],
            Self::Minus(_) => vec![VMLine::Sub],
            Self::Splat(_) => vec![VMLine::Call("Math.multiply".to_owned(), 2)],
            Self::Div(_) => vec![VMLine::Call("Math.divide".to_owned(), 2)],
            Self::And(_) => vec![VMLine::And],
            Self::Or(_) => vec![VMLine::Or],
            Self::Eq(_) => vec![VMLine::Eq],
            Self::Lt(_) => vec![VMLine::Lt],
            Self::Gt(_) => vec![VMLine::Gt],
        }
    }

    pub fn term(&self) -> &Term {
        match self {
            Self::Plus(term) => term,
            Self::Minus(term) => term,
            Self::Splat(term) => term,
            Self::Div(term) => term,
            Self::And(term) => term,
            Self::Or(term) => term,
            Self::Eq(term) => term,
            Self::Lt(term) => term,
            Self::Gt(term) => term,
        }
    }
}

impl<'a> Parses<'a> for Op {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            map(right(Sym::Plus, Term::parse_into), Self::Plus),
            or_else(
                map(right(Sym::Minus, Term::parse_into), Self::Minus),
                or_else(
                    map(right(Sym::Splat, Term::parse_into), Self::Splat),
                    or_else(
                        map(right(Sym::Slash, Term::parse_into), Self::Div),
                        or_else(
                            map(right(Sym::Amp, Term::parse_into), Self::And),
                            or_else(
                                map(right(Sym::Pipe, Term::parse_into), Self::Or),
                                or_else(
                                    map(right(Sym::Equals, Term::parse_into), Self::Eq),
                                    or_else(
                                        map(right(Sym::LAngle, Term::parse_into), Self::Lt),
                                        map(right(Sym::RAngle, Term::parse_into), Self::Gt),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg(Term),
    Tilde(Term),
}

impl UnaryOp {
    pub fn as_sym(&self) -> Sym {
        match self {
            Self::Neg(_) => Sym::Minus,
            Self::Tilde(_) => Sym::Tilde,
        }
    }

    pub fn operate(&self) -> VMLine {
        match self {
            Self::Neg(..) => VMLine::Neg,
            Self::Tilde(..) => VMLine::Not,
        }
    }

    pub fn term(&self) -> &Term {
        match self {
            Self::Neg(term) => term,
            Self::Tilde(term) => term,
        }
    }
}

impl<'a> Parses<'a> for UnaryOp {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            map(right(Sym::Minus, Term::parse_into), Self::Neg),
            map(right(Sym::Tilde, Term::parse_into), Self::Tilde),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    IntConst(IntConst),
    StringConst(StringConst),
    KeywordConst(KeywordConst),
    VarName(Id),
    VarSub(Id, Box<Expression>),
    Expr(Box<Expression>),
    UnaryOp(Box<UnaryOp>),
    SubroutineCall(Box<Call>),
}
impl Term {
    pub fn int_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        map(Token::int, Self::IntConst).parse(input)
    }

    pub fn string_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        map(Token::str, Self::StringConst).parse(input)
    }

    pub fn keyword_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        map(KeywordConst::parse_into, Self::KeywordConst).parse(input)
    }

    pub fn var_name_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        map(Token::id, Self::VarName).parse(input)
    }

    pub fn var_sub_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        map(
            pair(
                Token::id,
                right(Sym::LSquare, left(Expression::parse_into, Sym::RSquare)),
            ),
            |(id, expr)| Self::VarSub(id, Box::new(expr)),
        )
        .parse(input)
    }

    pub fn expr_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        map(
            right(Sym::LRound, left(Expression::parse_into, Sym::RRound)),
            |expr| Self::Expr(Box::new(expr)),
        )
        .parse(input)
    }

    pub fn unary_op_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        map(UnaryOp::parse_into, |value| Self::UnaryOp(Box::new(value))).parse(input)
    }

    pub fn subroutine_call_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        map(Call::parse_into, |value| {
            Self::SubroutineCall(Box::new(value))
        })
        .parse(input)
    }
}

impl<'a> Parses<'a> for Term {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            Self::expr_parser,
            or_else(
                Self::string_parser,
                or_else(
                    Self::unary_op_parser,
                    or_else(
                        Self::int_parser,
                        or_else(
                            Self::subroutine_call_parser,
                            or_else(
                                Self::var_sub_parser,
                                or_else(Self::var_name_parser, Self::keyword_parser),
                            ),
                        ),
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

impl From<KeywordConst> for Term {
    fn from(item: KeywordConst) -> Self {
        Term::KeywordConst(item)
    }
}

impl From<Id> for Term {
    fn from(item: Id) -> Self {
        Term::VarName(item)
    }
}

impl From<IntConst> for Term {
    fn from(item: IntConst) -> Self {
        Term::IntConst(item)
    }
}

impl From<StringConst> for Term {
    fn from(item: StringConst) -> Self {
        Term::StringConst(item)
    }
}

impl From<UnaryOp> for Term {
    fn from(item: UnaryOp) -> Self {
        Term::UnaryOp(Box::new(item))
    }
}

impl From<Call> for Term {
    fn from(item: Call) -> Self {
        Term::SubroutineCall(Box::new(item))
    }
}

impl<T> From<(Id, T)> for Term
where
    T: Into<Expression>,
{
    fn from(item: (Id, T)) -> Self {
        let (name, sub) = item;
        Term::VarSub(name, Box::new(sub.into()))
    }
}

impl From<Expression> for Term {
    fn from(item: Expression) -> Self {
        Term::Expr(Box::new(item))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
    term: Term,
    ops: Vec<Op>,
}
impl Expression {
    pub fn new(term: Term, ops: Vec<Op>) -> Self {
        Self { term, ops }
    }

    pub fn term(&self) -> &Term {
        &self.term
    }

    pub fn is_single_term(&self) -> bool {
        self.ops.is_empty()
    }

    pub fn iter(&self) -> Iter<'_, Op> {
        self.ops.iter()
    }
}

impl<'a> Parses<'a> for Expression {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            pair(Term::parse_into, range(Op::parse_into, 0..)),
            |(term, ops)| Self::new(term, ops),
        )
        .parse(input)
    }
}

impl From<Term> for Expression {
    fn from(item: Term) -> Self {
        Expression::new(item, Vec::new())
    }
}

impl From<(Term, Op)> for Expression {
    fn from(item: (Term, Op)) -> Self {
        let (t, op) = item;
        Expression::new(t, vec![op])
    }
}

impl From<(Term, Vec<Op>)> for Expression {
    fn from(item: (Term, Vec<Op>)) -> Self {
        let (t, ops) = item;
        Expression::new(t, ops)
    }
}

impl From<(Term, Op, Op)> for Expression {
    fn from(item: (Term, Op, Op)) -> Self {
        let (t, op0, op1) = item;
        Expression::new(t, vec![op0, op1])
    }
}

impl From<(Term, Op, Op, Op)> for Expression {
    fn from(item: (Term, Op, Op, Op)) -> Self {
        let (t, op0, op1, op2) = item;
        Expression::new(t, vec![op0, op1, op2])
    }
}

impl From<Id> for Expression {
    fn from(item: Id) -> Self {
        Expression::new(item.into(), Vec::new())
    }
}

impl From<IntConst> for Expression {
    fn from(item: IntConst) -> Self {
        Expression::new(item.into(), Vec::new())
    }
}

impl From<StringConst> for Expression {
    fn from(item: StringConst) -> Self {
        Expression::new(item.into(), Vec::new())
    }
}

impl From<KeywordConst> for Expression {
    fn from(item: KeywordConst) -> Self {
        Expression::new(item.into(), Vec::new())
    }
}

impl From<UnaryOp> for Expression {
    fn from(item: UnaryOp) -> Self {
        Expression::new(item.into(), Vec::new())
    }
}

impl From<Call> for Expression {
    fn from(item: Call) -> Self {
        Expression::new(item.into(), Vec::new())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionList {
    exprs: Vec<Expression>,
}
impl ExpressionList {
    pub fn new(exprs: Vec<Expression>) -> Self {
        Self { exprs }
    }
    pub fn len(&self) -> Result<i16, String> {
        self.exprs
            .len()
            .try_into()
            .map_err(|err| format!("too many expressions in call {:?}", err))
    }
    pub fn iter(&self) -> Iter<'_, Expression> {
        self.exprs.iter()
    }
}
impl<'a> Parses<'a> for ExpressionList {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            ok(map(
                pair(
                    Expression::parse_into,
                    range(right(Sym::Comma, Expression::parse_into), 0..),
                ),
                |(first, exprs)| vec![vec![first], exprs].concat(),
            )),
            |exprs| Self::new(exprs.unwrap_or_default()),
        )
        .parse(input)
    }
}

impl From<Vec<Expression>> for ExpressionList {
    fn from(items: Vec<Expression>) -> Self {
        ExpressionList::new(items)
    }
}

impl From<Expression> for ExpressionList {
    fn from(item: Expression) -> Self {
        ExpressionList::new(vec![item])
    }
}

impl<'a> IntoIterator for &'a ExpressionList {
    type Item = &'a Expression;
    type IntoIter = Iter<'a, Expression>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Call {
    qualifier: Option<Id>,
    name: Id,
    exprs: ExpressionList,
}
impl Call {
    pub fn new(qualifier: Option<Id>, name: Id, exprs: ExpressionList) -> Self {
        Self {
            qualifier,
            name,
            exprs,
        }
    }

    pub fn new_simp(name: Id) -> Self {
        Self::new(None, name, Vec::new().into())
    }

    pub fn new_qual(qualifier: Id, name: Id) -> Self {
        Self::new(Some(qualifier), name, Vec::new().into())
    }

    pub fn new_simp_params(name: Id, exprs: ExpressionList) -> Self {
        Self::new(None, name, exprs)
    }

    pub fn new_qual_params(qualifier: Id, name: Id, exprs: ExpressionList) -> Self {
        Self::new(Some(qualifier), name, exprs)
    }

    pub fn qualifier(&self) -> Option<&Id> {
        self.qualifier.as_ref()
    }

    pub fn name(&self) -> &Id {
        &self.name
    }

    pub fn expressions(&self) -> &ExpressionList {
        &self.exprs
    }
}

impl<'a> Parses<'a> for Call {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            pair(
                ok(left(Token::id, Sym::Dot)),
                pair(
                    Token::id,
                    right(Sym::LRound, left(ExpressionList::parse_into, Sym::RRound)),
                ),
            ),
            |(qualifier, (name, exprs))| Self::new(qualifier, name, exprs),
        )
        .parse(input)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KeywordConst {
    True,
    False,
    Null,
    This,
}
impl KeywordConst {
    pub fn as_keyword(&self) -> Keyword {
        match self {
            Self::True => Keyword::True,
            Self::False => Keyword::False,
            Self::Null => Keyword::Null,
            Self::This => Keyword::This,
        }
    }
}
impl<'a> Parses<'a> for KeywordConst {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            map(Keyword::True, |_| Self::True),
            or_else(
                map(Keyword::False, |_| Self::False),
                or_else(
                    map(Keyword::Null, |_| Self::Null),
                    map(Keyword::This, |_| Self::This),
                ),
            ),
        )
        .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jack::common::testutil::{assert_tokens, transform_result};

    fn token_result(tokens: &[Token]) -> Result<Expression, Option<Token>> {
        let parser = Expression::parse_into;
        transform_result(parser.parse(tokens))
    }

    fn token_result_to_term(tokens: &[Token]) -> Result<Term, Option<Token>> {
        let parser = Term::parse_into;
        transform_result(parser.parse(tokens))
    }

    fn token_result_to_unary_op(tokens: &[Token]) -> Result<UnaryOp, Option<Token>> {
        let parser = UnaryOp::parse_into;
        transform_result(parser.parse(tokens))
    }

    #[test]
    fn term_int_parser() {
        let cases = vec![(
            "12345",
            Ok(Term::IntConst(IntConst::new(12345).expect("valid"))),
        )];
        assert_tokens(cases, token_result_to_term);
    }

    #[test]
    fn term_string_parser() {
        let cases = vec![(
            "\"a string\"",
            Ok(Term::StringConst(StringConst::new("a string".to_owned()))),
        )];
        assert_tokens(cases, token_result_to_term);
    }

    #[test]
    fn term_expr_parser() {
        let cases = vec![
            (
                "(\"a string\")",
                Ok(Term::Expr(Box::new(Expression::new(
                    Term::StringConst(StringConst::new("a string".to_owned())),
                    Vec::new(),
                )))),
            ),
            (
                "(true)",
                Ok(Term::Expr(Box::new(Expression::new(
                    Term::KeywordConst(KeywordConst::True),
                    Vec::new(),
                )))),
            ),
            (
                "((false))",
                Ok(Term::Expr(Box::new(Expression::new(
                    Term::Expr(Box::new(Expression::new(
                        Term::KeywordConst(KeywordConst::False),
                        Vec::new(),
                    ))),
                    Vec::new(),
                )))),
            ),
        ];
        assert_tokens(cases, token_result_to_term);
    }

    #[test]
    fn term_call_parser() {
        let cases = vec![(
            "a_function()",
            Ok(Term::SubroutineCall(Box::new(Call::new(
                None,
                Id::new("a_function".to_owned()),
                ExpressionList::new(Vec::new()),
            )))),
        )];
        assert_tokens(cases, token_result_to_term);
    }

    #[test]
    fn unary_ops() {
        let cases = vec![(
            "-12345",
            Ok(UnaryOp::Neg(Term::IntConst(
                IntConst::new(12345).expect("valid"),
            ))),
        )];
        assert_tokens(cases, token_result_to_unary_op);
    }

    #[test]
    fn parsing() {
        let cases = vec![(
            r"index < values.size()",
            Ok((
                Id::from("index").into(),
                Op::Lt(Call::new_qual(Id::from("values"), Id::from("size")).into()),
            )
                .into()),
        )];
        assert_tokens(cases, token_result);
    }
}
