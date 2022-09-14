use std::slice::Iter;

use crate::parse::*;

use super::{
    expression::{Call, Expression, KeywordConst, Term},
    id::Id,
    keyword::Keyword,
    sym::Sym,
    token::Token,
    xmlformat::{XmlF, XmlFormattable},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let(Id, Box<Option<Expression>>, Box<Expression>),
    If(Box<Expression>, Box<Statements>, Box<Option<Statements>>),
    While(Box<Expression>, Box<Statements>),
    Do(Box<Call>),
    Return(Box<Option<Expression>>),
}
impl Statement {
    pub fn new_let_var(var_name: Id, expr: Expression) -> Self {
        Self::Let(var_name, Box::new(None), Box::new(expr))
    }

    pub fn new_let_var_sub(var_name: Id, sub_expr: Expression, expr: Expression) -> Self {
        Self::Let(var_name, Box::new(Some(sub_expr)), Box::new(expr))
    }

    pub fn new_if(condition: Expression, block: Statements) -> Self {
        Self::If(Box::new(condition), Box::new(block), Box::new(None))
    }

    pub fn new_if_else(condition: Expression, block: Statements, else_block: Statements) -> Self {
        Self::If(
            Box::new(condition),
            Box::new(block),
            Box::new(Some(else_block)),
        )
    }

    pub fn new_while(condition: Expression, block: Statements) -> Self {
        Self::While(Box::new(condition), Box::new(block))
    }

    pub fn new_do(call: Call) -> Self {
        Self::Do(Box::new(call))
    }

    pub fn new_return_void() -> Self {
        Self::Return(Box::new(None))
    }

    pub fn new_return_this() -> Self {
        Self::Return(Box::new(Some(Expression::new(
            Term::KeywordConst(KeywordConst::This),
            Vec::new(),
        ))))
    }

    pub fn new_return(value: Expression) -> Self {
        Self::Return(Box::new(Some(value)))
    }

    pub fn let_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        right(
            Keyword::Let,
            map(
                pair(
                    Token::id,
                    pair(
                        ok(right(
                            Sym::LSquare,
                            left(Expression::parse_into, Sym::RSquare),
                        )),
                        left(right(Sym::Equals, Expression::parse_into), Sym::Semi),
                    ),
                ),
                |(var_name, (var_sub, value))| {
                    Self::Let(var_name, Box::new(var_sub), Box::new(value))
                },
            ),
        )
        .parse(input)
    }

    pub fn if_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        right(
            Keyword::If,
            map(
                pair(
                    right(Sym::LRound, left(Expression::parse_into, Sym::RRound)),
                    pair(
                        right(Sym::LCurly, left(Statements::parse_into, Sym::RCurly)),
                        ok(right(
                            left(Keyword::Else, Sym::LCurly),
                            left(Statements::parse_into, Sym::RCurly),
                        )),
                    ),
                ),
                |(condition, (stmts, estmts))| {
                    Self::If(Box::new(condition), Box::new(stmts), Box::new(estmts))
                },
            ),
        )
        .parse(input)
    }

    pub fn while_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        right(
            Keyword::While,
            map(
                pair(
                    right(Sym::LRound, left(Expression::parse_into, Sym::RRound)),
                    right(Sym::LCurly, left(Statements::parse_into, Sym::RCurly)),
                ),
                |(condition, stmts)| Self::While(Box::new(condition), Box::new(stmts)),
            ),
        )
        .parse(input)
    }

    pub fn do_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        right(
            Keyword::Do,
            left(
                map(Call::parse_into, |call| Self::Do(Box::new(call))),
                Sym::Semi,
            ),
        )
        .parse(input)
    }

    pub fn return_parser(input: &[Token]) -> ParseResult<'_, &[Token], Self> {
        right(
            Keyword::Return,
            left(
                map(ok(Expression::parse_into), |expr| {
                    Self::Return(Box::new(expr))
                }),
                Sym::Semi,
            ),
        )
        .parse(input)
    }

    fn xml_body_let<'a>(
        id: &Id,
        osub: &Option<Expression>,
        value: &Expression,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::Let)?;
        xmlf.write_child(f, id)?;
        if let Some(sub) = osub.as_ref() {
            xmlf.write_child(f, &Sym::LSquare)?;
            xmlf.write_child(f, sub)?;
            xmlf.write_child(f, &Sym::RSquare)?;
        }
        xmlf.write_child(f, &Sym::Equals)?;
        xmlf.write_child(f, value)?;
        xmlf.write_child(f, &Sym::Semi)?;
        Ok(())
    }

    fn xml_body_if<'a>(
        condition: &Expression,
        body: &Statements,
        oelze: &Option<Statements>,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::If)?;
        xmlf.write_child(f, &Sym::LRound)?;
        xmlf.write_child(f, condition)?;
        xmlf.write_child(f, &Sym::RRound)?;
        xmlf.write_child(f, &Sym::LCurly)?;
        xmlf.write_child(f, body)?;
        xmlf.write_child(f, &Sym::RCurly)?;
        if let Some(elze) = oelze.as_ref() {
            xmlf.write_child(f, &Keyword::Else)?;
            xmlf.write_child(f, &Sym::LCurly)?;
            xmlf.write_child(f, elze)?;
            xmlf.write_child(f, &Sym::RCurly)?;
        }
        Ok(())
    }

    fn xml_body_while<'a>(
        condition: &Expression,
        body: &Statements,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::While)?;
        xmlf.write_child(f, &Sym::LRound)?;
        xmlf.write_child(f, condition)?;
        xmlf.write_child(f, &Sym::RRound)?;
        xmlf.write_child(f, &Sym::LCurly)?;
        xmlf.write_child(f, body)?;
        xmlf.write_child(f, &Sym::RCurly)?;
        Ok(())
    }

    fn xml_body_do<'a>(
        call: &Call,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::Do)?;
        if let Some(qual) = call.qualifier() {
            xmlf.write_child(f, qual)?;
            xmlf.write_child(f, &Sym::Dot)?;
        }
        xmlf.write_child(f, call.name())?;
        xmlf.write_child(f, &Sym::LRound)?;
        xmlf.write_child(f, call.expressions())?;
        xmlf.write_child(f, &Sym::RRound)?;
        xmlf.write_child(f, &Sym::Semi)?;
        Ok(())
    }

    fn xml_body_return<'a>(
        ovalue: &Option<Expression>,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::Return)?;
        if let Some(value) = ovalue.as_ref() {
            xmlf.write_child(f, value)?;
        }
        xmlf.write_child(f, &Sym::Semi)?;
        Ok(())
    }
}
impl<'a> Parses<'a> for Statement {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            Self::let_parser,
            or_else(
                Self::if_parser,
                or_else(
                    Self::while_parser,
                    or_else(Self::do_parser, Self::return_parser),
                ),
            ),
        )
        .parse(input)
    }
}

impl XmlFormattable for Statement {
    fn xml_elem(&self) -> &str {
        match self {
            Self::Let(..) => "letStatement",
            Self::If(..) => "ifStatement",
            Self::While(..) => "whileStatement",
            Self::Do(..) => "doStatement",
            Self::Return(..) => "returnStatement",
        }
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Let(id, osub, value) => Self::xml_body_let(id, osub, value, xmlf, f)?,
            Self::If(condition, stmts, elze) => Self::xml_body_if(condition, stmts, elze, xmlf, f)?,
            Self::While(condition, stmts) => Self::xml_body_while(condition, stmts, xmlf, f)?,
            Self::Do(call) => Self::xml_body_do(call, xmlf, f)?,
            Self::Return(value) => Self::xml_body_return(value, xmlf, f)?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Statements {
    stmts: Vec<Statement>,
}
impl Statements {
    pub fn new(stmts: Vec<Statement>) -> Self {
        Self { stmts }
    }

    pub fn append(&self, stmt: Statement) -> Self {
        Self::new(vec![self.stmts.to_vec(), vec![stmt]].concat())
    }

    pub fn iter(&self) -> Iter<'_, Statement> {
        self.stmts.iter()
    }
}

impl<'a> IntoIterator for &'a Statements {
    type Item = &'a Statement;
    type IntoIter = Iter<'a, Statement>;
    fn into_iter(self) -> Self::IntoIter {
        self.stmts.iter()
    }
}

impl<'a> Parses<'a> for Statements {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(range(Statement::parse_into, 0..), Self::new).parse(input)
    }
}
impl From<Vec<Statement>> for Statements {
    fn from(items: Vec<Statement>) -> Self {
        Statements::new(items)
    }
}

impl From<Statement> for Statements {
    fn from(item: Statement) -> Self {
        Statements::new(vec![item])
    }
}

impl XmlFormattable for Statements {
    fn xml_elem(&self) -> &str {
        "statements"
    }

    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        super::xmlformat::XmlBody::Expanded
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        for stmt in self.stmts.iter() {
            xmlf.write_child(f, stmt)?;
        }
        Ok(())
    }
}
#[cfg(test)]
mod tests {
    use crate::jack::{
        common::testutil::{assert_tokens, transform_result},
        expression::{Call, Op},
        statement::Statement,
        token::IntConst,
    };

    use super::*;

    fn token_result(tokens: &[Token]) -> Result<Statement, Option<Token>> {
        let parser = Statement::parse_into;
        transform_result(parser.parse(tokens))
    }

    #[test]
    fn parsing() {
        let cases = vec![(
            r"
            while(index < values.size()) {
                let total = total + values.get(index);
                let index = index + 1;
            }
            ",
            Ok(Statement::new_while(
                (
                    Id::from("index").into(),
                    Op::Lt(Call::new_qual(Id::from("values"), Id::from("size")).into()),
                )
                    .into(),
                vec![
                    Statement::new_let_var(
                        Id::from("total"),
                        (
                            Id::from("total").into(),
                            Op::Plus(
                                Call::new_qual_params(
                                    Id::from("values"),
                                    Id::from("get"),
                                    vec![Id::from("index").into()].into(),
                                )
                                .into(),
                            ),
                        )
                            .into(),
                    ),
                    Statement::new_let_var(
                        Id::from("index"),
                        (Id::from("index").into(), Op::Plus(IntConst::one().into())).into(),
                    ),
                ]
                .into(),
            )),
        )];
        assert_tokens(cases, token_result);
    }
}
