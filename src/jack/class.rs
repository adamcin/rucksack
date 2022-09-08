use std::fmt::Display;

use crate::parse::*;

use super::id::Id;
use super::keyword::Keyword;
use super::subroutine::*;
use super::sym::Sym;
use super::token::Token;
use super::typea::Type;
use super::xmlformat::{XmlBody, XmlF, XmlFormattable};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassVarKind {
    Static,
    Field,
}

impl ClassVarKind {
    pub fn as_keyword(&self) -> Keyword {
        match self {
            Self::Static => Keyword::Static,
            Self::Field => Keyword::Field,
        }
    }
}

impl<'a> Parses<'a> for ClassVarKind {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            map(Keyword::Static, |_| Self::Static),
            map(Keyword::Field, |_| Self::Field),
        )
        .parse(input)
    }
}
impl XmlFormattable for ClassVarKind {
    fn xml_elem<'a>(&'a self) -> &str {
        "keyword"
    }

    fn xml_body_type(&self) -> XmlBody {
        XmlBody::Inline
    }

    fn xml_inline_body(&self) -> String {
        self.as_keyword().xml_inline_body()
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassVarDec {
    var_kind: ClassVarKind,
    var_type: Type,
    var_name: Id,
    var_names: Vec<Id>,
}
impl ClassVarDec {
    pub fn new(var_kind: ClassVarKind, var_type: Type, var_name: Id, var_names: Vec<Id>) -> Self {
        Self {
            var_kind,
            var_type,
            var_name,
            var_names,
        }
    }

    pub fn var_kind<'a>(&'a self) -> &'a ClassVarKind {
        &self.var_kind
    }

    pub fn var_type<'a>(&'a self) -> &'a Type {
        &self.var_type
    }

    pub fn var_names<'a>(&'a self) -> Vec<&'a Id> {
        vec![vec![&self.var_name], self.var_names.iter().collect()].concat()
    }
}

impl<'a> Parses<'a> for ClassVarDec {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            pair(
                move |input| ClassVarKind::parse_into(input),
                pair(
                    move |input| Type::parse_into(input),
                    pair(
                        move |input| Token::id(input),
                        left(
                            range(right(Sym::Comma, move |input| Token::id(input)), 0..),
                            Sym::Semi,
                        ),
                    ),
                ),
            ),
            |(var_kind, (var_type, (var_name, var_names)))| {
                Self::new(var_kind, var_type, var_name, var_names)
            },
        )
        .parse(input)
    }
}

impl XmlFormattable for ClassVarDec {
    fn xml_elem<'a>(&'a self) -> &str {
        "classVarDec"
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &self.var_kind)?;
        xmlf.write_child(f, &self.var_type)?;
        xmlf.write_child(f, &self.var_name)?;
        for name in self.var_names.iter() {
            xmlf.write_child(f, &Sym::Comma)?;
            xmlf.write_child(f, name)?;
        }
        xmlf.write_child(f, &Sym::Semi)?;
        Ok(())
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    name: Id,
    vars: Vec<ClassVarDec>,
    subs: Vec<SubroutineDec>,
}

impl Class {
    pub fn new(name: Id, vars: Vec<ClassVarDec>, subs: Vec<SubroutineDec>) -> Self {
        Self { name, vars, subs }
    }

    pub fn name<'a>(&'a self) -> &'a str {
        self.name.as_str()
    }

    pub fn vars<'a>(&'a self) -> &'a [ClassVarDec] {
        self.vars.as_slice()
    }

    pub fn subs<'a>(&'a self) -> &'a [SubroutineDec] {
        self.subs.as_slice()
    }
}
impl<'a> Parses<'a> for Class {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        right(
            Keyword::Class,
            map(
                pair(
                    move |input| Token::id(input),
                    right(
                        Sym::LCurly,
                        left(
                            pair(
                                range(move |input| ClassVarDec::parse_into(input), 0..),
                                range(move |input| SubroutineDec::parse_into(input), 0..),
                            ),
                            Sym::RCurly,
                        ),
                    ),
                ),
                |(id, (vars, subs))| Self::new(id, vars, subs),
            ),
        )
        .parse(input)
    }
}

impl XmlFormattable for Class {
    fn xml_elem<'a>(&'a self) -> &str {
        "class"
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::Class)?;
        xmlf.write_child(f, &self.name)?;
        xmlf.write_child(f, &Sym::LCurly)?;
        for var in self.vars.iter() {
            xmlf.write_child(f, var)?;
        }
        for dec in self.subs.iter() {
            xmlf.write_child(f, dec)?;
        }
        xmlf.write_child(f, &Sym::RCurly)?;
        Ok(())
    }
}
impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", &Keyword::Class, self.name, &Sym::LCurly)?;
        write!(f, "{}", &Sym::RCurly)?;
        Ok(())
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::jack::{
        common::testutil::{assert_tokens, transform_result},
        expression::{Call, Op},
        statement::Statement,
        token::{IntConst, StringConst},
    };

    fn token_result(tokens: &[Token]) -> Result<Class, Option<Token>> {
        let parser = Class::parse_into;
        transform_result(parser.parse(tokens))
    }

    #[test]
    fn test_simple() {
        let cases = vec![
            (
                "class MyClass {}",
                Ok(Class::new(
                    Id::new("MyClass".to_owned()),
                    Vec::new(),
                    Vec::new(),
                )),
            ),
            (
                "class MyClass {} extra",
                Ok(Class::new(
                    Id::new("MyClass".to_owned()),
                    Vec::new(),
                    Vec::new(),
                )),
            ),
            (
                r"class MyClass {
                    static int count// some inline comment
                    /* 
                    some multiline comment;
                    */
                    ;

                } extra",
                Ok(Class::new(
                    "MyClass".into(),
                    vec![ClassVarDec::new(
                        ClassVarKind::Static,
                        Type::Int,
                        "count".into(),
                        Vec::new(),
                    )],
                    Vec::new(),
                )),
            ),
            (
                r#"
                // This file is part of www.nand2tetris.org
                // and the book "The Elements of Computing Systems"
                // by Nisan and Schocken, MIT Press.
                // File name: projects/09/Average/Main.jack

                // Inputs some numbers and computes their average
                class Main {
                    function void main() {
                        var Array a; 
                        var int length;
                        var int i, sum;

                        let length = Keyboard.readInt("How many numbers? ");
                        let a = Array.new(length); // constructs the array
                        
                        let i = 0;
                        while (i < length) {
                            let a[i] = Keyboard.readInt("Enter a number: ");
                            let sum = sum + a[i];
                            let i = i + 1;
                        }

                        do Output.printString("The average is ");
                        do Output.printInt(sum / length);
                        return;
                    }
                }
                "#,
                Ok(Class::new(
                    "Main".into(),
                    Vec::new(),
                    vec![SubroutineDec::new(
                        SubroutineKind::Function,
                        ReturnType::Void,
                        Id::from("main"),
                        Vec::new().into(),
                        SubroutineBody::new(
                            vec![
                                (Id::from("Array").into(), Id::from("a")).into(),
                                (Type::Int, Id::from("length")).into(),
                                (Type::Int, Id::from("i"), Id::from("sum")).into(),
                            ],
                            vec![
                                Statement::new_let_var(
                                    Id::from("length"),
                                    Call::new_qual_params(
                                        Id::from("Keyboard"),
                                        Id::from("readInt"),
                                        vec![StringConst::from("How many numbers? ").into()].into(),
                                    )
                                    .into(),
                                ),
                                Statement::new_let_var(
                                    Id::from("a"),
                                    Call::new_qual_params(
                                        Id::from("Array"),
                                        Id::from("new"),
                                        vec![Id::from("length").into()].into(),
                                    )
                                    .into(),
                                ),
                                Statement::new_let_var(Id::from("i"), IntConst::zero().into()),
                                Statement::new_while(
                                    (Id::from("i").into(), Op::Lt(Id::from("length").into()))
                                        .into(),
                                    vec![
                                        Statement::new_let_var_sub(
                                            Id::from("a"),
                                            Id::from("i").into(),
                                            Call::new_qual_params(
                                                Id::from("Keyboard"),
                                                Id::from("readInt"),
                                                vec![StringConst::from("Enter a number: ").into()]
                                                    .into(),
                                            )
                                            .into(),
                                        ),
                                        Statement::new_let_var(
                                            Id::from("sum"),
                                            (
                                                Id::from("sum").into(),
                                                Op::Plus((Id::from("a"), Id::from("i")).into()),
                                            )
                                                .into(),
                                        ),
                                        Statement::new_let_var(
                                            Id::from("i"),
                                            (
                                                Id::from("i").into(),
                                                Op::Plus(IntConst::one().into()),
                                            )
                                                .into(),
                                        ),
                                    ]
                                    .into(),
                                ),
                                Statement::new_do(Call::new_qual_params(
                                    Id::from("Output"),
                                    Id::from("printString"),
                                    vec![StringConst::from("The average is ").into()].into(),
                                )),
                                Statement::new_do(Call::new_qual_params(
                                    Id::from("Output"),
                                    Id::from("printInt"),
                                    vec![(
                                        Id::from("sum").into(),
                                        Op::Div(Id::from("length").into()),
                                    )
                                        .into()]
                                    .into(),
                                )),
                                Statement::new_return_void(),
                            ]
                            .into(),
                        ),
                    )],
                )),
            ),
        ];

        assert_tokens(cases, token_result);
    }
}
