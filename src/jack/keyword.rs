use std::fmt::Display;

use crate::parse::*;

use super::token::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Class,
    Constructor,
    Function,
    Method,
    Field,
    Static,
    Var,
    Int,
    Char,
    Boolean,
    Void,
    True,
    False,
    Null,
    This,
    Let,
    Do,
    If,
    Else,
    While,
    Return,
}
impl Keyword {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Class => "class",
            Self::Constructor => "constructor",
            Self::Function => "function",
            Self::Method => "method",
            Self::Field => "field",
            Self::Static => "static",
            Self::Var => "var",
            Self::Int => "int",
            Self::Char => "char",
            Self::Boolean => "boolean",
            Self::Void => "void",
            Self::True => "true",
            Self::False => "false",
            Self::Null => "null",
            Self::This => "this",
            Self::Let => "let",
            Self::Do => "do",
            Self::If => "if",
            Self::Else => "else",
            Self::While => "while",
            Self::Return => "return",
        }
    }

    pub fn all() -> Vec<Self> {
        use Keyword::*;
        vec![
            Class,
            Constructor,
            Function,
            Method,
            Field,
            Static,
            Var,
            Int,
            Char,
            Boolean,
            Void,
            True,
            False,
            Null,
            This,
            Let,
            Do,
            If,
            Else,
            While,
            Return,
        ]
    }

    pub fn is_reserved(token: &str) -> bool {
        Self::all().iter().any(|k| k.as_str() == token)
    }

    pub fn as_token(self) -> Token {
        Token::Keyword(self)
    }
}

impl<'a> Parser<'a, &'a str, Keyword> for Keyword {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'a str, Keyword> {
        map(match_literal(self.as_str()), |()| *self).parse(input)
    }
}

impl<'a> Parser<'a, &'a [Token], Keyword> for Keyword {
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, &'a [Token], Keyword> {
        match input.split_first() {
            Some((Token::Keyword(value), rem)) if value == self => Ok((rem, *self)),
            _ => Err(input),
        }
    }
}

impl<'a> Parses<'a> for Keyword {
    type Input = &'a str;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        let init_parser: Box<dyn Parser<'a, &'a str, Self>> = Box::new(none());

        Self::all()
            .iter()
            .fold(init_parser, |acc, keyword| -> Box<dyn Parser<&str, Self>> {
                Box::new(or_else(
                    move |input| keyword.parse(input),
                    move |input| acc.parse(input),
                ))
            })
            .parse(input)
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
#[cfg(test)]
mod tests {

    use super::*;
    #[test]
    fn keywords() {
        assert_eq!(
            Ok(("", Keyword::Boolean)),
            Keyword::Boolean.parse("boolean")
        );
    }
}
