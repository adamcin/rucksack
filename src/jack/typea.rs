use std::fmt::Display;

use crate::parse::*;

use super::{id::Id, keyword::Keyword, token::Token, xmlformat::XmlFormattable};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Char,
    Boolean,
    ClassName(Id),
}
impl Type {
    pub fn copy(&self) -> Self {
        match self {
            Self::Int => Self::Int,
            Self::Char => Self::Char,
            Self::Boolean => Self::Boolean,
            Self::ClassName(id) => Self::ClassName(id.copy()),
        }
    }
}
impl<'a> Parses<'a> for Type {
    type Input = &'a [Token];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        or_else(
            map(Keyword::Int, |_| Self::Int),
            or_else(
                map(Keyword::Char, |_| Self::Char),
                or_else(
                    map(Keyword::Boolean, |_| Self::Boolean),
                    map(Token::id, Self::ClassName),
                ),
            ),
        )
        .parse(input)
    }
}

impl From<Id> for Type {
    fn from(item: Id) -> Self {
        Self::ClassName(item)
    }
}

impl XmlFormattable for Type {
    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        super::xmlformat::XmlBody::Inline
    }

    fn xml_inline_body(&self) -> String {
        match self {
            Self::Int => Keyword::Int.xml_inline_body(),
            Self::Char => Keyword::Char.xml_inline_body(),
            Self::Boolean => Keyword::Boolean.xml_inline_body(),
            Self::ClassName(value) => value.xml_inline_body(),
        }
    }

    fn xml_elem(&self) -> &str {
        match self {
            Self::Int => Keyword::Int.xml_elem(),
            Self::Char => Keyword::Char.xml_elem(),
            Self::Boolean => Keyword::Boolean.xml_elem(),
            Self::ClassName(value) => value.xml_elem(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "{}", Keyword::Int),
            Self::Char => write!(f, "{}", Keyword::Char),
            Self::Boolean => write!(f, "{}", Keyword::Boolean),
            Self::ClassName(value) => write!(f, "{}", value),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::jack::{
        common::testutil::{assert_tokens, transform_result},
        token::IntConst,
    };

    use super::*;

    fn token_result_to_type(tokens: &[Token]) -> Result<Type, Option<Token>> {
        let parser = Type::parse_into;
        transform_result(parser.parse(tokens))
    }

    #[test]
    fn types() {
        let cases = vec![
            ("int", Ok(Type::Int)),
            ("char", Ok(Type::Char)),
            ("boolean", Ok(Type::Boolean)),
            ("MyClass", Ok(Type::ClassName(Id::from("MyClass")))),
            ("12345", Err(Some(IntConst::new(12345).expect("").into()))),
            ("1Class", Err(Some(IntConst::new(1).expect("").into()))),
        ];
        assert_tokens(cases, token_result_to_type);
    }
}
