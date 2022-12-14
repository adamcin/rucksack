use std::fmt::Display;

use crate::common::err_invalid_input;
use crate::parse::*;

use super::keyword::Keyword;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Id(String);

impl Id {
    pub fn new(value: String) -> Self {
        if Keyword::is_reserved(value.as_str()) {
            panic!("Id is reserved keyword: {}", value.as_str());
        }
        Self(value)
    }

    fn new_internal(value: String) -> Self {
        Self(value)
    }

    pub fn try_new(value: &str) -> Result<Id, &str> {
        if Keyword::is_reserved(value) {
            Err(value)
        } else {
            Ok(Self::new_internal(value.to_owned()))
        }
    }

    pub fn copy(&self) -> Self {
        Self::new(self.0.to_owned())
    }

    fn is_ident_lead(c: &char) -> bool {
        c.is_ascii_alphabetic() || c == &'_'
    }

    fn is_ident(c: &char) -> bool {
        Self::is_ident_lead(c) || c.is_ascii_digit()
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<&str> for Id {
    fn from(value: &str) -> Self {
        Self::new(value.to_owned())
    }
}

impl<'a> Parses<'a> for Id {
    type Input = &'a str;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        and_then(
            map(
                map(
                    pair(
                        pred(any_char, Self::is_ident_lead),
                        range(pred(any_char, Self::is_ident), 0..),
                    ),
                    |(l, cs)| vec![vec![l], cs].concat(),
                ),
                |chars| chars.into_iter().collect(),
            ),
            |s: String| {
                if Keyword::is_reserved(s.as_str()) {
                    Err(err_invalid_input(format!("{} is a reserved keyword", s)))
                } else {
                    Ok(Self::new(s))
                }
            },
        )
        .parse(input)
    }
}

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn ids() {
        let parser = Id::parse_into;

        assert_eq!(Ok(("", Id("MyClass".to_owned()))), parser.parse("MyClass"));
        assert_eq!(Err("12345"), parser.parse("12345"));
        assert_eq!(Err("1Class"), parser.parse("1Class"));
        assert_eq!(Ok(("", Id("Class1".to_owned()))), parser.parse("Class1"));
    }
}
