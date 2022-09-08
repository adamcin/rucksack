use std::fmt::Display;

use crate::parse::*;

use super::{token::Token, xmlformat::XmlFormattable};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sym {
    LCurly,
    RCurly,
    LRound,
    RRound,
    LSquare,
    RSquare,
    LAngle,
    RAngle,
    Dot,
    Comma,
    Semi,
    Plus,
    Minus,
    Splat,
    Slash,
    Amp,
    Pipe,
    Equals,
    Tilde,
}

impl Sym {
    pub fn as_str(&self) -> &'static str {
        use Sym::*;
        match self {
            LCurly => "{",
            RCurly => "}",
            LRound => "(",
            RRound => ")",
            LSquare => "[",
            RSquare => "]",
            LAngle => "<",
            RAngle => ">",
            Dot => ".",
            Comma => ",",
            Semi => ";",
            Plus => "+",
            Minus => "-",
            Splat => "*",
            Slash => "/",
            Amp => "&",
            Pipe => "|",
            Equals => "=",
            Tilde => "~",
        }
    }

    pub fn all() -> Vec<Self> {
        use Sym::*;
        vec![
            LCurly, RCurly, LRound, RRound, LSquare, RSquare, LAngle, RAngle, Dot, Comma, Semi,
            Plus, Minus, Splat, Slash, Amp, Pipe, Equals, Tilde,
        ]
    }

    pub fn as_token(self) -> Token {
        Token::Sym(self)
    }
}

impl<'a> Parser<'a, &'a str, Sym> for Sym {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'a str, Sym> {
        map(match_literal(self.as_str()), |()| *self).parse(input)
    }
}

impl<'a> Parser<'a, &'a [Token], Sym> for Sym {
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, &'a [Token], Self> {
        match input.split_first() {
            Some((Token::Sym(value), rem)) if value == self => Ok((rem, *self)),
            _ => Err(input),
        }
    }
}

impl<'a> Parses<'a> for Sym {
    type Input = &'a str;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        let init_parser: Box<dyn Parser<'a, &'a str, Self>> = Box::new(none());

        Sym::all()
            .iter()
            .fold(init_parser, |acc, sym| -> Box<dyn Parser<&str, Self>> {
                Box::new(or_else(
                    move |input| sym.parse(input),
                    move |input| acc.parse(input),
                ))
            })
            .parse(input)
    }
}

impl XmlFormattable for Sym {
    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        super::xmlformat::XmlBody::Inline
    }
    
    fn xml_elem<'a>(&'a self) -> &str {
        "symbol"
    }

    fn xml_inline_body(&self) -> String {
        match self {
            Self::Amp => "&amp;".to_owned(),
            Self::LAngle => "&lt;".to_owned(),
            Self::RAngle => "&gt;".to_owned(),
            _ => self.as_str().to_owned(),
        }
    }
}

impl Display for Sym {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::*;

    #[test]
    fn parse_within_brackets() {
        // let ctx = Ctx{};
        // let parser = right(left(match_literal("needle")))
    }
}
