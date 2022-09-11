use crate::parse::*;
use std::collections::HashMap;
use std::fmt::Display;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    name: String,
}

impl Symbol {
    fn to_u16(&self) -> Option<u16> {
        self.name.parse::<u16>().ok()
    }

    fn name(&self) -> &str {
        &self.name
    }

    pub fn to_binary(&self) -> Option<String> {
        self.to_u16().map(|num| format!("{:015b}", num))
    }

    pub fn parser() -> SymbolParser {
        SymbolParser {}
    }
}

impl From<&str> for Symbol {
    fn from(item: &str) -> Self {
        Self {
            name: item.to_owned(),
        }
    }
}

impl From<&u16> for Symbol {
    fn from(item: &u16) -> Self {
        Self {
            name: format!("{}", item),
        }
    }
}

impl From<String> for Symbol {
    fn from(item: String) -> Self {
        Self { name: item }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

pub struct SymbolParser {}

impl SymbolParser {
    fn is_ident_lead(c: &char) -> bool {
        // _ . $ :
        c.is_ascii_alphabetic() || c == &'_' || c == &'.' || c == &'$' || c == &':'
    }

    fn is_ident(c: &char) -> bool {
        Self::is_ident_lead(c) || c.is_ascii_digit()
    }
}

impl<'a> Parser<'a, &'a str, Symbol> for SymbolParser {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'a str, Symbol> {
        map(
            map(
                or_else(
                    range(digit_char(), 1..=5),
                    map(
                        pair(
                            pred(any_char, Self::is_ident_lead),
                            range(pred(any_char, Self::is_ident), 0..),
                        ),
                        |(l, cs)| vec![vec![l], cs].concat(),
                    ),
                ),
                |chars| chars.into_iter().collect(),
            ),
            |s: String| -> Symbol { s.into() },
        )
        .parse(input)
    }
}

type SymbolMapping = (Symbol, u16);

pub enum PredefSymbol {
    Sp,
    Lcl,
    Arg,
    This,
    That,
    Screen,
    Kbd,
}

impl PredefSymbol {
    fn to_mapping(&self) -> SymbolMapping {
        let sym = self.as_symbol();
        let offset = match self {
            Self::Sp => 0,
            Self::Lcl => 1,
            Self::Arg => 2,
            Self::This => 3,
            Self::That => 4,
            Self::Screen => 16384,
            Self::Kbd => 24576,
        };
        (sym, offset)
    }

    fn all() -> Vec<Self> {
        vec![
            Self::Sp,
            Self::Lcl,
            Self::Arg,
            Self::This,
            Self::That,
            Self::Screen,
            Self::Kbd,
        ]
    }

    pub fn as_symbol(&self) -> Symbol {
        self.as_str().into()
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::Sp => "SP",
            Self::Lcl => "LCL",
            Self::Arg => "ARG",
            Self::This => "THIS",
            Self::That => "THAT",
            Self::Screen => "SCREEN",
            Self::Kbd => "KBD",
        }
    }
}

pub struct Symbols {
    var_count: u16,
    table: HashMap<Symbol, u16>,
}

impl Symbols {
    pub fn new() -> Self {
        let table: HashMap<_, _> = HashMap::from_iter(
            vec![Self::builtin_registers(), Self::builtin_others()]
                .concat()
                .into_iter(),
        );
        Self {
            var_count: 16,
            table,
        }
    }

    pub fn copy(other: &Self) -> Self {
        Self {
            var_count: other.var_count,
            table: other.table.to_owned(),
        }
    }

    pub fn reg_replace(&mut self, symbol: &Symbol) -> Symbol {
        self.replace(symbol).unwrap_or_else(|| {
            let next = self.next_variable();
            self.register_variable(&next, symbol);
            (&next).into()
        })
    }

    pub fn replace(&self, symbol: &Symbol) -> Option<Symbol> {
        symbol
            .to_u16()
            .map(|data| (&data).into())
            .or_else(|| self.table.get(symbol).map(|data| data.into()))
    }

    pub fn register_variable(&mut self, data: &u16, symbol: &Symbol) {
        self.table.insert(symbol.to_owned(), data.to_owned());
    }

    fn next_variable(&mut self) -> u16 {
        let next = self.var_count;
        self.var_count += 1;
        next
    }

    fn builtin_registers() -> Vec<SymbolMapping> {
        (0..16).map(|i| (format!("R{}", i).into(), i)).collect()
    }

    fn builtin_others() -> Vec<SymbolMapping> {
        PredefSymbol::all()
            .iter()
            .map(|predef| predef.to_mapping())
            .collect()
    }
}
