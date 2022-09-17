use crate::common::{err_invalid_input, FileUnit, Unit, UnitFactory};
use crate::parse::*;
use crate::symbol::{Symbol, Symbols};
use std::fmt::Debug;
use std::fmt::Display;
use std::io::Error;
use std::io::Write;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Comp {
    XXX,
    OOO,
    III,
    OsI,
    DDD,
    AAA,
    MMM,
    NoD,
    NoA,
    NoM,
    OsD,
    OsA,
    OsM,
    DpI,
    ApI,
    MpI,
    DsI,
    AsI,
    MsI,
    DpA,
    DpM,
    DsA,
    DsM,
    AsD,
    MsD,
    DnA,
    DnM,
    DrA,
    DrM,
}

impl From<&str> for Comp {
    fn from(item: &str) -> Self {
        match item {
            "0" => Comp::OOO,
            "1" => Comp::III,
            "-1" => Comp::OsI,
            "D" => Comp::DDD,
            "A" => Comp::AAA,
            "M" => Comp::MMM,
            "!D" => Comp::NoD,
            "!A" => Comp::NoA,
            "!M" => Comp::NoM,
            "-D" => Comp::OsD,
            "-A" => Comp::OsA,
            "-M" => Comp::OsM,
            "D+1" => Comp::DpI,
            "A+1" => Comp::ApI,
            "M+1" => Comp::MpI,
            "D-1" => Comp::DsI,
            "A-1" => Comp::AsI,
            "M-1" => Comp::MsI,
            "D+A" | "A+D" => Comp::DpA,
            "D+M" | "M+D" => Comp::DpM,
            "D-A" => Comp::DsA,
            "D-M" => Comp::DsM,
            "A-D" => Comp::AsD,
            "M-D" => Comp::MsD,
            "D&A" | "A&D" => Comp::DnA,
            "D&M" | "M&D" => Comp::DnM,
            "D|A" | "A|D" => Comp::DrA,
            "D|M" | "M|D" => Comp::DrM,
            _ => Comp::XXX,
        }
    }
}

impl Comp {
    fn as_str(&self) -> &'static str {
        match self {
            Self::OOO => "0",
            Self::III => "1",
            Self::OsI => "-1",
            Self::DDD => "D",
            Self::AAA => "A",
            Self::MMM => "M",
            Self::NoD => "!D",
            Self::NoA => "!A",
            Self::NoM => "!M",
            Self::OsD => "-D",
            Self::OsA => "-A",
            Self::OsM => "-M",
            Self::DpI => "D+1",
            Self::ApI => "A+1",
            Self::MpI => "M+1",
            Self::DsI => "D-1",
            Self::AsI => "A-1",
            Self::MsI => "M-1",
            Self::DpA => "D+A",
            Self::DpM => "D+M",
            Self::DsA => "D-A",
            Self::DsM => "D-M",
            Self::AsD => "A-D",
            Self::MsD => "M-D",
            Self::DnA => "D&A",
            Self::DnM => "D&M",
            Self::DrA => "D|A",
            Self::DrM => "D|M",
            Self::XXX => "!!!",
        }
    }

    fn as_hack(&self) -> &'static str {
        match self {
            Self::OOO => "0101010",
            Self::III => "0111111",
            Self::OsI => "0111010",
            Self::DDD => "0001100",
            Self::AAA => "0110000",
            Self::MMM => "1110000",
            Self::NoD => "0001101",
            Self::NoA => "0110001",
            Self::NoM => "1110001",
            Self::OsD => "0001111",
            Self::OsA => "0110011",
            Self::OsM => "1110011",
            Self::DpI => "0011111",
            Self::ApI => "0110111",
            Self::MpI => "1110111",
            Self::DsI => "0001110",
            Self::AsI => "0110010",
            Self::MsI => "1110010",
            Self::DpA => "0000010",
            Self::DpM => "1000010",
            Self::DsA => "0010011",
            Self::DsM => "1010011",
            Self::AsD => "0000111",
            Self::MsD => "1000111",
            Self::DnA => "0000000",
            Self::DnM => "1000000",
            Self::DrA => "0010101",
            Self::DrM => "1010101",
            Self::XXX => "1111111",
        }
    }

    fn parser() -> CompParser {
        CompParser {}
    }
}

impl Display for Comp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

pub struct CompParser {}

impl<'a> Parser<'a, &'a str, Comp> for CompParser {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'a str, Comp> {
        match (1..=3)
            .rev()
            .filter(|&len| input.len() >= len)
            .filter_map(|bound| -> Option<(usize, Comp)> {
                let comp: Comp = (&input[0..bound]).into();
                Some(comp).filter(|c| c != &Comp::XXX).map(|c| (bound, c))
            })
            .next()
        {
            Some((bound, comp)) => Ok((&input[bound..], comp)),
            None => Err(input),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Dest {
    Invalid,
    Null,
    M,
    D,
    DM,
    A,
    AM,
    AD,
    ADM,
}

impl From<&str> for Dest {
    fn from(item: &str) -> Self {
        item.chars()
            .map(|c| -> Dest { (&c).into() })
            .fold(Self::Null, |acc, d| acc.combine(&d))
    }
}

impl From<&char> for Dest {
    fn from(item: &char) -> Self {
        match item {
            'M' => Self::M,
            'D' => Self::D,
            'A' => Self::A,
            _ => Self::Invalid,
        }
    }
}

impl Dest {
    fn as_str(&self) -> &'static str {
        match self {
            Self::M => "M",
            Self::D => "D",
            Self::DM => "MD",
            Self::A => "A",
            Self::AM => "AM",
            Self::AD => "AD",
            Self::ADM => "ADM",
            Self::Null => "",
            _ => "!!!",
        }
    }

    fn as_hack(&self) -> &'static str {
        match self {
            Self::M => "001",
            Self::D => "010",
            Self::DM => "011",
            Self::A => "100",
            Self::AM => "101",
            Self::AD => "110",
            Self::ADM => "111",
            _ => "000",
        }
    }

    pub fn includes(&self, other: &Dest) -> bool {
        match (self, other) {
            (&Self::Invalid, _) => true,
            (_, &Self::Null) => true,
            (_, _) if self == other => true,
            (&Self::ADM, _) if other != &Self::Invalid => true,
            (&Self::AM, &Self::A) | (&Self::AM, &Self::M) => true,
            (&Self::AD, &Self::A) | (&Self::AD, &Self::D) => true,
            (&Self::DM, &Self::D) | (&Self::DM, &Self::M) => true,
            _ => false,
        }
    }

    pub fn combine(&self, other: &Dest) -> Self {
        match (self, other) {
            (_, _) if self.includes(other) => *self,
            (&Self::AM, &Self::D) | (&Self::DM, &Self::A) | (&Self::AD, &Self::M) => Self::ADM,
            (&Self::A, &Self::M) => Self::AM,
            (&Self::A, &Self::D) => Self::AD,
            (&Self::D, &Self::M) => Self::DM,
            _ => other.combine(self),
        }
    }

    pub fn parser() -> DestParser {
        DestParser {}
    }
}

pub struct DestParser {}

impl<'a> Parser<'a, &'a str, Dest> for DestParser {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'a str, Dest> {
        map(
            left(
                range(
                    pred(map(any_char, |c| (&c).into()), |dest: &Dest| {
                        dest != &Dest::Invalid
                    }),
                    1..=3,
                ),
                match_literal("="),
            ),
            |dests| -> Dest { dests.iter().fold(Dest::Null, |acc, d| acc.combine(d)) },
        )
        .parse(input)
    }
}

impl Display for Dest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Jump {
    Invalid,
    Null,
    Jgt,
    Jeq,
    Jge,
    Jlt,
    Jne,
    Jle,
    Jmp,
}

impl From<&str> for Jump {
    fn from(item: &str) -> Self {
        match item {
            "JGT" => Jump::Jgt,
            "JEQ" => Jump::Jeq,
            "JGE" => Jump::Jge,
            "JLT" => Jump::Jlt,
            "JNE" => Jump::Jne,
            "JLE" => Jump::Jle,
            "JMP" => Jump::Jmp,
            _ => Jump::Invalid,
        }
    }
}

impl Jump {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Jgt => "JGT",
            Self::Jeq => "JEQ",
            Self::Jge => "JGE",
            Self::Jlt => "JLT",
            Self::Jne => "JNE",
            Self::Jle => "JLE",
            Self::Jmp => "JMP",
            Self::Null => "",
            _ => "!!!",
        }
    }

    fn as_hack(&self) -> &'static str {
        match self {
            Self::Jgt => "001",
            Self::Jeq => "010",
            Self::Jge => "011",
            Self::Jlt => "100",
            Self::Jne => "101",
            Self::Jle => "110",
            Self::Jmp => "111",
            _ => "000",
        }
    }

    fn parser() -> JumpParser {
        JumpParser {}
    }
}

impl Display for Jump {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

pub struct JumpParser {}

impl<'a> Parser<'a, &'a str, Jump> for JumpParser {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'a str, Jump> {
        match (3..=3)
            .filter(|&len| input.len() >= len)
            .map(|bound| -> Jump { (&input[0..bound]).into() })
            .next()
        {
            Some(jump) if jump != Jump::Invalid => Ok((&input[3..], jump)),
            _ => Err(input),
        }
    }
}

#[derive(Clone)]
pub enum ASMLine {
    Comment(String),
    AInstr(Symbol),
    CInstr(Dest, Comp, Jump),
    LInstr(Symbol),
}

impl ASMLine {
    fn strip_symbol(&self, symbols: &mut Symbols) -> ASMLine {
        match self {
            ASMLine::AInstr(symbol) => ASMLine::AInstr(symbols.reg_replace(symbol)),
            ASMLine::LInstr(symbol) => symbols
                .replace(symbol)
                .map(ASMLine::LInstr)
                .unwrap_or_else(|| panic!("unregistered label {}", symbol)),
            _ => self.to_owned(),
        }
    }

    fn to_hack(&self) -> Option<String> {
        match self {
            ASMLine::AInstr(symbol) => symbol.to_binary().map(|data| format!("0{}", data)),
            ASMLine::CInstr(dest, comp, jump) => Some(format!(
                "111{}{}{}",
                comp.as_hack(),
                dest.as_hack(),
                jump.as_hack()
            )),
            _ => None,
        }
    }

    fn as_string(&self) -> String {
        match self {
            ASMLine::LInstr(symbol) => format!("({})", symbol),
            ASMLine::AInstr(symbol) => format!("@{}", symbol),
            ASMLine::CInstr(Dest::Null, comp, Jump::Null) => format!("{}", comp),
            ASMLine::CInstr(Dest::Null, comp, jump) => format!("{};{}", comp, jump),
            ASMLine::CInstr(dest, comp, Jump::Null) => format!("{}={}", dest, comp),
            ASMLine::CInstr(dest, comp, jump) => format!("{}={};{}", dest, comp, jump),
            ASMLine::Comment(text) => format!("//{}", text),
        }
    }

    pub fn is_indexed(&self) -> bool {
        self.is_sloc() && !self.is_label()
    }

    pub fn is_sloc(&self) -> bool {
        !matches!(self, &ASMLine::Comment(..))
    }

    pub fn is_blank(&self) -> bool {
        false
    }

    fn is_label(&self) -> bool {
        matches!(self, &ASMLine::LInstr(..))
    }

    fn parser() -> ASMLineParser {
        ASMLineParser {}
    }
}

impl Display for ASMLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

pub struct ASMLineParser {}

impl<'a> Parser<'a, &'a str, ASMLine> for ASMLineParser {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'a str, ASMLine> {
        or_else(
            right(
                match_literal("@"),
                left(
                    map(Symbol::parser(), ASMLine::AInstr),
                    range(inlinespace_char(), 0..),
                ),
            ),
            or_else(
                right(
                    match_literal("("),
                    left(map(Symbol::parser(), ASMLine::LInstr), match_literal(")")),
                ),
                or_else(
                    right(
                        match_literal("//"),
                        map(non_nl0(), |text| {
                            ASMLine::Comment(text.into_iter().collect())
                        }),
                    ),
                    map(
                        pair(
                            peek(4, |p: &'a str| Ok((p, p.contains('='))), Dest::parser()),
                            pair(
                                Comp::parser(),
                                peek(
                                    1,
                                    |p: &'a str| Ok((p, p.starts_with(';'))),
                                    right(match_literal(";"), Jump::parser()),
                                ),
                            ),
                        ),
                        |(o_dest, (comp, o_jump))| {
                            ASMLine::CInstr(
                                o_dest.unwrap_or(Dest::Null),
                                comp,
                                o_jump.unwrap_or(Jump::Null),
                            )
                        },
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

#[derive(Clone)]
pub struct ASMUnit {
    src_path: String,
}

impl Unit for ASMUnit {
    type Syntax = ASMParsed;

    fn src_path(&self) -> &str {
        self.src_path.as_str()
    }
}

impl FileUnit for ASMUnit {
    fn parse(&self) -> Result<Self::Syntax, Error> {
        let source = std::fs::read_to_string(self.src_path())?;
        let parser = ASMParser {};
        match parser.parse(&source) {
            Ok((_remaining, syntax)) => Ok(ASMParsed::new(syntax)),
            Err(source_at) => Err(err_invalid_input(source_at)),
        }
    }
}

impl UnitFactory for ASMUnit {
    type Unit = Self;

    fn unit_from(src_path: &str) -> Result<Self::Unit, Error> {
        if !src_path.ends_with(Self::src_ext()) {
            return Err(err_invalid_input(format!("Invalid filename: {}", src_path)));
        }
        Ok(Self::new(src_path.to_owned()))
    }
}

impl ASMUnit {
    pub fn src_ext() -> &'static str {
        ".asm"
    }

    pub fn new(src_path: String) -> Self {
        Self { src_path }
    }

    pub fn out_unit(&self) -> HackUnit {
        HackUnit::new(
            self.src_path()
                .to_owned()
                .replace(Self::src_ext(), HackUnit::src_ext()),
        )
    }
}

pub struct HackUnit {
    src_path: String,
}

impl HackUnit {
    fn new(src_path: String) -> Self {
        Self { src_path }
    }

    fn src_ext() -> &'static str {
        ".hack"
    }

    fn parser() -> HackParser {
        HackParser {}
    }
}

impl Unit for HackUnit {
    type Syntax = HackSyntax;

    fn src_path(&self) -> &str {
        self.src_path.as_str()
    }
}

impl FileUnit for HackUnit {
    fn parse(&self) -> Result<Self::Syntax, Error> {
        let source = std::fs::read_to_string(self.src_path())?;
        let parser = Self::parser();
        match parser.parse(&source) {
            Ok((_remaining, lines)) => Ok(HackSyntax::new(lines)),
            Err(source_at) => Err(err_invalid_input(source_at)),
        }
    }
}

pub struct HackParser {}

impl<'a> Parser<'a, &'a str, Vec<String>> for HackParser {
    fn parse(&self, _input: &'a str) -> ParseResult<'a, &'a str, Vec<String>> {
        unimplemented!()
    }
}

pub struct HackSyntax {
    lines: Vec<String>,
}

impl HackSyntax {
    fn new(lines: Vec<String>) -> Self {
        Self { lines }
    }
}

impl<'p> Display for HackSyntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for code in self.lines.iter() {
            writeln!(f, "{}", code)?;
        }
        Ok(())
    }
}

impl<'p> Debug for HackSyntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

pub struct ASMParsed {
    source: Vec<ASMLine>,
}

pub fn now(src: &str) -> Vec<ASMLine> {
    ASMParsed::parse_literal(src).expect("assemble error")
}

impl ASMParsed {
    pub fn new(source: Vec<ASMLine>) -> Self {
        return Self { source };
    }

    pub fn parse_literal(src: &str) -> Result<Vec<ASMLine>, Error> {
        let parser = ASMParser {};
        match parser.parse(src) {
            Ok((_remaining, syntax)) => Ok(syntax),
            Err(source_at) => Err(err_invalid_input(source_at)),
        }
    }

    fn lines<'a>(&'a self) -> Vec<&'a ASMLine> {
        self.source.iter().collect()
    }

    fn slocs<'a>(&'a self) -> Vec<&'a ASMLine> {
        self.source.iter().filter(|&line| line.is_sloc()).collect()
    }

    pub fn into_first_pass(self) -> FirstPass {
        FirstPass::new(self)
    }
}

impl Display for ASMParsed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line_num = 0;
        let width = format!("{}", self.lines().len()).len();
        let line_width = 50;
        for line in self.lines().iter().filter(|line| !line.is_blank()) {
            let line_str = format!("{line}");
            if line.is_indexed() {
                writeln!(f, "{line_str:line_width$} //   {line_num:width$}")?;
                line_num += 1;
            } else {
                writeln!(f, "{line:line_width$}")?;
            }
        }
        Ok(())
    }
}

impl Debug for ASMParsed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line_num = 0;
        let width = format!("{}", self.lines().len()).len();
        let line_width = 50;
        for line in self.lines().iter().filter(|line| !line.is_blank()) {
            let line_str = format!("{line}");
            if line.is_indexed() {
                writeln!(f, "{line_str:line_width$} //   {line_num:width$}")?;
                line_num += 1;
            } else {
                writeln!(f, "{line:line_width$}")?;
            }
        }
        Ok(())
    }
}

pub struct FirstPass {
    parsed: ASMParsed,
    symbols: Symbols,
}

impl<'p> FirstPass {
    fn new(parsed: ASMParsed) -> Self {
        let mut symbols = Symbols::new();
        Self::register_labels(&parsed, &mut symbols);
        return Self { parsed, symbols };
    }

    fn lines<'f>(&'f self) -> Vec<&'f ASMLine> {
        return self.parsed.slocs().iter().cloned().collect();
    }

    fn register_labels(parsed: &ASMParsed, symbols: &mut Symbols) {
        let mut line: u16 = 0;
        for sloc in parsed.slocs().iter() {
            match sloc {
                ASMLine::LInstr(symbol) => symbols.register_variable(&line, symbol),
                _ => line = line + 1,
            }
        }
    }

    pub fn into_second_pass(self) -> SecondPass {
        SecondPass::new(self)
    }
}

pub struct SecondPass {
    stripped: Vec<ASMLine>,
}

impl<'p> SecondPass {
    fn new(first_pass: FirstPass) -> Self {
        let mut symbols = Symbols::copy(&first_pass.symbols);
        let stripped = first_pass
            .lines()
            .iter()
            .map(|line| line.strip_symbol(&mut symbols))
            .collect();
        Self { stripped }
    }

    fn lines<'a>(&'a self) -> Vec<&'a ASMLine> {
        return self.stripped.iter().map(|line| line).collect();
    }

    fn to_hack(&self) -> HackSyntax {
        HackSyntax {
            lines: self
                .lines()
                .iter()
                .filter_map(|line| line.to_hack())
                .collect(),
        }
    }
}

impl<'p> Display for SecondPass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for code in self.lines() {
            if let Some(line) = code.to_hack() {
                writeln!(f, "{}", line)?;
            };
        }
        Ok(())
    }
}

impl<'p> Debug for SecondPass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line = 0;
        let lines = self.lines();
        let width = format!("{}", lines.len()).len();
        for code in lines {
            if code.is_label() {
                writeln!(
                    f,
                    "{:width$}                  {}",
                    " ",
                    code.as_string(),
                    width = width
                )?;
            } else {
                writeln!(
                    f,
                    "{:width$} {} {}",
                    line,
                    code.to_hack().unwrap_or("????????????????".to_owned()),
                    code.as_string(),
                    width = width
                )?;
                line = line + 1;
            }
        }
        Ok(())
    }
}

pub struct ASMParser {}

impl<'a> Parser<'a, &'a str, Vec<ASMLine>> for ASMParser {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'a str, Vec<ASMLine>> {
        range(
            right(
                space0(),
                left(ASMLine::parser(), right(pad0(), or_else(newline(), eof))),
            ),
            0..,
        )
        .parse(input)
    }
}

pub struct HackAssembler {}

impl HackAssembler {
    pub fn do_main(args: &[String]) {
        for arg in args {
            Self::do_unit(arg);
        }
    }

    pub fn do_unit(path: &str) {
        let unit = ASMUnit::unit_from(path).expect(&format!("failed to read {}", path));
        let code = unit
            .parse()
            .expect("expected parsed")
            .into_first_pass()
            .into_second_pass();
        writeln!(std::io::stdout(), "{:?}", code).ok();
        let hack = code.to_hack();
        unit.out_unit()
            .save(&hack)
            .expect("failed to save hack file");
    }
}

#[cfg(test)]
mod test {
    use crate::parse::*;

    use super::*;

    #[test]
    fn dest_parser() {
        let parse_dest = ok(Dest::parser());
        assert_eq!(Ok(("A+1", Some(Dest::A))), parse_dest.parse("A=A+1"));
        assert_eq!(Ok(("B=B+1", None)), parse_dest.parse("B=B+1"))
    }

    #[test]
    fn jump_parser() {
        let parse_jump = ok(Jump::parser());
        assert_eq!(Ok(("", Some(Jump::Jmp))), parse_jump.parse("JMP"));
        assert_eq!(Ok(("JNG", None)), parse_jump.parse("JNG"));
    }

    #[test]
    fn comp_parser() {
        let parse_comp = pair(
            peek(
                4,
                |p: &'static str| Ok((p, p.contains("="))),
                Dest::parser(),
            ),
            pair(
                Comp::parser(),
                peek(
                    1,
                    |p: &'static str| Ok((p, p.starts_with(";"))),
                    right(match_literal(";"), Jump::parser()),
                ),
            ),
        );
        assert_eq!(
            Ok(("", (Some(Dest::A), (Comp::ApI, Some(Jump::Jmp))))),
            parse_comp.parse("A=A+1;JMP")
        );
        assert_eq!(Err("B=B+1"), parse_comp.parse("B=B+1"));
    }
}
