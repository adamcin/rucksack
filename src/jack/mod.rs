pub(crate) mod analyze;
mod class;
mod common;
pub(crate) mod compile;
mod expression;
mod id;
mod keyword;
mod statement;
mod subroutine;
mod sym;
mod token;
mod typea;
use std::fs::DirEntry;
use std::io::Error;
use std::path::Path;

use crate::common::{err_invalid_input, DirUnit, FileUnit, Unit, UnitFactory};
use crate::parse::*;
use crate::vm::{VMDirUnit, VMUnit, VMUnitFactory, VMUnitType};

use self::class::Class;
use self::token::TokenStream;

pub struct JackFileUnit {
    src_path: String,
}

impl JackFileUnit {
    pub fn new(src_path: String) -> Self {
        Self { src_path }
    }

    pub fn out_unit(&self) -> Result<VMUnit, Error> {
        Ok(VMUnit::new(self.src_path().replace(".jack", ".vm")))
    }
}

impl Unit for JackFileUnit {
    type Syntax = Class;

    fn src_path(&self) -> &str {
        self.src_path.as_str()
    }
}

impl FileUnit for JackFileUnit {
    fn parse(&self) -> Result<Self::Syntax, Error> {
        let source = std::fs::read_to_string(self.src_path())?;
        let (_str_rem, stream) = TokenStream::parse_into(source.as_str()).expect("must work");
        let parser = Class::parse_into;
        let result = parser.parse(stream.tokens());
        match result {
            Ok((_rem, parsed)) => Ok(parsed),
            Err(error_at) => {
                println!("{:?}", error_at);
                Err(err_invalid_input(format!("parse error")))
            }
        }
    }
}

pub struct JackDirUnit {
    src_path: String,
}

impl Unit for JackDirUnit {
    type Syntax = Class;

    fn src_path(&self) -> &str {
        &self.src_path
    }
}

impl DirUnit for JackDirUnit {
    fn filename_for(elem: &Self::Syntax) -> String {
        format!("{}.jack", elem.name())
    }
    fn parse(&self) -> Result<Vec<Self::Syntax>, Error> {
        JackDirParser::parse_all(self)
    }
}

impl JackDirUnit {
    pub fn new(src_path: String) -> Self {
        Self { src_path }
    }

    pub fn out_unit(&self) -> Result<VMDirUnit, Error> {
        VMUnitFactory::read_dir_as_dir(self.src_path.as_str())
    }
}

pub struct JackDirParser {}

impl JackDirParser {
    fn parse_all(unit: &JackDirUnit) -> Result<Vec<Class>, Error> {
        let children: Result<Vec<DirEntry>, Error> = std::fs::read_dir(unit.src_path())?.collect();
        let jack_files: Vec<String> = children?
            .iter()
            .filter_map(|child| child.path().to_str().map(String::from))
            .filter(|name| name.ends_with(".jack"))
            .collect();
        let units: Result<Vec<JackUnitType>, Error> = jack_files
            .iter()
            .map(|src_path| JackUnitFactory::unit_from(src_path))
            .collect();

        units?
            .iter()
            .map(|wrapped| match wrapped {
                JackUnitType::FileUnit(jack_unit) => jack_unit.parse(),
                _ => Err(err_invalid_input("had a dir type in a dir type")),
            })
            .collect()
    }
}

pub enum JackUnitType {
    FileUnit(JackFileUnit),
    DirUnit(JackDirUnit),
}
impl JackUnitType {
    pub fn out_unit(&self) -> Result<VMUnitType, Error> {
        match self {
            Self::FileUnit(unit) => unit.out_unit().map(VMUnitType::File),
            Self::DirUnit(unit) => unit.out_unit().map(VMUnitType::Dir),
        }
    }
}
impl Unit for JackUnitType {
    type Syntax = Class;

    fn src_path(&self) -> &str {
        match self {
            Self::FileUnit(unit) => unit.src_path.as_str(),
            Self::DirUnit(unit) => unit.src_path.as_str(),
        }
    }
}

impl DirUnit for JackUnitType {
    fn filename_for(elem: &Self::Syntax) -> String {
        format!("{}.jack", elem.name())
    }

    fn parse(&self) -> Result<Vec<Self::Syntax>, Error> {
        match self {
            Self::FileUnit(unit) => unit.parse().map(|class| vec![class]),
            Self::DirUnit(unit) => unit.parse(),
        }
    }
}

pub struct JackUnitFactory {}

impl JackUnitFactory {
    fn read_jack(src_path: &str) -> Result<JackUnitType, Error> {
        if !src_path.ends_with(".jack") {
            return Err(err_invalid_input(format!("Invalid filename: {}", src_path)));
        }
        Ok(JackUnitType::FileUnit(JackFileUnit::new(
            src_path.to_owned(),
        )))
    }

    pub fn read_dir_as_dir(src_path: &str) -> Result<JackDirUnit, Error> {
        Ok(JackDirUnit::new(src_path.to_owned()))
    }

    pub fn read_dir(src_path: &str) -> Result<JackUnitType, Error> {
        Self::read_dir_as_dir(src_path).map(JackUnitType::DirUnit)
    }
}

impl UnitFactory for JackUnitFactory {
    type Unit = JackUnitType;
    fn unit_from(path: &str) -> Result<Self::Unit, Error> {
        let path_as_path = Path::new(path);
        if path_as_path.is_dir() {
            // for fs::read_dir(path).ok()
            Self::read_dir(path) // .expect(&format!("failed to read directory {}", src_path))
        } else if path.ends_with(".jack") {
            Self::read_jack(path) // .expect(&format!("failed to read vm {}", src_path))
        } else {
            Err(err_invalid_input(format!(
                "invalid source file extension {}",
                path
            )))
        }
    }
}
