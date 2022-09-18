mod api;
mod class;
mod expression;
mod statement;
mod sub;
mod vartable;

use std::{convert::TryFrom, io::Error};

use crate::{common::err_invalid_input, vm::VMParsed};

use self::api::Api;

use super::{class::Class, JackUnitFactory};

use crate::common::*;

pub type CompileError = String;
pub fn comberr(wrap: String, prev: &CompileError) -> CompileError {
    format!("{}\n* {}", wrap, prev)
}

pub struct Compiler {}

impl Compiler {
    pub fn compile(&self, classes: Vec<Class>) -> Result<Vec<VMParsed>, Error> {
        let api = Api::new(classes.as_slice())
            .map_err(|err| err_invalid_input(format!("api compile error {}", err)))?;
        classes
            .into_iter()
            .map(|class| {
                let name = class.name().to_owned();
                VMParsed::try_from((&api, class))
                    .map_err(|err| format!("class {} compile error {}", name, err))
            })
            .map(|r| r.map_err(err_invalid_input))
            .collect()
    }
}

pub struct JackCompiler {}

impl JackCompiler {
    pub fn do_main(args: &[String]) -> Result<(), Error> {
        if args.is_empty() {
            Self::do_unit(".")?;
        } else {
            for arg in args {
                Self::do_unit(arg)?;
            }
        }
        Ok(())
    }

    fn do_unit(path: &str) -> Result<(), Error> {
        let unit = JackUnitFactory::unit_from(path)
            .unwrap_or_else(|err| panic!("failed to read unit {}, error: {:?}", path, err));
        let classes = unit.parse()?;
        let vm_unit = unit.out_unit()?;
        let compiler = Compiler {};
        let parseds = compiler
            .compile(classes)
            .map_err(|err| err_invalid_input(format!("unit {} compile error: {:?}", path, err)))?;
        vm_unit.save(&parseds)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use std::{io::Error, path::PathBuf};

    use crate::jack::compile::JackCompiler;

    #[test]
    fn compile_11() {
        let dirs: Vec<_> = vec![
            "data/11/Average",
            "data/11/ComplexArrays",
            "data/11/ConvertToBin",
            "data/11/Pong",
            "data/11/Seven",
            "data/11/Square",
        ]
        .iter()
        .cloned()
        .map(|s| s.to_owned())
        .collect();

        let result = JackCompiler::do_main(dirs.as_slice());
        if let Err(error) = &result {
            println!("compile_11 error: {:?}", error);
        }
        assert!(&result.is_ok());
    }

    #[test]
    fn compile_12() {
        let dirs: Vec<_> = vec!["data/12"]
            .iter()
            .cloned()
            .map(|s| s.to_owned())
            .collect();

        let result = JackCompiler::do_main(dirs.as_slice());
        if let Err(error) = &result {
            println!("compile_11 error: {:?}", error);
        }
        assert!(&result.is_ok());
    }

    fn copy_class_to_test_dir(test_dir: &str, filename: &str) -> Result<(), Error> {
        let mut dest_file = PathBuf::from(test_dir);
        let mut src_file: PathBuf = dest_file.parent().unwrap().into();
        src_file.push(filename);
        dest_file.push(filename);
        std::fs::copy(src_file, dest_file).map(|_| ())
    }

    #[test]
    fn compile_12_array() -> Result<(), Error> {
        let dir = "data/12/ArrayTest";
        copy_class_to_test_dir(dir, "Array.jack")?;
        JackCompiler::do_main(&[dir.to_owned()])?;
        Ok(())
    }

    #[test]
    fn compile_12_keyboard() -> Result<(), Error> {
        let dir = "data/12/KeyboardTest";
        copy_class_to_test_dir(dir, "Keyboard.jack")?;
        JackCompiler::do_main(&[dir.to_owned()])?;
        Ok(())
    }

    #[test]
    fn compile_12_math() -> Result<(), Error> {
        let dir = "data/12/MathTest";
        copy_class_to_test_dir(dir, "Math.jack")?;
        JackCompiler::do_main(&[dir.to_owned()])?;
        Ok(())
    }

    #[test]
    fn compile_12_memory() -> Result<(), Error> {
        let dir = "data/12/MemoryTest";
        copy_class_to_test_dir(dir, "Memory.jack")?;
        JackCompiler::do_main(&[dir.to_owned()])?;
        Ok(())
    }

    #[test]
    fn compile_12_output() -> Result<(), Error> {
        let dir = "data/12/OutputTest";
        copy_class_to_test_dir(dir, "Output.jack")?;
        JackCompiler::do_main(&[dir.to_owned()])?;
        Ok(())
    }

    #[test]
    fn compile_12_screen() -> Result<(), Error> {
        let dir = "data/12/ScreenTest";
        copy_class_to_test_dir(dir, "Screen.jack")?;
        JackCompiler::do_main(&[dir.to_owned()])?;
        Ok(())
    }

    #[test]
    fn compile_12_string() -> Result<(), Error> {
        let dir = "data/12/StringTest";
        copy_class_to_test_dir(dir, "String.jack")?;
        JackCompiler::do_main(&[dir.to_owned()])?;
        Ok(())
    }

    #[test]
    fn compile_12_sys() -> Result<(), Error> {
        let dir = "data/12/SysTest";
        copy_class_to_test_dir(dir, "Sys.jack")?;
        JackCompiler::do_main(&[dir.to_owned()])?;
        Ok(())
    }
}
