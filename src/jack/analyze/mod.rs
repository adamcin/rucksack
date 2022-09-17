use std::{
    fs::{read_to_string, File},
    io::Error,
    io::Write,
    path::Path,
};

use crate::{common::*, parse::Parses};

use self::xmlformat::XmlF;

use super::{token::TokenStream, JackUnitFactory};

mod class;
mod expression;
mod id;
mod keyword;
mod statement;
mod subroutine;
mod sym;
mod token;
mod typea;
mod xmlformat;

pub struct JackAnalyzer {}

impl JackAnalyzer {
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
        if path.ends_with(".jack") {
            let unit = JackUnitFactory::unit_from(path)
                .unwrap_or_else(|err| panic!("failed to read unit {}, error: {:?}", path, err));
            let syntaxes = unit
                .parse()
                .unwrap_or_else(|err| panic!("failed to read unit {}, error: {:?}", path, err));
            let syntax = syntaxes.first().expect("one class expected");
            let syntax_path = path.replace(".jack", ".xml");
            let mut syntax_file = File::create(Path::new(&syntax_path))?;
            let fmtter_syntax = XmlF::new(syntax, 0, 2);
            write!(syntax_file, "{}", fmtter_syntax).unwrap_or_else(|err| {
                panic!(
                    "failed to write .xml file {}, error: {:?}",
                    syntax_path, err
                )
            });

            let source = read_to_string(&path)?;
            let (_, stream) = TokenStream::parse_into(source.as_str())
                .unwrap_or_else(|err| panic!("failed to read unit {}, error: {:?}", path, err));
            let token_path = path.replace(".jack", "T.xml");
            let mut token_file = File::create(Path::new(&token_path))?;
            let fmtter_tokens = XmlF::new(&stream, 0, 0);
            write!(token_file, "{}", fmtter_tokens).unwrap_or_else(|err| {
                panic!(
                    "failed to write T.xml file {}, error: {:?}",
                    token_path, err
                )
            });

            Ok(())
        } else {
            Err(err_invalid_input("only .jack paths accepted"))
        }
    }
}

#[cfg(test)]
mod test {
    use crate::jack::common::testutil::read_test_file;

    use super::*;

    #[test]
    fn xml_out_tokens() {
        let jack_files = vec![
            "data/10/ArrayTest/Main.jack",
            "data/10/ExpressionLessSquare/Main.jack",
            "data/10/ExpressionLessSquare/Square.jack",
            "data/10/ExpressionLessSquare/SquareGame.jack",
            "data/10/Square/Main.jack",
            "data/10/Square/Square.jack",
            "data/10/Square/SquareGame.jack",
        ];

        for jack_file in jack_files {
            let token_path = jack_file.replace(".jack", "T.xml");
            let syntax_path = jack_file.replace(".jack", ".xml");
            let compare_token_path = token_path.replace("data/", "compare/");
            let compare_syntax_path = syntax_path.replace("data/", "compare/");

            JackAnalyzer::do_main(&[jack_file.to_owned()]).expect("should work");
            let formatted_tokens = read_test_file(token_path.as_str()).trim().to_owned();
            let formatted_syntax = read_test_file(syntax_path.as_str()).trim().to_owned();
            let expected_tokens = read_test_file(compare_token_path.as_str())
                .trim()
                .to_owned();
            let expected_syntax = read_test_file(compare_syntax_path.as_str())
                .trim()
                .to_owned();

            for (n, (expect, actual)) in expected_tokens
                .lines()
                .zip(formatted_tokens.lines())
                .enumerate()
            {
                assert_eq!(expect, actual, "line {} in {}", n + 1, token_path.as_str());
            }

            for (n, (expect, actual)) in expected_syntax
                .lines()
                .zip(formatted_syntax.lines())
                .enumerate()
            {
                assert_eq!(expect, actual, "line {} in {}", n + 1, syntax_path.as_str());
            }
        }
    }
}
