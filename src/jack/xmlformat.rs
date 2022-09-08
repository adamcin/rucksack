use std::fmt::Display;

pub enum XmlBody {
    Inline,
    Expanded,
}

pub trait XmlFormattable: Sized + std::fmt::Debug {
    fn xml_elem<'a>(&'a self) -> &str;
    fn xml_body_type(&self) -> XmlBody {
        XmlBody::Expanded
    }
    fn xml_inline_body(&self) -> String {
        "".to_owned()
    }
    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self.xml_body_type() {
            XmlBody::Inline => write!(f, " {} ", self.xml_inline_body()),
            XmlBody::Expanded => Ok(()),
        }
    }
}

pub struct XmlF<'a, T: XmlFormattable> {
    indent: usize,
    indent_inc: usize,
    syntax: &'a T,
}

impl<'a, T> XmlF<'a, T>
where
    T: XmlFormattable,
{
    pub fn new(syntax: &'a T, indent: usize, indent_inc: usize) -> Self {
        Self {
            indent,
            indent_inc,
            syntax,
        }
    }

    fn for_child<'b, U>(&'a self, syntax: &'b U) -> XmlF<'b, U>
    where
        U: XmlFormattable,
        'a: 'b,
    {
        XmlF {
            indent: self.indent + self.indent_inc,
            indent_inc: self.indent_inc,
            syntax,
        }
    }

    pub fn write_child<'b, U>(
        &'a self,
        f: &mut std::fmt::Formatter<'_>,
        syntax: &'b U,
    ) -> std::fmt::Result
    where
        U: XmlFormattable,
        'a: 'b,
    {
        write!(f, "{}", self.for_child(syntax))
    }

    fn tab(indent: usize) -> String {
        format!("{0:indent$}", "")
    }
}

impl<'a, T> Display for XmlF<'a, T>
where
    T: XmlFormattable,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tab = Self::tab(self.indent);
        write!(f, "{tab}<{}>", self.syntax.xml_elem())?;
        match self.syntax.xml_body_type() {
            XmlBody::Inline => {
                self.syntax.write_xml_body(&self, f)?;
            }
            XmlBody::Expanded => {
                writeln!(f, "")?;
                self.syntax.write_xml_body(&self, f)?;
                write!(f, "{tab}")?
            }
        }
        writeln!(f, "</{}>", self.syntax.xml_elem())?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{fmt::Debug, fs::read_to_string, path::Path};

    use crate::jack::token::TokenStream;
    use crate::jack::{class::Class, common::testutil::read_test_file};
    use crate::parse::*;

    use super::{XmlF, XmlFormattable};

    #[derive(Debug)]
    struct SimpleInline {}

    impl XmlFormattable for SimpleInline {
        fn xml_inline_body(&self) -> String {
            "chars".to_owned()
        }

        fn xml_body_type(&self) -> super::XmlBody {
            super::XmlBody::Inline
        }

        fn xml_elem<'a>(&'a self) -> &str {
            "simple"
        }
    }

    #[test]
    fn xml_indent() {
        let simple = SimpleInline {};
        assert_eq!(
            "<simple> chars </simple>\n".to_owned(),
            format!("{}", XmlF::new(&simple, 0, 2))
        );
        assert_eq!(
            "  <simple> chars </simple>\n".to_owned(),
            format!("{}", XmlF::new(&simple, 2, 2))
        );
    }

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
            let source = read_test_file(jack_file).trim().to_owned();
            let expected_tokens = read_test_file(token_path.as_str()).trim().to_owned();
            let expected_syntax = read_test_file(syntax_path.as_str()).trim().to_owned();

            let (_, stream) = TokenStream::parse_into(source.as_str()).expect("");
            let fmtter_tokens = XmlF::new(&stream, 0, 0);
            let formatted_tokens = format!("{}", fmtter_tokens).trim().to_owned();
            for (n, (expect, actual)) in expected_tokens
                .lines()
                .zip(formatted_tokens.lines())
                .enumerate()
            {
                assert_eq!(expect, actual, "line {} in {}", n + 1, token_path.as_str());
            }

            let (_, syntax) = Class::parse_into(stream.tokens()).expect("");
            let fmtter_syntax = XmlF::new(&syntax, 0, 2);
            let formatted_syntax = format!("{}", fmtter_syntax).trim().to_owned();
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
