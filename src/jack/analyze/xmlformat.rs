use std::fmt::Display;

pub enum XmlBody {
    Inline,
    Expanded,
}

pub trait XmlFormattable: Sized + std::fmt::Debug {
    fn xml_elem(&self) -> &str;
    fn xml_body_type(&self) -> XmlBody {
        XmlBody::Expanded
    }
    fn xml_inline_body(&self) -> String {
        "".to_owned()
    }
    fn write_xml_body<'a>(
        &self,
        _xmlf: &XmlF<'a, Self>,
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
        XmlF::new(syntax, self.indent + self.indent_inc, self.indent_inc)
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
                self.syntax.write_xml_body(self, f)?;
            }
            XmlBody::Expanded => {
                writeln!(f)?;
                self.syntax.write_xml_body(self, f)?;
                write!(f, "{tab}")?
            }
        }
        writeln!(f, "</{}>", self.syntax.xml_elem())?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

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

        fn xml_elem(&self) -> &str {
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
}
