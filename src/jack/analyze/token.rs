use crate::jack::token::{IntConst, StringConst, Token, TokenStream};

use super::xmlformat::{XmlBody, XmlF, XmlFormattable};

impl XmlFormattable for IntConst {
    fn xml_elem(&self) -> &str {
        "integerConstant"
    }

    fn xml_inline_body(&self) -> String {
        format!("{}", self.value())
    }

    fn xml_body_type(&self) -> XmlBody {
        XmlBody::Inline
    }
}

impl XmlFormattable for StringConst {
    fn xml_elem(&self) -> &str {
        "stringConstant"
    }

    fn xml_inline_body(&self) -> String {
        self.value()
    }

    fn xml_body_type(&self) -> XmlBody {
        XmlBody::Inline
    }
}

impl XmlFormattable for Token {
    fn xml_elem(&self) -> &str {
        match self {
            Self::Keyword(value) => value.xml_elem(),
            Self::Sym(value) => value.xml_elem(),
            Self::Identifier(value) => value.xml_elem(),
            Self::StringConst(value) => value.xml_elem(),
            Self::IntConst(value) => value.xml_elem(),
        }
    }

    fn xml_body_type(&self) -> XmlBody {
        XmlBody::Inline
    }

    fn xml_inline_body(&self) -> String {
        match self {
            Self::Keyword(keyword) => keyword.xml_inline_body(),
            Self::Sym(sym) => sym.xml_inline_body(),
            Self::Identifier(id) => id.xml_inline_body(),
            Self::StringConst(value) => value.xml_inline_body(),
            Self::IntConst(value) => value.xml_inline_body(),
        }
    }
}

impl XmlFormattable for TokenStream {
    fn xml_elem(&self) -> &str {
        "tokens"
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        for token in self.tokens() {
            xmlf.write_child(f, token)?;
        }
        Ok(())
    }
}
