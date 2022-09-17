use crate::jack::{
    class::{Class, ClassVarDec, ClassVarKind},
    keyword::Keyword,
    sym::Sym,
};

use super::xmlformat::{XmlBody, XmlF, XmlFormattable};

impl XmlFormattable for ClassVarDec {
    fn xml_elem(&self) -> &str {
        "classVarDec"
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, self.var_kind())?;
        xmlf.write_child(f, self.var_type())?;
        if let Some((&first, tail)) = self.var_names().split_first() {
            xmlf.write_child(f, first)?;
            for &name in tail {
                xmlf.write_child(f, &Sym::Comma)?;
                xmlf.write_child(f, name)?;
            }
        }
        xmlf.write_child(f, &Sym::Semi)?;
        Ok(())
    }
}

impl XmlFormattable for ClassVarKind {
    fn xml_elem(&self) -> &str {
        "keyword"
    }

    fn xml_body_type(&self) -> XmlBody {
        XmlBody::Inline
    }

    fn xml_inline_body(&self) -> String {
        self.as_keyword().xml_inline_body()
    }
}

impl XmlFormattable for Class {
    fn xml_elem(&self) -> &str {
        "class"
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::Class)?;
        xmlf.write_child(f, self.id())?;
        xmlf.write_child(f, &Sym::LCurly)?;
        for var in self.vars() {
            xmlf.write_child(f, var)?;
        }
        for dec in self.subs() {
            xmlf.write_child(f, dec)?;
        }
        xmlf.write_child(f, &Sym::RCurly)?;
        Ok(())
    }
}
