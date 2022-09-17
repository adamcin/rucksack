use crate::jack::{
    keyword::Keyword,
    subroutine::{
        ParameterList, ReturnType, SubroutineBody, SubroutineDec, SubroutineKind, VarDec,
    },
    sym::Sym,
};

use super::xmlformat::{XmlF, XmlFormattable};

impl XmlFormattable for SubroutineKind {
    fn xml_elem(&self) -> &str {
        match self {
            Self::Constructor => Keyword::Constructor.xml_elem(),
            Self::Function => Keyword::Function.xml_elem(),
            Self::Method => Keyword::Method.xml_elem(),
        }
    }

    fn xml_inline_body(&self) -> String {
        match self {
            Self::Constructor => Keyword::Constructor.xml_inline_body(),
            Self::Function => Keyword::Function.xml_inline_body(),
            Self::Method => Keyword::Method.xml_inline_body(),
        }
    }

    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        match self {
            Self::Constructor => Keyword::Constructor.xml_body_type(),
            Self::Function => Keyword::Function.xml_body_type(),
            Self::Method => Keyword::Method.xml_body_type(),
        }
    }
}

impl XmlFormattable for ReturnType {
    fn xml_inline_body(&self) -> String {
        match self {
            Self::Void => Keyword::Void.xml_inline_body(),
            Self::Returns(value) => value.xml_inline_body(),
        }
    }

    fn xml_elem(&self) -> &str {
        match self {
            Self::Void => Keyword::Void.xml_elem(),
            Self::Returns(value) => value.xml_elem(),
        }
    }

    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        match self {
            Self::Void => Keyword::Void.xml_body_type(),
            Self::Returns(value) => value.xml_body_type(),
        }
    }
}

impl XmlFormattable for ParameterList {
    fn xml_elem(&self) -> &str {
        "parameterList"
    }

    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        super::xmlformat::XmlBody::Expanded
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let mut first = true;
        for param in self.vars() {
            if !first {
                xmlf.write_child(f, &Sym::Comma)?;
            }
            first = false;
            xmlf.write_child(f, param.var_type())?;
            xmlf.write_child(f, param.var_name())?;
        }
        Ok(())
    }
}

impl XmlFormattable for VarDec {
    /*
    <varDec>
        <keyword> var </keyword>
        <keyword> int </keyword>
        <identifier> i </identifier>
        <symbol> , </symbol>
        <identifier> sum </identifier>
        <symbol> ; </symbol>
      </varDec>
     */
    fn xml_elem(&self) -> &str {
        "varDec"
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::Var)?;
        xmlf.write_child(f, self.var_type())?;
        if let Some((&first, tail)) = self.var_names().split_first() {
            xmlf.write_child(f, first)?;
            for &var_name in tail {
                xmlf.write_child(f, &Sym::Comma)?;
                xmlf.write_child(f, var_name)?;
            }
        }

        xmlf.write_child(f, &Sym::Semi)?;
        Ok(())
    }
}

impl XmlFormattable for SubroutineBody {
    fn xml_elem(&self) -> &str {
        "subroutineBody"
    }

    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        super::xmlformat::XmlBody::Expanded
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Sym::LCurly)?;
        for var_dec in self.vars() {
            xmlf.write_child(f, var_dec)?;
        }
        xmlf.write_child(f, self.statements())?;
        xmlf.write_child(f, &Sym::RCurly)?;
        Ok(())
    }
}

impl XmlFormattable for SubroutineDec {
    fn xml_elem(&self) -> &str {
        "subroutineDec"
    }

    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        super::xmlformat::XmlBody::Expanded
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, self.kind())?;
        xmlf.write_child(f, self.return_type())?;
        xmlf.write_child(f, self.name())?;
        xmlf.write_child(f, &Sym::LRound)?;
        xmlf.write_child(f, self.params())?;
        xmlf.write_child(f, &Sym::RRound)?;
        xmlf.write_child(f, self.body())?;
        Ok(())
    }
}
