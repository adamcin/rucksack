use crate::jack::{
    expression::{Call, Expression},
    id::Id,
    keyword::Keyword,
    statement::{Statement, Statements},
    sym::Sym,
};

use super::xmlformat::{XmlF, XmlFormattable};

impl Statement {
    fn xml_body_let<'a>(
        id: &Id,
        osub: &Option<Expression>,
        value: &Expression,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::Let)?;
        xmlf.write_child(f, id)?;
        if let Some(sub) = osub.as_ref() {
            xmlf.write_child(f, &Sym::LSquare)?;
            xmlf.write_child(f, sub)?;
            xmlf.write_child(f, &Sym::RSquare)?;
        }
        xmlf.write_child(f, &Sym::Equals)?;
        xmlf.write_child(f, value)?;
        xmlf.write_child(f, &Sym::Semi)?;
        Ok(())
    }

    fn xml_body_if<'a>(
        condition: &Expression,
        body: &Statements,
        oelze: &Option<Statements>,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::If)?;
        xmlf.write_child(f, &Sym::LRound)?;
        xmlf.write_child(f, condition)?;
        xmlf.write_child(f, &Sym::RRound)?;
        xmlf.write_child(f, &Sym::LCurly)?;
        xmlf.write_child(f, body)?;
        xmlf.write_child(f, &Sym::RCurly)?;
        if let Some(elze) = oelze.as_ref() {
            xmlf.write_child(f, &Keyword::Else)?;
            xmlf.write_child(f, &Sym::LCurly)?;
            xmlf.write_child(f, elze)?;
            xmlf.write_child(f, &Sym::RCurly)?;
        }
        Ok(())
    }

    fn xml_body_while<'a>(
        condition: &Expression,
        body: &Statements,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::While)?;
        xmlf.write_child(f, &Sym::LRound)?;
        xmlf.write_child(f, condition)?;
        xmlf.write_child(f, &Sym::RRound)?;
        xmlf.write_child(f, &Sym::LCurly)?;
        xmlf.write_child(f, body)?;
        xmlf.write_child(f, &Sym::RCurly)?;
        Ok(())
    }

    fn xml_body_do<'a>(
        call: &Call,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::Do)?;
        if let Some(qual) = call.qualifier() {
            xmlf.write_child(f, qual)?;
            xmlf.write_child(f, &Sym::Dot)?;
        }
        xmlf.write_child(f, call.name())?;
        xmlf.write_child(f, &Sym::LRound)?;
        xmlf.write_child(f, call.expressions())?;
        xmlf.write_child(f, &Sym::RRound)?;
        xmlf.write_child(f, &Sym::Semi)?;
        Ok(())
    }

    fn xml_body_return<'a>(
        ovalue: &Option<Expression>,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, &Keyword::Return)?;
        if let Some(value) = ovalue.as_ref() {
            xmlf.write_child(f, value)?;
        }
        xmlf.write_child(f, &Sym::Semi)?;
        Ok(())
    }
}

impl XmlFormattable for Statement {
    fn xml_elem(&self) -> &str {
        match self {
            Self::Let(..) => "letStatement",
            Self::If(..) => "ifStatement",
            Self::While(..) => "whileStatement",
            Self::Do(..) => "doStatement",
            Self::Return(..) => "returnStatement",
        }
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Let(id, osub, value) => Self::xml_body_let(id, osub, value, xmlf, f)?,
            Self::If(condition, stmts, elze) => Self::xml_body_if(condition, stmts, elze, xmlf, f)?,
            Self::While(condition, stmts) => Self::xml_body_while(condition, stmts, xmlf, f)?,
            Self::Do(call) => Self::xml_body_do(call, xmlf, f)?,
            Self::Return(value) => Self::xml_body_return(value, xmlf, f)?,
        }
        Ok(())
    }
}

impl XmlFormattable for Statements {
    fn xml_elem(&self) -> &str {
        "statements"
    }

    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        super::xmlformat::XmlBody::Expanded
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        for stmt in self.iter() {
            xmlf.write_child(f, stmt)?;
        }
        Ok(())
    }
}
