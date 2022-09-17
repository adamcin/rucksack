use crate::jack::{
    expression::{Expression, ExpressionList, Term},
    sym::Sym,
};

use super::xmlformat::{XmlF, XmlFormattable};

impl XmlFormattable for Term {
    fn xml_elem(&self) -> &str {
        "term"
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::VarName(id) => xmlf.write_child(f, id)?,
            Self::VarSub(id, expr) => {
                xmlf.write_child(f, id)?;
                xmlf.write_child(f, &Sym::LSquare)?;
                xmlf.write_child(f, expr.as_ref())?;
                xmlf.write_child(f, &Sym::RSquare)?;
            }
            Self::StringConst(value) => xmlf.write_child(f, value)?,
            Self::IntConst(value) => xmlf.write_child(f, value)?,
            Self::KeywordConst(value) => xmlf.write_child(f, &value.as_keyword())?,
            Self::UnaryOp(op) => {
                xmlf.write_child(f, &op.as_sym())?;
                xmlf.write_child(f, op.term())?;
            }
            Self::Expr(expr) => {
                xmlf.write_child(f, &Sym::LRound)?;
                xmlf.write_child(f, expr.as_ref())?;
                xmlf.write_child(f, &Sym::RRound)?;
            }
            Self::SubroutineCall(call) => {
                if let Some(qual) = call.qualifier() {
                    xmlf.write_child(f, qual)?;
                    xmlf.write_child(f, &Sym::Dot)?;
                }
                xmlf.write_child(f, call.name())?;
                xmlf.write_child(f, &Sym::LRound)?;
                xmlf.write_child(f, call.expressions())?;
                xmlf.write_child(f, &Sym::RRound)?;
            }
        };
        Ok(())
    }
}

impl XmlFormattable for Expression {
    fn xml_elem(&self) -> &str {
        "expression"
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        xmlf.write_child(f, self.term())?;
        for op in self.iter() {
            xmlf.write_child(f, &op.as_sym())?;
            xmlf.write_child(f, op.term())?;
        }
        Ok(())
    }
}

impl XmlFormattable for ExpressionList {
    fn xml_elem(&self) -> &str {
        "expressionList"
    }

    fn write_xml_body<'a>(
        &self,
        xmlf: &XmlF<'a, Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let mut first = true;
        for expr in self.iter() {
            if !first {
                xmlf.write_child(f, &Sym::Comma)?;
            } else {
                first = false;
            }
            xmlf.write_child(f, expr)?;
        }
        Ok(())
    }
}
