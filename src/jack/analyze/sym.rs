use crate::jack::sym::Sym;

use super::xmlformat::XmlFormattable;

impl XmlFormattable for Sym {
    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        super::xmlformat::XmlBody::Inline
    }

    fn xml_elem(&self) -> &str {
        "symbol"
    }

    fn xml_inline_body(&self) -> String {
        match self {
            Self::Amp => "&amp;".to_owned(),
            Self::LAngle => "&lt;".to_owned(),
            Self::RAngle => "&gt;".to_owned(),
            _ => self.as_str().to_owned(),
        }
    }
}
