use crate::jack::id::Id;

use super::xmlformat::XmlFormattable;

impl XmlFormattable for Id {
    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        super::xmlformat::XmlBody::Inline
    }

    fn xml_elem(&self) -> &str {
        "identifier"
    }

    fn xml_inline_body(&self) -> String {
        (self.as_str()).to_owned()
    }
}
