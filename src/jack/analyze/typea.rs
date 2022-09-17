use crate::jack::{keyword::Keyword, typea::Type};

use super::xmlformat::XmlFormattable;

impl XmlFormattable for Type {
    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        super::xmlformat::XmlBody::Inline
    }

    fn xml_inline_body(&self) -> String {
        match self {
            Self::Int => Keyword::Int.xml_inline_body(),
            Self::Char => Keyword::Char.xml_inline_body(),
            Self::Boolean => Keyword::Boolean.xml_inline_body(),
            Self::ClassName(value) => value.xml_inline_body(),
        }
    }

    fn xml_elem(&self) -> &str {
        match self {
            Self::Int => Keyword::Int.xml_elem(),
            Self::Char => Keyword::Char.xml_elem(),
            Self::Boolean => Keyword::Boolean.xml_elem(),
            Self::ClassName(value) => value.xml_elem(),
        }
    }
}
