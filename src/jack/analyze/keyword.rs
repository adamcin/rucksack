//

use crate::jack::keyword::Keyword;

use super::xmlformat::XmlFormattable;

impl XmlFormattable for Keyword {
    fn xml_body_type(&self) -> super::xmlformat::XmlBody {
        super::xmlformat::XmlBody::Inline
    }

    fn xml_elem(&self) -> &str {
        "keyword"
    }

    fn xml_inline_body(&self) -> String {
        self.as_str().to_owned()
    }
}

mod tests {

    #[test]
    fn test_xml_fmt() {
        let xmlout = format!(
            "{}",
            crate::jack::analyze::xmlformat::XmlF::new(&crate::jack::keyword::Keyword::Class, 0, 0)
        );
        assert_eq!("<keyword> class </keyword>\n", &xmlout);
    }
}
