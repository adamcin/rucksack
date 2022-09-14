#[cfg(test)]
pub(crate) mod testutil {
    use std::{fmt::Debug, fs::read_to_string, path::Path};

    use crate::{
        jack::{class::Class, token::*},
        parse::*,
    };

    pub trait TokenResultFn<R> = Fn(&[Token]) -> Result<R, Option<Token>>;

    pub fn transform_result<'a, R>(
        result: ParseResult<'a, &'a [Token], R>,
    ) -> Result<R, Option<Token>> {
        result
            .map_err(|value| value.first().cloned())
            .map(|(_, value)| value)
    }

    pub fn assert_tokens<F, R>(pairs: Vec<(&str, Result<R, Option<Token>>)>, pf: F)
    where
        F: TokenResultFn<R> + Copy, // Box<dyn Parser<'a, &'a [Token], R> + 'a> + Copy + 'a,
        R: Debug + std::cmp::PartialEq,
    {
        pairs.into_iter().for_each(|(source, expected)| {
            let src_str = source.to_owned();
            let (_, stream) = TokenStream::parse_into(src_str.as_str())
                .unwrap_or_else(|_| panic!("failed to tokenize {}", source));
            let result = pf(stream.tokens());
            assert_eq!(expected, result);
        });
    }

    pub fn read_test_file<P>(path: P) -> String
    where
        P: AsRef<Path> + Debug,
    {
        read_to_string(&path).unwrap_or_else(|_| {
            panic!(
                "read_test_file failed to read path {:?} from current directory {}",
                &path,
                std::path::absolute(".")
                    .expect("failed to get absolute path of current directory")
                    .display(),
            )
        })
    }

    pub fn read_class<P>(jack_file: P) -> Result<Class, String>
    where
        P: AsRef<Path> + Debug,
    {
        let source = read_test_file(jack_file).trim().to_owned();
        let (_, stream) = TokenStream::parse_into(source.as_str()).map_err(|err| err.to_owned())?;
        let (_, class) = Class::parse_into(stream.tokens()).map_err(|err| format!("{:?}", err))?;
        Ok(class)
    }
}
