use std::fmt::Display;

pub(crate) fn expand<T: Display>(s: T) -> String {
    s.to_string()
        .replace('{', "{\n")
        .replace('}', "}\n")
        .replace(',', ",\n")
        .replace(';', ";\n")
}

#[macro_export]
macro_rules! assert_token_stream_eq {
    ($exp:expr, { $($tt:tt)* }) => {
        ::pretty_assertions::assert_eq!($crate::test::expand(quote::ToTokens::to_token_stream(&$exp)),
            $crate::test::expand(::quote::quote!($($tt)*)));
    };
}
