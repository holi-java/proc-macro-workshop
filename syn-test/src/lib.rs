use std::fmt::Display;

#[doc(hidden)]
#[inline(always)]
#[cold]
pub fn expand<T: Display>(s: T) -> String {
    s.to_string()
        .replace('{', "{\n")
        .replace('}', "}\n")
        .replace(',', ",\n")
        .replace(';', ";\n")
}

#[macro_export]
macro_rules! assert_token_stream_eq {
    ($exp:expr, { $($tt:tt)* }) => {
        ::pretty_assertions::assert_eq!($crate::expand(quote::ToTokens::to_token_stream(&$exp)),
            $crate::expand(::quote::quote!($($tt)*)));
    };
}

#[macro_export]
macro_rules! generate_parse_quote {
    ($name:ident, $ty:ty) => {
        $crate::generate_parse_quote!(($)$name, $ty);
    };
    (($dollar:tt) $name:ident, $ty:ty) => {
        macro_rules! $name {
            ($dollar($dollar tt:tt)*) => {{
                let $name: $ty = ::syn::parse_quote!($dollar($dollar tt)*);
                $name
            }};
        }
    };
}

#[macro_export]
macro_rules! generate_parse_str {
    ($name:ident, $ty:ty) => {
        $crate::generate_parse_str!(($)$name, $ty);
    };
    (($dollar:tt) $name:ident, $ty:ty) => {
        macro_rules! $name {
            ($dollar($dollar tt:tt)*) => {{
                let $name: ::syn::Result<$ty> = ::syn::parse_str(stringify!($dollar($dollar tt)*));
                $name
            }};
        }
    };
    }
