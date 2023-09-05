use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::{Expr::Lit, ExprLit, Lit::Str, Result};

pub(crate) struct Field {
    field: syn::Field,
}

impl Field {
    fn format(&self) -> TokenStream {
        let name = &self.field.ident;
        for attr in &self.field.attrs {
            if attr.path().is_ident("debug") {
                let expr = &attr.meta.require_name_value().unwrap().value;
                if let Lit(ExprLit {
                    attrs: _,
                    lit: Str(format),
                }) = expr
                {
                    return quote!(format_args!(#format, self.#name));
                }
            }
        }
        quote!(self.#name)
    }
}

impl Parse for Field {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(syn::Field::parse_named(input)?.into())
    }
}

impl ToTokens for Field {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let (name, format) = (&self.field.ident, self.format());
        tokens.extend(quote!(field(&stringify!(#name), &#format)));
    }
}

impl From<syn::Field> for Field {
    fn from(field: syn::Field) -> Self {
        Field { field }
    }
}

#[cfg(test)]
mod tests {
    use crate::assert_token_stream_eq;
    macro_rules! field {
        ($($tt: tt)*) => {{
            let field: super::Field = ::syn::parse_quote!($($tt)*);
            field
        }};
    }

    #[test]
    fn normal_field() {
        let field = field!(s: String);
        assert_token_stream_eq!(field, { field(&stringify!(s), &self.s) });
    }

    #[test]
    fn custom_format_field() {
        let field = field!(#[debug = ":>5"]s: String);
        assert_token_stream_eq!(field, { field(&stringify!(s), &format_args!(":>5", self.s)) });
    }
}
