use std::ops::Deref;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::Parse;

pub(crate) struct Generics {
    inner: syn::Generics,
}

impl Generics {
    pub fn params(&self) -> TokenStream {
        let params = self.type_params();
        let params = quote!(#(#params),*);
        if params.is_empty() {
            TokenStream::new()
        } else {
            quote!(<#params>)
        }
    }
}

impl Parse for Generics {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let inner = input.parse::<syn::Generics>()?;
        Ok(inner.into())
    }
}

impl ToTokens for Generics {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let params = self
            .type_params()
            .map(|param| quote!(#param: ::core::fmt::Debug));
        let params = quote!(#(#params),*);
        if !params.is_empty() {
            tokens.extend(quote!(<#params>));
        }
    }
}

impl Deref for Generics {
    type Target = syn::Generics;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl From<syn::Generics> for Generics {
    fn from(inner: syn::Generics) -> Self {
        Self { inner }
    }
}

#[cfg(test)]
mod tests {
    use crate::{assert_token_stream_eq, generate_macro};

    generate_macro!(generics, super::Generics);

    #[test]
    fn empty() {
        let g = generics!();
        assert_token_stream_eq!(g, {});
        assert_token_stream_eq!(g.params(), {});
    }

    #[test]
    fn simple() {
        let g = generics!(<T>);
        assert_token_stream_eq!(g, { <T: ::core::fmt::Debug>});
        assert_token_stream_eq!(g.params(), {<T>});
    }
}
