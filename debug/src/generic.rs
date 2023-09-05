use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

pub(crate) struct Generics<'a> {
    inner: &'a DeriveInput,
}

impl Generics<'_> {
    pub fn params(&self) -> TokenStream {
        let params = self.inner.generics.type_params().map(|param| &param.ident);
        let params = quote!(#(#params),*);
        if params.is_empty() {
            TokenStream::new()
        } else {
            quote!(<#params>)
        }
    }

    pub fn where_clause(&self) -> TokenStream {
        let params = self
            .inner
            .generics
            .type_params()
            .map(|param| &param.ident)
            .map(|param| quote!(#param: ::core::fmt::Debug));
        let bounds = quote!(#(#params),*);
        if bounds.is_empty() {
            TokenStream::new()
        } else {
            quote!(where #bounds,)
        }
    }
}

impl<'a> From<&'a syn::DeriveInput> for Generics<'a> {
    fn from(input: &'a syn::DeriveInput) -> Self {
        Generics { inner: input }
    }
}

#[cfg(test)]
mod tests {
    use crate::{assert_token_stream_eq, generate_macro};

    generate_macro!(derive, syn::DeriveInput);

    macro_rules! generics {
        ($name:ident => $($tt:tt)*) => {
            let _derive = derive!($($tt)*);
            let $name = super::Generics::from(&_derive);
        };
    }

    #[test]
    fn empty() {
        generics!(g =>
            #[derive(CustomDebug)]
            struct Empty;
        );
        assert_token_stream_eq!(g.where_clause(), {});
        assert_token_stream_eq!(g.params(), {});
    }

    #[test]
    fn simple() {
        generics!(g =>
            #[derive(CustomDebug)]
            struct Field<T> {
                name: T
            }
        );
        assert_token_stream_eq!(g.where_clause(), { where T: ::core::fmt::Debug,});
        assert_token_stream_eq!(g.params(), {<T>});
    }

    #[test]
    fn generic_with_bounds() {
        generics!(g =>
            #[derive(CustomDebug)]
            struct Field<T: Trait> {
                name: T
            }
        );
        assert_token_stream_eq!(g.where_clause(), { where T: ::core::fmt::Debug, });
        assert_token_stream_eq!(g.params(), {<T>});
    }

    #[test]
    #[ignore]
    fn generic_with_associated_type() {
        generics!(g =>
            #[derive(CustomDebug)]
            struct Field<T: Trait> {
                name: T::Name
            }
        );
        assert_token_stream_eq!(g.where_clause(), { where T: Trait, T::Name: ::core::fmt::Debug, });
        assert_token_stream_eq!(g.params(), {<T>});
    }
}
