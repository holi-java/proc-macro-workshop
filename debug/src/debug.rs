use std::borrow::Borrow;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse::Parse, Data, DeriveInput, Ident};

use crate::{field::Field, generic::Generics};

pub(crate) struct Debug {
    input: DeriveInput,
}

impl Debug {
    fn name(&self) -> &Ident {
        &self.input.ident
    }

    fn fields(&self) -> impl Iterator<Item = Field> + '_ {
        let fields = match self.input.data {
            Data::Struct(ref s) => &s.fields,
            _ => todo!(),
        };
        fields.iter().map(|f| Field::from(f.clone()))
    }

    fn generics(&self) -> Generics {
        self.input.borrow().into()
    }
}

impl Parse for Debug {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let input = input.parse::<DeriveInput>()?;
        Ok(Debug { input })
    }
}

impl ToTokens for Debug {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let (name, fields, generics) = (self.name(), self.fields(), self.generics());
        let (params, where_clause) = (generics.params(), generics.where_clause());
        let fmt: TokenStream = quote!(::core::fmt);
        tokens.extend(quote!(
            impl #params #fmt::Debug for #name #params #where_clause {
                fn fmt(&self, f: &mut #fmt::Formatter<'_>) -> #fmt::Result {
                    f.debug_struct(stringify!(#name)).
                        #(#fields.)*
                        finish()
                }
            }
        ));
    }
}

#[cfg(test)]
mod tests {
    use crate::{assert_token_stream_eq, generate_macro};
    generate_macro!(debug, super::Debug);

    #[test]
    fn name() {
        let debug = debug!(
            #[derive(CustomDebug)]
            struct Field;
        );

        assert_token_stream_eq!(debug.name(), { Field });
    }

    #[test]
    fn generic_name() {
        let debug = debug!(
            #[derive(CustomDebug)]
            struct Field<T>(T);
        );

        assert_token_stream_eq!(debug.name(), { Field });
    }

    #[test]
    fn impl_debug_for_unit_struct() {
        let debug = debug!(
            #[derive(CustomDebug)]
            struct Field;
        );

        assert_token_stream_eq!(debug, {
            impl ::core::fmt::Debug for Field {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    f.debug_struct(stringify!(Field)).finish()
                }
            }
        });
    }

    #[test]
    fn impl_debug_for_named_struct() {
        let debug = debug!(
            #[derive(CustomDebug)]
            struct Field {
                name: String,
                readonly: bool,
            }
        );
        assert_token_stream_eq!(debug, {
            impl ::core::fmt::Debug for Field {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    f.debug_struct(stringify!(Field))
                        .field(&stringify!(name), &self.name)
                        .field(&stringify!(readonly), &self.readonly)
                        .finish()
                }
            }
        });
    }

    #[test]
    fn impl_debug_for_struct_with_generics() {
        let debug = debug!(
            #[derive(CustomDebug)]
            struct Field<T> {
                name: T,
            }
        );
        assert_token_stream_eq!(debug, {
            impl<T> ::core::fmt::Debug for Field<T>
            where
                T: ::core::fmt::Debug,
            {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    f.debug_struct(stringify!(Field))
                        .field(&stringify!(name), &self.name)
                        .finish()
                }
            }
        });
    }
}
