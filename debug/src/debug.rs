use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse::Parse, Data, DeriveInput, Ident};

use crate::field::Field;

pub(crate) struct Debug {
    input: DeriveInput,
}

impl Debug {
    fn name(&self) -> &Ident {
        &self.input.ident
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
        let name = self.name();
        let fmt: TokenStream = quote!(::core::fmt);
        let fields = match self.input.data {
            Data::Struct(ref s) => &s.fields,
            _ => todo!(),
        };

        let debug_fields = fields.into_iter().map(|f| Field::from(f.clone()));
        tokens.extend(quote!(
            impl #fmt::Debug for Field {
                fn fmt(&self, f: &mut #fmt::Formatter<'_>) -> #fmt::Result {
                    f.debug_struct(stringify!(#name)).
                        #(#debug_fields.)*
                        finish()
                }
            }
        ));
    }
}

#[cfg(test)]
mod tests {
    use crate::assert_token_stream_eq;

    macro_rules! debug {
        ($($tt:tt)*) => {{
            let debug: super::Debug = syn::parse_quote!($($tt)*);
            debug
        }};
    }

    #[test]
    fn name() {
        let debug = debug!(
            #[derive(CustomDebug)]
            struct Field;
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
}
