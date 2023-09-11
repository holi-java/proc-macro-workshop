use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Data, DeriveInput, Expr, ExprLit, Lit};

use crate::generics::Generics;

pub(crate) struct Fields<'a> {
    input: &'a DeriveInput,
    generics: Generics<'a>,
}

impl Fields<'_> {
    pub fn params(&self) -> TokenStream {
        let params = self.input.generics.type_params().map(|p| &p.ident);
        let params = quote!(#(#params),*);
        if params.is_empty() {
            return params;
        }
        quote!(<#params>)
    }

    pub fn where_clause(&self) -> TokenStream {
        let params = self
            .fields()
            .map(|f| f.type_params())
            .filter(|it| !it.is_empty());
        let params = quote!(#(#params,)*);
        if params.is_empty() {
            return params;
        }
        quote!(where #params)
    }

    pub fn fields(&self) -> impl Iterator<Item = Field> {
        match &self.input.data {
            Data::Struct(s) => s.fields.iter().map(|field| Field::new(self, field)),
            _ => todo!("supports struct only"),
        }
    }
}

impl ToTokens for Fields<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let fields = self.fields();
        tokens.extend(quote!(#(.#fields)*));
    }
}

impl<'a> From<&'a DeriveInput> for Fields<'a> {
    fn from(input: &'a DeriveInput) -> Self {
        Fields {
            input,
            generics: Generics::new(&input.generics),
        }
    }
}

pub(crate) struct Field<'a, 'b> {
    owner: &'a Fields<'a>,
    field: &'b syn::Field,
}

impl<'a, 'b> Field<'a, 'b> {
    pub fn new(owner: &'a Fields<'a>, field: &'b syn::Field) -> Self {
        Field { owner, field }
    }

    pub fn type_params(&self) -> TokenStream {
        self.owner.generics.type_param_bounds(&self.field.ty)
    }

    fn format_args(&self) -> TokenStream {
        let name = &self.field.ident;
        for attr in &self.field.attrs {
            if let Ok(meta) = attr.meta.require_name_value() {
                if meta.path.is_ident("debug") {
                    let format = match &meta.value {
                        Expr::Lit(ExprLit {
                            attrs: _,
                            lit: Lit::Str(s),
                        }) => s,
                        _ => todo!(),
                    };
                    return quote!(format_args!(#format, self.#name));
                }
            }
        }
        quote!(self.#name)
    }
}

impl ToTokens for Field<'_, '_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let (name, format_args) = (&self.field.ident, self.format_args());
        tokens.extend(quote!(field(&stringify!(#name), &#format_args)))
    }
}

#[cfg(test)]
mod tests {
    use syn_test::assert_token_stream_eq;

    use crate::generate_parse_quote;

    generate_parse_quote!(derive, syn::DeriveInput);

    macro_rules! fields {
        ($name: ident, $($tt:tt)*) => {
            let _derive = derive!($($tt)*);
            let $name = $crate::fields::Fields::from(&_derive);
        };
    }

    #[test]
    fn empty() {
        fields!(
            fields,
            #[derive(Any)]
            struct Empty;
        );

        assert_eq!(fields.fields().count(), 0);
        assert_token_stream_eq!(fields.params(), {});
        assert_token_stream_eq!(fields.where_clause(), {});
        assert_token_stream_eq!(fields, {});
    }

    #[test]
    fn no_generics() {
        fields!(
            fields,
            #[derive(Any)]
            struct Field {
                name: String,
            }
        );

        assert_eq!(fields.fields().count(), 1);
        assert_token_stream_eq!(fields.params(), {});
        assert_token_stream_eq!(fields.where_clause(), {});
        assert_token_stream_eq!(fields, {.field(&stringify!(name), &self.name)});
    }

    #[test]
    fn generic_param_without_bounds() {
        fields!(
            fields,
            #[derive(Any)]
            struct Field<N, T> {
                name: N,
                value: T,
            }
        );

        assert_eq!(fields.fields().count(), 2);
        assert_token_stream_eq!(fields.params(), { <N,T> });
        assert_token_stream_eq!(fields.where_clause(), {
            where N: ::core::fmt::Debug, T: ::core::fmt::Debug,
        });
        assert_token_stream_eq!(fields, {
            .field(&stringify!(name), &self.name)
            .field(&stringify!(value), &self.value)
        });
    }

    #[test]
    fn generic_param_with_bounds() {
        fields!(
            fields,
            #[derive(Any)]
            struct Field<N: Trait1, T: Trait2> {
                name: N,
                value: T,
            }
        );

        assert_token_stream_eq!(fields.params(), { <N,T> });
        assert_eq!(fields.fields().count(), 2);
        assert_token_stream_eq!(fields.where_clause(), {
            where N: ::core::fmt::Debug, T: ::core::fmt::Debug,
        });
    }
    #[test]
    fn generic_param_used_more_than_once() {
        fields!(
            fields,
            #[derive(Any)]
            struct Field<T> {
                name: T,
                alias: T,
            }
        );

        assert_token_stream_eq!(fields.where_clause(), { where T: ::core::fmt::Debug, });
    }

    mod field {
        use super::*;
        macro_rules! first_field {
            ($name: ident, $($tt:tt)*) => {
                fields!(_fields, $($tt)*);
                let $name = _fields.fields().next().unwrap();
            };
        }

        #[test]
        fn custom_format() {
            first_field!(
                field,
                #[derive(Any)]
                struct Field {
                    #[debug = ":>5"]
                    name: String,
                }
            );

            assert_token_stream_eq!(field.type_params(), {});
            assert_token_stream_eq!(field, {
                field(&stringify!(name), &format_args!(":>5", self.name))
            });
        }

        #[test]
        fn non_generics_type_params() {
            first_field!(
                field,
                #[derive(Any)]
                struct Field {
                    name: String,
                }
            );

            assert_token_stream_eq!(field.type_params(), {});
            assert_token_stream_eq!(field, { field(&stringify!(name), &self.name) });
        }

        #[test]
        fn generics_type_params() {
            first_field!(
                field,
                #[derive(Any)]
                struct Field<T> {
                    name: T,
                }
            );

            assert_token_stream_eq!(field.type_params(), { T: ::core::fmt::Debug });
        }

        #[test]
        fn nested_generics_type_params() {
            first_field!(
                field,
                #[derive(Any)]
                struct Field<T> {
                    name: PhantomData<T>,
                }
            );

            assert_token_stream_eq!(field.type_params(), { PhantomData<T>: ::core::fmt::Debug });
        }
    }
}
