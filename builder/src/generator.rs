use crate::field::Field;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{parse::Parse, Data, DeriveInput};

pub(crate) struct Generator {
    name: Ident,
    builder_name: Ident,
    fields: Vec<Field>,
}

impl Generator {
    fn builder_method(&self) -> TokenStream {
        let (name, builder_name) = (&self.name, &self.builder_name);
        let inits = self.fields.iter().map(Field::to_builder_init_token_stream);
        quote!(impl #name {
            pub fn builder() -> #builder_name {
                #builder_name{
                    #(#inits,)*
                }
            }
        })
    }

    fn builder(&self) -> TokenStream {
        let (name, imp) = (&self.builder_name, self.impl_builder());
        let fields = &self.fields;
        quote!(
            pub struct #name {
                #(#fields,)*
            }
            #imp
        )
    }

    fn impl_builder(&self) -> TokenStream {
        let (name, builder_name) = (&self.name, &self.builder_name);
        let setters = self.fields.iter().map(Field::setter);
        let build_method = {
            let inits = self.fields.iter().map(Field::to_init_token_stream);
            quote!(
                pub fn build(&self) -> ::core::result::Result<#name, String> {
                    ::core::result::Result::Ok(#name { #(#inits,)* })
                }
            )
        };
        quote!(
            impl #builder_name {
                #(#setters)*
                #build_method
            }
        )
    }
}

impl Parse for Generator {
    fn parse(source: syn::parse::ParseStream) -> syn::Result<Self> {
        let input = source.parse::<DeriveInput>()?;
        let name = input.ident.clone();
        let fields = match input.data {
            Data::Struct(s) => s.fields,
            _ => return Err(source.error("unsupported")),
        };
        Ok(Generator {
            builder_name: format_ident!("{}Builder", name),
            name,
            fields: fields.iter().map(|f| Field::from(f.clone())).collect(),
        })
    }
}

impl ToTokens for Generator {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend([self.builder_method(), self.builder()]);
    }
}

impl From<Generator> for TokenStream {
    fn from(generator: Generator) -> Self {
        generator.into_token_stream()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_token_stream_eq;

    #[test]
    fn empty_struct() {
        let g: Generator = syn::parse_quote!(
            #[derive(Builder)]
            pub struct Command;
        );

        assert_eq!(g.name, "Command");
        assert_eq!(g.builder_name, "CommandBuilder");
        assert!(g.fields.is_empty());
        assert_token_stream_eq!(g.builder_method(), {
            impl Command {
                pub fn builder() -> CommandBuilder {
                    CommandBuilder {}
                }
            }
        });
        assert_token_stream_eq!(g.builder(), {
            pub struct CommandBuilder {}

            impl CommandBuilder {
                pub fn build(&self) -> ::core::result::Result<Command, String> {
                    ::core::result::Result::Ok(Command {})
                }
            }
        });
    }

    #[test]
    fn struct_with_mandated_fields() {
        let g: Generator = syn::parse_quote!(
            #[derive(Builder)]
            pub struct Command {
                work_dir: String,
            }
        );

        assert_token_stream_eq!(
            g.fields.first(),
            { work_dir: ::core::option::Option<String> }
        );
        assert_token_stream_eq!(g.builder_method(), {
            impl Command {
                pub fn builder() -> CommandBuilder {
                    CommandBuilder {
                        work_dir: ::core::option::Option::None,
                    }
                }
            }
        });

        assert_token_stream_eq!(g.builder(), {
            pub struct CommandBuilder {
                work_dir: ::core::option::Option<String>,
            }

            impl CommandBuilder {
                pub fn work_dir(&mut self, work_dir: String) -> &mut Self {
                    self.work_dir = ::core::option::Option::Some(work_dir);
                    self
                }

                pub fn build(&self) -> ::core::result::Result<Command, String> {
                    ::core::result::Result::Ok(Command {
                        work_dir: self
                            .work_dir
                            .clone()
                            .ok_or(format!("field missing: `{}`", stringify!(work_dir)))?,
                    })
                }
            }
        });
    }

    #[test]
    fn struct_with_option_fields() {
        let g: Generator = syn::parse_quote!(
            #[derive(Builder)]
            pub struct Command {
                work_dir: Option<String>,
            }
        );

        assert_token_stream_eq!(
            g.fields.first(),
            {work_dir: ::core::option::Option<String>}
        );
        assert_token_stream_eq!(g.builder_method(), {
            impl Command {
                pub fn builder() -> CommandBuilder {
                    CommandBuilder {
                        work_dir: ::core::option::Option::None,
                    }
                }
            }
        });
        assert_token_stream_eq!(g.builder(), {
            pub struct CommandBuilder {
                work_dir: ::core::option::Option<String>,
            }

            impl CommandBuilder {
                pub fn work_dir(&mut self, work_dir: String) -> &mut Self {
                    self.work_dir = ::core::option::Option::Some(work_dir);
                    self
                }

                pub fn build(&self) -> ::core::result::Result<Command, String> {
                    ::core::result::Result::Ok(Command {
                        work_dir: self.work_dir.clone(),
                    })
                }
            }
        });
    }
}
