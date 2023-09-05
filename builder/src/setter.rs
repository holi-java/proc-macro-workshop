use proc_macro2::TokenStream;
use quote::{quote_spanned, ToTokens};
use syn::spanned::Spanned;

use crate::field::Field;

pub(crate) struct Setter<'a> {
    field: &'a Field,
}

impl<'a> ToTokens for Setter<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        macro_rules! setter {
            ($name: ident, $ty: ident) => {
                quote_spanned!(self.field.span() =>
                    pub fn #$name(&mut self, #$name: #$ty) -> &mut Self {
                        self.#$name = ::core::option::Option::Some(#$name);
                        self
                    }
                )
            }
        }

        let (name, ty) = (self.field.name(), self.field.ty());
        match self.field.each() {
            Ok(None) => tokens.extend(setter!(name, ty)),
            Ok(Some((each, item_ty))) => {
                if name.is_some_and(|name| name != &each) {
                    tokens.extend(setter!(name, ty));
                }
                tokens.extend(quote_spanned!(self.field.span() =>
                    pub fn #each(&mut self, #each: #item_ty) -> &mut Self {
                        self.#name.get_or_insert_with(::core::default::Default::default).push(#each);
                        self
                    }
                ));
            }
            Err(err) => tokens.extend(err.into_compile_error()),
        }
    }
}

impl<'a> From<&'a Field> for Setter<'a> {
    fn from(field: &'a Field) -> Self {
        Setter { field }
    }
}

#[cfg(test)]
mod tests {
    use crate::assert_token_stream_eq;
    macro_rules! setter {
        ($name:ident = $($tt:tt)*) => {
            use $crate::field::Field;
            let field: Field = syn::parse_quote!($($tt)*);
            let $name = field.setter();
        };
    }

    #[test]
    fn generate_setter_for_non_optional_field() {
        setter!(set = dir: String);
        assert_token_stream_eq!(set, {
            pub fn dir(&mut self, dir: String) -> &mut Self {
                self.dir = ::core::option::Option::Some(dir);
                self
            }
        });
    }

    #[test]
    fn generate_setter_for_optional_field() {
        setter!(set = dir: Option<String>);
        assert_token_stream_eq!(set, {
            pub fn dir(&mut self, dir: String) -> &mut Self {
                self.dir = ::core::option::Option::Some(dir);
                self
            }
        });
    }

    #[test]
    fn generate_setter_for_field_with_each_attr() {
        setter!(set = #[builder(each = "arg")] args: Vec<String>);
        assert_token_stream_eq!(set, {
            pub fn args(&mut self, args: Vec<String>) -> &mut Self {
                self.args = ::core::option::Option::Some(args);
                self
            }
            pub fn arg(&mut self, arg: String) -> &mut Self {
                self.args
                    .get_or_insert_with(::core::default::Default::default)
                    .push(arg);
                self
            }
        });
    }

    #[test]
    fn generate_setter_for_field_with_each_attr_with_repeated_name() {
        setter!(set = #[builder(each = "args")] args: Vec<String>);
        assert_token_stream_eq!(set, {
            pub fn args(&mut self, args: String) -> &mut Self {
                self.args
                    .get_or_insert_with(::core::default::Default::default)
                    .push(args);
                self
            }
        });
    }
}
