use std::ops::Deref;

use proc_macro2::TokenStream;
use quote::{quote_spanned, ToTokens};
use syn::{
    parse::Parse, spanned::Spanned, Error, Expr, ExprLit, GenericArgument, Ident, Lit,
    MetaNameValue, PathArguments, Type,
};

use crate::setter::Setter;

pub(crate) struct Field {
    pub inner: syn::Field,
}

impl Field {
    pub fn name(&self) -> Option<&Ident> {
        self.inner.ident.as_ref()
    }

    pub fn ty(&self) -> &Type {
        self.raw_type().unwrap_or(self.field_type())
    }

    pub fn field_type(&self) -> &Type {
        &self.inner.ty
    }

    pub fn raw_type(&self) -> Option<&Type> {
        if let Type::Path(path) = self.field_type() {
            let path = path.path.segments.last()?;
            if path.ident == "Option" {
                if let PathArguments::AngleBracketed(ref args) = path.arguments {
                    if let syn::GenericArgument::Type(ty) = args.args.first()? {
                        return Some(ty);
                    }
                }
            }
        }
        None
    }

    pub fn to_init_token_stream(&self) -> TokenStream {
        let name = self.name();
        let init = {
            let init = quote_spanned!(self.inner.span() => self.#name.clone());
            if self.each().is_ok_and(|each| each.is_some()) {
                quote_spanned!(self.inner.span() => #init.unwrap_or_default())
            } else if self.raw_type().is_none() {
                quote_spanned!( self.inner.span() => 
                    #init.ok_or(format!("field missing: `{}`", stringify!(#name)))?)
            } else {
                init
            }
        };
        quote_spanned!(self.inner.span() => #name: #init)
    }

    pub fn to_builder_init_token_stream(&self) -> TokenStream {
        let name = self.name();
        quote_spanned!(self.inner.span() => #name: ::core::option::Option::None)
    }

    pub fn setter(&self) -> Setter {
        Setter::from(self)
    }

    pub fn each(&self) -> Result<Option<(Ident, Type)>, Error> {
        let error = |err| Error::new_spanned(self.field_type(), err);

        fn attr_error<S: Spanned>(s: S) -> Error {
            Error::new(s.span(), r#"expected `builder(each = "...")`"#)
        }

        for attr in &self.inner.attrs {
            if attr.meta.path().is_ident("builder") {
                let context = &attr.meta;
                let meta = context.require_list().map_err(|_| attr_error(context))?;
                let meta = syn::parse2::<MetaNameValue>(meta.tokens.clone())
                    .map_err(|_| attr_error(context))?;
                if !meta.path.is_ident("each") {
                    return Err(attr_error(context));
                }
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(name),
                    attrs: _,
                }) = &meta.value
                {
                    if let Type::Path(path) = self.ty() {
                        let seg = path.path.segments.last().unwrap();
                        if seg.ident != "Vec" {
                            return Err(error(&format!(
                                "#[each] unsupports type: `{}`",
                                seg.ident
                            )));
                        }
                        if let PathArguments::AngleBracketed(args) = &seg.arguments {
                            let ty = args.args.first().unwrap();
                            if let GenericArgument::Type(ty) = ty {
                                let name = Ident::new(&name.value(), self.inner.span());
                                return Ok(Some((name, ty.clone())));
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
    }
}

impl Parse for Field {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Field {
            inner: syn::Field::parse_named(input)?,
        })
    }
}

impl ToTokens for Field {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let (name, ty) = (self.name(), self.ty());
        tokens.extend(quote_spanned!(self.inner.span() => #name: ::core::option::Option<#ty>));
    }
}

impl From<syn::Field> for Field {
    fn from(field: syn::Field) -> Self {
        Field { inner: field }
    }
}

impl Deref for Field {
    type Target = syn::Field;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_token_stream_eq;

    #[test]
    fn generate_field_declaration() {
        let field: Field = syn::parse_quote!(s: String);
        assert_eq!(field.name().unwrap(), "s");
        assert_token_stream_eq!(field, {s: ::core::option::Option<String>});
    }

    #[test]
    fn generate_init() {
        let field: Field = syn::parse_quote!(s: String);
        assert_token_stream_eq!(field.to_init_token_stream(), {
            s: self.s.clone().ok_or(format!("field missing: `{}`", stringify!(s)))?
        });
    }

    #[test]
    fn generate_init_for_each_attr() {
        let field: Field = syn::parse_quote!(#[builder(each = "arg")] args: Vec<String>);
        assert_token_stream_eq!(field.to_init_token_stream(), {
            args: self.args.clone().unwrap_or_default()
        });
    }

    #[test]
    fn generate_builder_init() {
        let field: Field = syn::parse_quote!(s: String);
        assert_token_stream_eq!(field.to_builder_init_token_stream(), {
            s: ::core::option::Option::None
        });
    }

    #[test]
    fn generate_option_field_token_stream() {
        let field: Field = syn::parse_quote!(s: Option<String>);
        assert_token_stream_eq!(field, { s: ::core::option::Option<String> });
        assert_token_stream_eq!(field.to_init_token_stream(), { s: self.s.clone() });
        assert_token_stream_eq!(field.to_builder_init_token_stream(), { s: ::core::option::Option::None });
    }

    #[test]
    fn full_qualified_option_field() {
        let field: Field = syn::parse_quote!(s: std::option::Option<String>);
        assert_token_stream_eq!(field, { s: ::core::option::Option<String> });
        assert_token_stream_eq!(field.to_init_token_stream(), { s: self.s.clone() });
        assert_token_stream_eq!(field.to_builder_init_token_stream(), { s: ::core::option::Option::None });
    }

    #[test]
    fn raw_type() {
        let field: Field = syn::parse_quote!(s: String);
        assert_eq!(field.raw_type(), None);

        let field: Field = syn::parse_quote!(s: Option<String>);
        assert_token_stream_eq!(field.raw_type(), { String });
    }

    #[test]
    fn each() {
        let field: Field = syn::parse_quote!(s: String);
        assert_eq!(field.each().unwrap(), None);

        let field: Field = syn::parse_quote!(#[builder(each = "arg")] args: Vec<&'static str>);
        let (name, ty) = field.each().unwrap().unwrap();
        assert_token_stream_eq!(name, { arg });
        assert_token_stream_eq!(ty, { &'static str });
    }

    #[test]
    fn each_with_unsupported_field_type() {
        let field: Field = syn::parse_quote!(#[builder(each = "arg")] args: String);
        assert_eq!(
            field.each().unwrap_err().to_string(),
            "#[each] unsupports type: `String`"
        );
    }

    #[test]
    fn invalid_each_attr() {
        let field: Field = syn::parse_quote!(#[builder(each)] args: Vec<String>);
        assert_eq!(
            field.each().unwrap_err().to_string(),
            r#"expected `builder(each = "...")`"#
        );
    }

    #[test]
    fn unknown_attr() {
        let field: Field = syn::parse_quote!(#[builder(eac = "arg")] args: Vec<String>);
        assert_eq!(
            field.each().unwrap_err().to_string(),
            r#"expected `builder(each = "...")`"#
        );
    }
}
