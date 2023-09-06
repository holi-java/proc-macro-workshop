use std::cell::RefCell;
use std::collections::HashSet;

use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{PathArguments, Type};

pub(crate) struct Generics<'a> {
    inner: &'a syn::Generics,
    processed: RefCell<HashSet<Ident>>,
}

impl<'a> Generics<'a> {
    pub fn new(inner: &'a syn::Generics) -> Self {
        Generics {
            inner,
            processed: Default::default(),
        }
    }

    pub fn type_params(&self) -> TokenStream {
        let type_params = self.inner.type_params();
        let type_params = quote!(#(#type_params),*);
        if !type_params.is_empty() {
            return quote!(<#type_params>);
        }
        type_params
    }

    fn is_generic_arg(&self, ident: &Ident) -> bool {
        self.inner
            .type_params()
            .any(|p| p.ident == *ident && self.processed.borrow_mut().insert(ident.clone()))
    }

    pub fn type_param_bounds(&self, ty: &Type) -> TokenStream {
        return collect_type_param_bounds(self, ty, ty);
        fn collect_type_param_bounds(context: &Generics, ty: &Type, root: &Type) -> TokenStream {
            match ty {
                Type::Path(path) => {
                    let mut tokens = TokenStream::new();
                    for seg in &path.path.segments {
                        let token = match &seg.arguments {
                            PathArguments::None if context.is_generic_arg(&seg.ident) => {
                                quote!(#root: ::core::fmt::Debug)
                            }
                            PathArguments::AngleBracketed(args) => args
                                .args
                                .iter()
                                .map(|arg| match arg {
                                    syn::GenericArgument::Type(generic) => {
                                        collect_type_param_bounds(context, generic, root)
                                    }
                                    _ => todo!(),
                                })
                                .collect(),
                            _ => TokenStream::new(),
                        };
                        tokens.extend(token);
                    }
                    tokens
                }
                Type::Reference(tref) => collect_type_param_bounds(context, &tref.elem, root),
                Type::Tuple(tuple) => {
                    let bounds = tuple
                        .elems
                        .iter()
                        .map(|ty| collect_type_param_bounds(context, ty, root))
                        .filter(|t| !t.is_empty());
                    quote!(#(#bounds),*)
                }
                _ => todo!("unsupported type: {:?}", core::mem::discriminant(ty)),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Generics;
    use crate::{assert_token_stream_eq, generate_macro};
    generate_macro!(type_of, syn::Type);
    generate_macro!(generics, syn::Generics);
    macro_rules! generics_of {
        ($name: ident, $($tt:tt)*) => {
            let $name = generics!($($tt)*);
            let $name = Generics::new(&$name);
        };
    }

    #[test]
    fn raw_type() {
        generics_of!(g, <T>);
        let ty = type_of!(String);
        assert_token_stream_eq!(g.type_param_bounds(&ty), {});
    }

    #[test]
    fn generic_without_bounds() {
        generics_of!(g, <T>);
        let ty = type_of!(T);

        assert_token_stream_eq!(g.type_param_bounds(&ty), { T: ::core::fmt::Debug });
    }

    #[test]
    fn generic_refernce_without_bounds() {
        generics_of!(g, <T>);
        let ty = type_of!(&T);

        assert_token_stream_eq!(g.type_param_bounds(&ty), { &T: ::core::fmt::Debug });
    }

    #[test]
    fn generic_tuple_without_bounds() {
        generics_of!(g, <T, U>);
        assert_token_stream_eq!(
            g.type_param_bounds(&type_of!((T, U))),
            { (T, U): ::core::fmt::Debug, (T, U): ::core::fmt::Debug }
        );

        generics_of!(g, <T, U>);
        assert_token_stream_eq!(
            g.type_param_bounds(&type_of!((T, u32))),
            { (T, u32): ::core::fmt::Debug }
        );
    }

    #[test]
    fn generic_with_bounds() {
        generics_of!(g, <T: Trait>);
        assert_token_stream_eq!(
            g.type_param_bounds(&type_of!(T)),
            { T: ::core::fmt::Debug }
        );
    }

    #[test]
    fn generate_type_param_bounds_once() {
        generics_of!(g, <T>);
        let ty = type_of!(T);

        assert_token_stream_eq!(
            g.type_param_bounds(&ty),
            { T: ::core::fmt::Debug }
        );

        assert_token_stream_eq!(g.type_param_bounds(&ty), {});
    }

    #[test]
    fn nested_generic_type() {
        generics_of!(g, <T>);
        let ty = type_of!(::core::marker::PhantomData<T>);
        assert_token_stream_eq!(
            g.type_param_bounds(&ty),
            { ::core::marker::PhantomData<T>: ::core::fmt::Debug }
        );
    }

    #[test]
    fn associated_type() {
        generics_of!(g, <T: Trait>);
        let ty = type_of!(T::Value);
        assert_token_stream_eq!(
            g.type_param_bounds(&ty),
            { T::Value: ::core::fmt::Debug }
        );
    }

    #[test]
    fn nested_associated_type() {
        generics_of!(g, <T: Trait>);
        let ty = type_of!(Box<T::Value>);
        assert_token_stream_eq!(
            g.type_param_bounds(&ty),
            { Box<T::Value>: ::core::fmt::Debug }
        );
    }
}
