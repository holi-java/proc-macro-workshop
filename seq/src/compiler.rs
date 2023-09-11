use proc_macro2::{Ident, TokenStream};

pub trait Compiler {
    type Output;

    fn compile(&self, var: &Ident, once: bool) -> Self::Output;
}

impl<T: Compiler> Compiler for [T] {
    type Output = Vec<T::Output>;

    fn compile(&self, var: &Ident, once: bool) -> Self::Output {
        self.iter().map(|each| each.compile(var, once)).collect()
    }
}

type Once<T> = std::cell::RefCell<Option<T>>;

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Context<T> {
    Once(Once<T>),
    Repeat(T),
    Group(Vec<Self>),
}

impl<T> Context<T> {
    pub fn new(value: T, once: bool) -> Self {
        if once {
            self::once(value)
        } else {
            self::repeat(value)
        }
    }
}

impl<T: Eval> Eval for Context<T> {
    fn eval(&self, range: impl IntoIterator<Item = i32> + Clone) -> TokenStream {
        match self {
            Context::Once(item) => item
                .borrow_mut()
                .take()
                .map(|value| value.eval(range.clone()))
                .unwrap_or_default(),
            Context::Repeat(item) => item.eval(range),
            Context::Group(items) => range.into_iter().map(|n| items.eval(Some(n))).collect(),
        }
    }
}

pub fn once<T>(value: T) -> Context<T> {
    Context::Once(Once::new(Some(value)))
}

pub fn repeat<T>(value: T) -> Context<T> {
    Context::Repeat(value)
}

pub fn group<T, I: Into<Vec<Context<T>>>>(contexts: I) -> Context<T> {
    Context::Group(contexts.into())
}

pub trait Eval {
    fn eval(&self, range: impl IntoIterator<Item = i32> + Clone) -> TokenStream;
}

impl<T: Eval> Eval for [T] {
    fn eval(&self, range: impl IntoIterator<Item = i32> + Clone) -> TokenStream {
        self.iter().map(|each| each.eval(range.clone())).collect()
    }
}

#[cfg(test)]
const _: () = {
    use quote::ToTokens;

    use crate::stream::Stream;

    impl<T: ToTokens> ToTokens for Context<T> {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            match self {
                Context::Once(item) => tokens.extend(item.borrow().as_ref().stream()),
                Context::Repeat(item) => tokens.extend(item.stream()),
                Context::Group(items) => tokens.extend(items.stream()),
            }
        }
    }
};
