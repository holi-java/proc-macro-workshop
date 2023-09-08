use std::cell::RefCell;

use proc_macro2::{Delimiter, Group, Ident, Literal, Span, TokenStream, TokenTree};

pub(crate) trait Eval {
    fn eval(&self, n: i32) -> TokenStream;
}

type Once<T> = RefCell<Option<T>>;

#[derive(Debug)]
pub(crate) enum Section {
    Once(Once<Box<Section>>),
    Repeat(TokenStream),
    Var(Span),
    Concat(Ident),
    Group(Delimiter, Span, Vec<Section>),
}

impl Section {
    #[allow(dead_code)]
    pub fn once<T: Into<TokenStream>>(tokens: T) -> Self {
        Section::Once(Once::new(Some(Box::new(Self::repeat(tokens)))))
    }

    pub fn repeat<T: Into<TokenStream>>(tokens: T) -> Self {
        Section::Repeat(tokens.into())
    }

    pub fn group<S, I>(delimiter: Delimiter, span: S, sections: I) -> Self
    where
        I: IntoIterator<Item = Section>,
        S: Into<Span>,
    {
        Section::Group(delimiter, span.into(), Vec::from_iter(sections))
    }

    pub fn var<S: Into<Span>>(span: S) -> Self {
        Section::Var(span.into())
    }

    pub fn concat(ident: Ident) -> Self {
        Section::Concat(ident)
    }
}

impl Eval for Section {
    fn eval(&self, n: i32) -> TokenStream {
        return match self {
            Self::Once(tokens) => tokens
                .borrow_mut()
                .take()
                .map(|s| s.eval(n))
                .unwrap_or_else(Default::default),
            Self::Repeat(tokens) => tokens.clone(),
            Self::Var(span) => {
                let mut lit = Literal::i32_unsuffixed(n);
                lit.set_span(*span);
                TokenTree::Literal(lit).into()
            }
            Self::Concat(ident) => {
                TokenTree::Ident(Ident::new(&format!("{ident}{n}"), ident.span())).into()
            }
            Self::Group(delimiter, span, sections) => {
                let mut group = Group::new(*delimiter, sections.eval(n));
                group.set_span(span.clone());
                TokenTree::Group(group).into()
            }
        };
    }
}

impl Eval for [Section] {
    fn eval(&self, n: i32) -> TokenStream {
        self.iter().map(|s| s.eval(n)).collect()
    }
}

#[cfg(test)]
impl PartialEq for Section {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Once(lhs), Self::Once(rhs)) => {
                lhs.borrow().as_ref().map(|s| format!("{:?}", s))
                    == rhs.borrow().as_ref().map(|s| format!("{:?}", s))
            }
            (Self::Repeat(lhs), Self::Repeat(rhs)) => lhs.to_string() == rhs.to_string(),
            (Self::Var(lhs), Self::Var(rhs)) => lhs.source_text() == rhs.source_text(),
            (Self::Concat(lhs), Self::Concat(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Section;
    use crate::{assert_token_stream_eq, generate_macro, section::Eval};
    use proc_macro2::{Delimiter, Span, TokenStream};
    use quote::quote;

    generate_macro!(ident, syn::Ident);

    #[test]
    fn eval_once() {
        let section = Section::once(quote!(a));

        assert_token_stream_eq!(section.eval(1), { a });
        assert_token_stream_eq!(section.eval(1), {});
    }

    #[test]
    fn eval_repeat() {
        let section = repeat(quote!(N));

        assert_token_stream_eq!(section.eval(1), { N });
        assert_token_stream_eq!(section.eval(2), { N });
    }

    #[test]
    fn eval_var() {
        let span = Span::call_site();
        assert_token_stream_eq!(Section::var(span).eval(1), { 1 });
        assert_token_stream_eq!(Section::var(span).eval(2), { 2 });
    }

    #[test]
    fn eval_concat() {
        let section = Section::concat(ident!(f));
        assert_token_stream_eq!(section.eval(1), { f1 });
        assert_token_stream_eq!(section.eval(2), { f2 });
    }

    #[test]
    fn eval_sections() {
        let section = vec![Section::concat(ident!(f)), Section::once(quote!(b))];
        assert_token_stream_eq!(section.eval(1), { f1 b });
        assert_token_stream_eq!(section.eval(2), { f2 });
    }

    #[test]
    fn eval_group() {
        let section = Section::group(
            Delimiter::None,
            Span::call_site(),
            [Section::concat(ident!(f)), Section::once(quote!(b))],
        );
        assert_token_stream_eq!(section.eval(1), { f1 b });
        assert_token_stream_eq!(section.eval(2), { f2 });

        let section = Section::group(
            Delimiter::Parenthesis,
            Span::call_site(),
            [Section::Var(Span::call_site()), Section::once(quote!(b))],
        );
        assert_token_stream_eq!(section.eval(2), { (2 b) });
    }

    fn repeat<T: Into<TokenStream>>(tokens: T) -> Section {
        Section::repeat(tokens)
    }
}
