use proc_macro2::{Delimiter, Group, Ident, Literal, Span, TokenStream, TokenTree};
use quote::ToTokens;

use crate::compiler::{Context, Eval};

#[derive(Debug, Clone)]
pub(crate) enum Token {
    Var(Option<Ident>, Ident),
    Tree(TokenTree),
    Group(Delimiter, Span, bool, Vec<Context<Token>>),
}

impl Token {
    pub fn eval(&self, n: i32) -> TokenTree {
        match self {
            Token::Var(None, var) => {
                let mut lit = Literal::i32_unsuffixed(n);
                lit.set_span(var.span());
                lit.into()
            }
            Token::Var(Some(prefix), _) => {
                Ident::new(&format!("{prefix}{n}"), prefix.span()).into()
            }
            Token::Tree(tree) => tree.clone(),
            Token::Group(_, _, _, _) => unreachable!(),
        }
    }
}

impl Eval for Token {
    fn eval(&self, range: impl IntoIterator<Item = i32> + Clone) -> TokenStream {
        if let Token::Group(delim, span, inner, tokens) = self {
            let group = |tt| {
                let mut group = Group::new(*delim, tt);
                group.set_span(*span);
                TokenTree::Group(group)
            };
            // { #(...)* }
            if *inner {
                return group(range.into_iter().map(|n| tokens.eval(Some(n))).collect())
                    .into_token_stream();
            }
            // { ... }
            return range
                .into_iter()
                .map(|n| group(tokens.eval(Some(n))))
                .collect();
        }

        range.into_iter().map(|n| self.eval(n)).collect()
    }
}

#[cfg(test)]
const _: () = {
    use quote::TokenStreamExt;
    use syn::Token;

    use crate::stream::Stream;

    impl ToTokens for Token {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                Token::Var(Some(prefix), var) => {
                    tokens.append_separated([prefix.clone(), var.clone()], token_of::<Token![~]>())
                }
                Token::Var(_, var) => {
                    tokens.extend(token_of::<Token![~]>().into_token_stream());
                    tokens.append(var.clone());
                }
                Token::Tree(tt) => tokens.append(tt.clone()),
                Token::Group(delim, span, _, body) => {
                    let mut group = Group::new(*delim, body.stream());
                    group.set_span(*span);
                    tokens.append(TokenTree::Group(group));
                }
            }
        }
    }

    fn token_of<T: Default>() -> T {
        Default::default()
    }

    impl PartialEq for Token {
        fn eq(&self, other: &Self) -> bool {
            return match (self, other) {
                (Self::Var(l0, l1), Self::Var(r0, r1)) => l0 == r0 && l1 == r1,
                (Self::Tree(l0), Self::Tree(r0)) => eq(l0, r0),
                (Self::Group(d1, _, i1, lhs), Self::Group(d2, _, i2, rhs)) => {
                    d1 == d2 && i1 == i2 && eq(lhs.stream(), rhs.stream())
                }
                _ => false,
            };

            fn eq<T>(lhs: T, rhs: T) -> bool
            where
                T: Stream,
            {
                lhs.stream().to_string() == rhs.stream().to_string()
            }
        }
    }
};

#[cfg(test)]
mod tests {
    use super::Token::*;
    use crate::compiler::{repeat as repeated, Eval};
    use crate::{assert_token_stream_eq, generate_parse_quote};
    use proc_macro2::{Delimiter, Span};

    generate_parse_quote!(ident, syn::Ident);
    generate_parse_quote!(tree, proc_macro2::TokenTree);

    #[test]
    fn to_tokens() {
        assert_token_stream_eq!(Var(ident!(f).into(), ident!(N)), { f~N });
        assert_token_stream_eq!(Var(None, ident!(N)), { ~N });
        assert_token_stream_eq!(
            Group(Delimiter::Parenthesis, Span::call_site(), false, vec![
                repeated(Tree(tree!(<))),
                repeated(Var(None, ident!(N)))
            ]),
            {( < ~ N )}
        );
    }

    #[test]
    fn eval_vars() {
        assert_token_stream_eq!(Var(None, ident!(N)).eval(1), { 1 });
        assert_token_stream_eq!(Var(Some(ident!(f)), ident!(N)).eval(2), { f2 });
    }

    #[test]
    fn eval_token_tree() {
        assert_token_stream_eq!(Tree(tree!(a)).eval(1), { a });
    }

    #[test]
    fn eval_group() {
        let group = Group(
            Delimiter::Bracket,
            Span::call_site(),
            false,
            vec![
                repeated(Tree(tree!(a))),
                repeated(Tree(tree!(,))),
                repeated(Var(None, ident!(N))),
            ],
        );
        assert_token_stream_eq!(Eval::eval(&group, 1..=1), { [a, 1] });
    }
}
