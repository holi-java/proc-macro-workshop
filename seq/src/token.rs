use proc_macro2::{Delimiter, Group, Ident, Literal, Span, TokenStream, TokenTree};
use quote::ToTokens;
use quote::TokenStreamExt;

use crate::compiler::{Context, Eval, Value};

#[derive(Debug, Clone)]
pub(crate) enum Token {
    Var(Option<Ident>, Ident),
    Tree(TokenTree),
    Group(Value<Delimiter>, Span, Vec<Context<Token>>),
}

impl Eval for Token {
    fn eval(&self, range: impl IntoIterator<Item = i32> + Clone) -> TokenStream {
        match self {
            Token::Var(None, var) => {
                let mut tokens = TokenStream::new();
                for n in range {
                    let mut lit = Literal::i32_unsuffixed(n);
                    lit.set_span(var.span());
                    tokens.append(TokenTree::Literal(lit));
                }
                tokens
            }
            Token::Var(Some(prefix), _) => {
                let mut tokens = TokenStream::new();
                for n in range {
                    tokens.append(Ident::new(&format!("{prefix}{n}"), prefix.span()));
                }
                tokens
            }
            Token::Tree(tree) => {
                let mut tokens = TokenStream::new();
                for _ in range {
                    tokens.append(tree.clone());
                }
                tokens
            }
            Token::Group(delim, span, tokens) => {
                if let (Some(delim), inner) = (delim.get(), delim.get().is_some()) {
                    let mut body = TokenStream::new();
                    if inner {
                        for n in range {
                            let mut group = Group::new(delim, tokens.eval(n..n + 1));
                            group.set_span(*span);
                            body.append(group);
                        }
                        return body;
                    }
                    for n in range {
                        body.extend(tokens.eval(n..n + 1));
                    }
                    let mut group = Group::new(delim, body);
                    group.set_span(*span);
                    return group.into_token_stream();
                }
                todo!()
            }
        }
    }
}

#[cfg(test)]
const _: () = {
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
                Token::Group(delim, span, body) => {
                    let mut group = Group::new(delim.get().unwrap(), body.stream());
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
                (Self::Group(d1, _, lhs), Self::Group(d2, _, rhs)) => {
                    d1 == d2 && eq(lhs.stream(), rhs.stream())
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
    use crate::compiler::{repeat as repeated, Eval, Value};
    use crate::{assert_token_stream_eq, generate_parse_quote};
    use proc_macro2::{Delimiter, Span};

    generate_parse_quote!(ident, syn::Ident);
    generate_parse_quote!(tree, proc_macro2::TokenTree);

    #[test]
    fn to_tokens() {
        assert_token_stream_eq!(Var(ident!(f).into(), ident!(N)), { f~N });
        assert_token_stream_eq!(Var(None, ident!(N)), { ~N });
        assert_token_stream_eq!(
            Group(Value::new(Delimiter::Parenthesis, false), Span::call_site(), vec![
                repeated(Tree(tree!(<))),
                repeated(Var(None, ident!(N)))
            ]),
            {( < ~ N )}
        );
    }

    #[test]
    fn eval_vars() {
        assert_token_stream_eq!(Var(None, ident!(N)).eval(1..2), { 1 });
        assert_token_stream_eq!(Var(Some(ident!(f)), ident!(N)).eval(2..3), { f2 });
    }

    #[test]
    fn eval_token_tree() {
        assert_token_stream_eq!(Tree(tree!(a)).eval(1..2), { a });
    }

    #[test]
    fn eval_group() {
        let group = Group(
            Value::new(Delimiter::Bracket, false),
            Span::call_site(),
            vec![
                repeated(Tree(tree!(a))),
                repeated(Tree(tree!(,))),
                repeated(Var(None, ident!(N))),
            ],
        );
        assert_token_stream_eq!(group.eval(1..2), { [a, 1] });
    }
}
