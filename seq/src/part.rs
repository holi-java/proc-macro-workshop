#[cfg(test)]
use crate::stream::Stream;
use proc_macro2::{TokenStream, TokenTree};
use quote::TokenStreamExt;
use syn::{parse::Parse, Ident};

use crate::{
    compiler::{group, Compiler, Context},
    repeat::Repeat,
    then::Then,
    token::Token,
};

#[derive(Debug)]
pub(crate) struct Part {
    pub body: TokenStream,
}

impl Compiler for Part {
    type Output = Context<Token>;

    fn compile(&self, var: &Ident, once: bool) -> Self::Output {
        let mut tokens = vec![];
        compile(self.body.clone(), var, once, &mut tokens);
        return group(tokens);

        fn compile<S>(stream: S, var: &Ident, once: bool, tokens: &mut Vec<Context<Token>>)
        where
            S: IntoIterator<Item = TokenTree>,
        {
            use Token::*;
            let (mut prev, mut tilde) = (None, None);
            for tt in stream.into_iter() {
                match tt {
                    TokenTree::Ident(ident) if ident != *var && tilde.is_none() => prev
                        .replace(ident)
                        .then(|tt| tokens.push(Context::new(Tree(tt.into()), once))),
                    TokenTree::Punct(punct) if punct.as_char() == '~' => {
                        tilde
                            .replace(punct)
                            .then(|tt| tokens.push(Context::new(Tree(tt.into()), once)));
                        continue;
                    }
                    TokenTree::Ident(ident) if ident == *var => {
                        if tilde.take().is_some() {
                            tokens.push(Context::new(Var(prev.take(), ident), once));
                            continue;
                        }
                        prev.take()
                            .then(|prev| tokens.push(Context::new(Tree(prev.into()), once)));
                        tokens.push(Context::new(Var(None, ident), once));
                    }
                    TokenTree::Group(group) => {
                        let mut sub = vec![];
                        compile(group.stream(), var, once, &mut sub);

                        prev.take()
                            .then(|tt| tokens.push(Context::new(Tree(tt.into()), once)));
                        tilde
                            .take()
                            .then(|tt| tokens.push(Context::new(Tree(tt.into()), once)));
                        tokens.push(Context::new(
                            Group(group.delimiter(), group.span(), once, sub),
                            once,
                        ));
                    }
                    _ => {
                        prev.take()
                            .then(|tt| tokens.push(Context::new(Tree(tt.into()), once)));
                        tilde
                            .take()
                            .then(|tt| tokens.push(Context::new(Tree(tt.into()), once)));
                        tokens.push(Context::new(Tree(tt), once));
                    }
                }
            }
            prev.take()
                .then(|tt| tokens.push(Context::new(Tree(tt.into()), once)));
            tilde
                .take()
                .then(|tt| tokens.push(Context::new(Tree(tt.into()), once)));
        }
    }
}

impl Parse for Part {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut body = TokenStream::new();
        while !input.is_empty() {
            if input.peek(syn::Token![#]) && input.fork().parse::<Repeat>().is_ok() {
                break;
            }
            body.append(match input.fork().parse::<TokenTree>()? {
                TokenTree::Group(group) if syn::parse2::<Self>(group.stream()).is_err() => {
                    break;
                }
                _ => input.parse::<TokenTree>()?,
            });
        }
        Ok(Part { body })
    }
}

#[cfg(test)]
const _: () = {
    impl quote::ToTokens for Part {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.extend(self.body.stream())
        }
    }

    impl PartialEq for Part {
        fn eq(&self, other: &Self) -> bool {
            self.body.to_string() == other.body.to_string()
        }
    }
};
#[cfg(test)]
mod tests {
    use proc_macro2::{Delimiter, Span};
    use quote::quote;

    use crate::{
        assert_token_stream_eq,
        compiler::{group, once, repeat as repeated, Compiler, Eval},
        generate_parse_quote, generate_parse_str,
        token::Token::*,
    };

    generate_parse_str!(part, super::Part);
    generate_parse_quote!(tree, proc_macro2::TokenTree);
    generate_parse_quote!(ident, syn::Ident);

    #[test]
    fn parse_part() {
        let pound = quote!(#);
        assert_token_stream_eq!(part!(a b c).unwrap(), {a b c});
        assert_token_stream_eq!(part!(# a b c).unwrap(), {#pound a b c});
        assert_token_stream_eq!(part!(#(a b c)).unwrap(), {#pound (a b c)});
        assert_token_stream_eq!(part!(struct{}).unwrap(), { struct{} });
        assert_token_stream_eq!(part!(struct{ a }).unwrap(), { struct{ a } });
    }

    #[test]
    fn parse_fn() {
        let part = part!(
            fn f~N() -> u64 {
                N * 3
            }
        )
        .unwrap();

        assert_token_stream_eq!(part, {
            fn f~N() -> u64 {
                N * 3
            }
        });

        let context = part.compile(&ident!(N), false);

        assert_token_stream_eq!(context,{
            fn f~N() -> u64 {
                ~N * 3
            }
        });

        assert_token_stream_eq!(context.eval(2..3), {
            fn f2() -> u64 {
                2 * 3
            }
        });
    }

    #[test]
    fn eval_macro() {
        let pound = tree!(#);
        let part = part!(#[derive(Copy, Clone)]).unwrap();

        assert_token_stream_eq!(part, {#pound [derive(Copy, Clone)]});

        let context = part.compile(&ident!(N), false);

        assert_eq!(
            context,
            group([
                repeated(Tree(tree!(#))),
                repeated(Group(
                    Delimiter::Bracket,
                    Span::call_site(),
                    false,
                    vec![
                        repeated(Tree(tree!(derive))),
                        repeated(Group(
                            Delimiter::Parenthesis,
                            Span::call_site(),
                            false,
                            vec![
                                repeated(Tree(tree!(Copy))),
                                repeated(Tree(tree!(,))),
                                repeated(Tree(tree!(Clone))),
                            ]
                        ))
                    ]
                ))
            ])
        );

        assert_token_stream_eq!(context.eval(0..1), {#pound [derive(Copy, Clone)]});
    }

    #[test]
    fn fails_on_parse_repeat() {
        let result = part!(#(a)*);

        assert_eq!(result.unwrap_err().to_string(), "unexpected token");
    }

    #[test]
    fn fails_on_parse_repeat_at_end() {
        let result = part!(x #(a)*);

        assert_eq!(result.unwrap_err().to_string(), "unexpected token");
    }

    #[test]
    fn fails_on_parse_repeat_in_group() {
        let result = part!(x (#(a)*));

        assert_eq!(result.unwrap_err().to_string(), "unexpected token");
    }

    #[test]
    fn compile_var() {
        let part = part!(N).unwrap();
        assert_eq!(
            part.compile(&ident!(N), false),
            group([repeated(Var(None, ident!(N)))])
        );
        assert_eq!(
            part.compile(&ident!(B), false),
            group([repeated(Tree(tree!(N)))])
        );

        let part = part!(N B).unwrap();
        assert_eq!(
            part.compile(&ident!(N), false),
            group([repeated(Var(None, ident!(N))), repeated(Tree(tree!(B)))])
        );
        assert_eq!(
            part.compile(&ident!(B), false),
            group([repeated(Tree(tree!(N))), repeated(Var(None, ident!(B)))])
        );

        let part = part!(fn f~N).unwrap();
        assert_eq!(
            part.compile(&ident!(N), false),
            group([
                repeated(Tree(tree!(fn))),
                repeated(Var(Some(ident!(f)), ident!(N)))
            ])
        );
    }

    #[test]
    fn compile_var_followed_by_tilde() {
        let part = part!(~N).unwrap();
        assert_eq!(
            part.compile(&ident!(N), false),
            group([repeated(Var(None, ident!(N)))])
        );
        assert_eq!(
            part.compile(&ident!(B), false),
            group([repeated(Tree(tree!(~))), repeated(Tree(tree!(N)))])
        );

        let part = part!(~F N).unwrap();
        assert_eq!(
            part.compile(&ident!(N), false),
            group([
                repeated(Tree(tree!(~))),
                repeated(Tree(tree!(F))),
                repeated(Var(None, ident!(N)))
            ])
        );
    }

    #[test]
    fn compile_var_followed_by_tildes() {
        let part = part!(~~N).unwrap();
        assert_eq!(
            part.compile(&ident!(N), false),
            group([repeated(Tree(tree!(~))), repeated(Var(None, ident!(N)))])
        );
        assert_eq!(
            part.compile(&ident!(B), false),
            group([
                repeated(Tree(tree!(~))),
                repeated(Tree(tree!(~))),
                repeated(Tree(tree!(N)))
            ])
        );
    }

    #[test]
    fn compile_concat_var() {
        let part = part!(f~N).unwrap();

        assert_eq!(
            part.compile(&ident!(N), false),
            group([repeated(Var(Some(ident!(f)), ident!(N)))])
        );
        assert_eq!(
            part.compile(&ident!(B), false),
            group([
                repeated(Tree(tree!(f))),
                repeated(Tree(tree!(~))),
                repeated(Tree(tree!(N)))
            ])
        );
    }

    #[test]
    fn compile_grouped_var() {
        let part = part!((N)).unwrap();
        assert_eq!(
            part.compile(&ident!(N), false),
            group([repeated(Group(
                Delimiter::Parenthesis,
                Span::call_site(),
                false,
                vec![repeated(Var(None, ident!(N)))]
            ))])
        );
        assert_eq!(
            part.compile(&ident!(B), false),
            group([repeated(Group(
                Delimiter::Parenthesis,
                Span::call_site(),
                false,
                vec![repeated(Tree(tree!(N)))]
            ))])
        );
    }

    #[test]
    fn compile_once() {
        let part = part!(N).unwrap();
        assert_eq!(
            part.compile(&ident!(N), true),
            group([once(Var(None, ident!(N)))])
        );
        assert_eq!(
            part.compile(&ident!(B), true),
            group([once(Tree(tree!(N)))])
        );

        let part = part!((N B)).unwrap();
        assert_eq!(
            part.compile(&ident!(N), true),
            group([once(Group(
                Delimiter::Parenthesis,
                Span::call_site(),
                true,
                vec![once(Var(None, ident!(N))), once(Tree(tree!(B)))]
            ))])
        );
    }

    #[test]
    fn eval_var() {
        let var = Var(Some(ident!(f)), ident!(N));
        assert_token_stream_eq!(var.eval(2..3), { f2 });
    }
}
