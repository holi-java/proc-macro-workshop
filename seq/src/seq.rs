use proc_macro2::{Group, Ident, Literal, TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Error, Expr, ExprLit, ExprRange, Lit, Token,
};

pub(crate) struct Seq {
    ident: Ident,
    range: ExprRange,
    body: Body,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
        let _ = input.parse::<Token![in]>()?;
        let range = input.parse::<ExprRange>()?;
        let body = input.parse::<Body>()?;

        Ok(Seq { ident, range, body })
    }
}

impl ToTokens for Seq {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let start = to_i32(&self.range.start, 0).unwrap();
        let end = to_i32(&self.range.end, i32::MAX).unwrap();
        tokens.extend(self.body.eval(&self.ident, start..end));

        fn to_i32(expr: &Option<Box<Expr>>, default: i32) -> syn::Result<i32> {
            expr.as_ref()
                .map(|expr| match &**expr {
                    Expr::Lit(ExprLit {
                        lit: Lit::Int(n),
                        attrs: _,
                    }) => n.base10_parse(),
                    _ => Err(Error::new(expr.span(), "supports integer only")),
                })
                .unwrap_or(Ok(default))
        }
    }
}

struct Body {
    body: TokenStream,
}
impl Body {
    pub fn eval<I>(&self, var: &Ident, range: I) -> TokenStream
    where
        I: IntoIterator<Item = i32>,
    {
        let mut result = TokenStream::new();
        for n in range {
            for tt in self.body.clone() {
                result.append(replace(tt, var, n));
            }
        }
        return result;

        fn replace(tt: TokenTree, var: &Ident, n: i32) -> TokenTree {
            match &tt {
                TokenTree::Ident(ident) if ident == var => {
                    let mut lit = Literal::i32_unsuffixed(n);
                    lit.set_span(tt.span());
                    lit.into()
                }
                TokenTree::Group(group) => {
                    let mut tokens = TokenStream::new();
                    for tt in group.stream() {
                        tokens.append(replace(tt, var, n));
                    }
                    let mut group = Group::new(group.delimiter(), tokens);
                    group.set_span(tt.span());
                    group.into()
                }
                _ => tt,
            }
        }
    }
}

impl Parse for Body {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let body;
        let _ = syn::braced!(body in input);
        Ok(Body {
            body: body.step(|cursor| {
                let mut body = TokenStream::new();
                let mut rest = *cursor;
                while let Some((tt, next)) = rest.token_tree() {
                    body.append(tt);
                    rest = next;
                }
                Ok((body, rest))
            })?,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{assert_token_stream_eq, generate_macro};
    generate_macro!(seq, super::Seq);
    generate_macro!(ident, syn::Ident);
    generate_macro!(body, super::Body);

    #[test]
    fn parse_seq() {
        let seq = seq!(
            N in 0..8 {
                // nothing
            }
        );

        assert_token_stream_eq!(seq.ident, { N });
        assert_token_stream_eq!(seq.range, { 0..8 });
    }

    #[test]
    fn seq_with_empty_body() {
        let seq = seq!(
            N in 0..8 {
                // nothing
            }
        );

        assert_token_stream_eq!(seq, {});
    }

    #[test]
    fn seq_with_var_only() {
        let seq = seq!(
            N in 0..3 {
                N
            }
        );

        assert_token_stream_eq!(seq, {0 1 2});
    }

    #[test]
    fn parse_body() {
        assert_token_stream_eq!(body!({}).body, {});
        assert_token_stream_eq!(body!({ N }).body, { N });
    }

    #[test]
    fn eval_body() {
        assert_token_stream_eq!(body!({}).eval(&ident!(N), 0..2), {});
        assert_token_stream_eq!(body!({N}).eval(&ident!(N), 0..2), {0 1});
    }

    #[test]
    fn eval_body_grouped_var() {
        assert_token_stream_eq!(body!({ (N) }).eval(&ident!(N), 0..2), { (0)(1) });
    }
}
