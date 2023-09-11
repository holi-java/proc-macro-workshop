use proc_macro2::Ident;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Error, Expr, ExprLit, ExprRange, Lit, RangeLimits, Token,
};

use crate::{body::Body, compiler::Eval};

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
        let sections = self.body.compile(&self.ident);
        if let RangeLimits::Closed(_) = self.range.limits {
            tokens.extend(sections.into_iter().map(|sec| sec.eval(start..=end)));
        } else {
            tokens.extend(sections.into_iter().map(|sec| sec.eval(start..end)));
        }

        fn to_i32(expr: &Option<Box<Expr>>, default: i32) -> syn::Result<i32> {
            expr.as_ref()
                .map(|expr| match &**expr {
                    #[rustfmt::skip]
                    Expr::Lit(ExprLit { lit: Lit::Int(n), attrs: _, }) => n.base10_parse(),
                    _ => Err(Error::new(expr.span(), "supports integer only")),
                })
                .unwrap_or(Ok(default))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{assert_token_stream_eq, generate_parse_quote, seq::Seq};
    generate_parse_quote!(seq, super::Seq);

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

        let seq = seq!(
            N in 1..3 {
                N
            }
        );
        assert_token_stream_eq!(seq, { 1 2 });
    }

    #[test]
    fn seq_with_inclusive_range() {
        let seq = seq!(
            N in 0..=3 {
                N
            }
        );
        assert_token_stream_eq!(seq, {0 1 2 3});
    }

    #[test]
    fn seq_with_var_repeatedly() {
        let seq = seq!(N in 1..4 {
            fn f~N () -> u64 {
                N * 2
            }
        });

        assert_token_stream_eq!(seq, {
            fn f1() -> u64 {
                1 * 2
            }

            fn f2() -> u64 {
                2 * 2
            }

            fn f3() -> u64 {
                3 * 2
            }
        });
    }

    #[test]
    fn seq_with_repeat_pattern() {
        #[rustfmt::skip]
        let seq = syn::parse_str::<Seq>(
        "N in 0..3 {\
            struct Fields {\
                #(field~N: String,)*\
            }\
        }",
        )
        .unwrap();

        assert_token_stream_eq!(seq, {
            struct Fields {
                field0: String,
                field1: String,
                field2: String,
            }
        });
    }
}
