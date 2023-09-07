use proc_macro2::Ident;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    Block, ExprRange, Token,
};

pub(crate) struct Seq {
    ident: Ident,
    sep: Token![in],
    range: ExprRange,
    body: Body,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
        let sep = input.parse::<Token![in]>()?;
        let range = input.parse::<ExprRange>()?;
        let body = input.parse::<Body>()?;

        Ok(Seq {
            ident,
            sep,
            range,
            body,
        })
    }
}

impl ToTokens for Seq {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {}
}

struct Body {
    block: Block,
}

impl Parse for Body {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Body {
            block: input.parse::<Block>()?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::Seq;
    use crate::assert_token_stream_eq;
    use quote::{quote, ToTokens};

    #[test]
    fn parse_seq() {
        let seq = syn::parse2::<Seq>(
            quote!(N in 0..8 {
                // nothing
            })
            .into_token_stream(),
        )
        .unwrap();

        assert_token_stream_eq!(seq.ident, { N });
        assert_token_stream_eq!(seq.sep, {in});
        assert_token_stream_eq!(seq.range, { 0..8 });
    }
}
