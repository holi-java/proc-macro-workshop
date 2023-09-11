use proc_macro2::Ident;
use syn::{
    parse::{Parse, ParseStream},
    Result, Token,
};

use crate::{compiler::Compiler, part::Part};

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub(crate) struct Repeat {
    pub body: Part,
}

impl Parse for Repeat {
    fn parse(input: ParseStream) -> Result<Self> {
        {
            let _body;
            let fork = input.fork();
            fork.parse::<Token![#]>()?;
            syn::parenthesized!(_body in fork);
            fork.parse::<Token![*]>()?;
        }

        let body;
        input.parse::<Token![#]>()?;
        syn::parenthesized!(body in input);
        input.parse::<Token![*]>()?;
        Ok(Repeat {
            body: Part::parse(&body)?,
        })
    }
}

impl Compiler for Repeat {
    type Output = <Part as Compiler>::Output;

    fn compile(&self, var: &Ident, _: bool) -> Self::Output {
        self.body.compile(var, false)
    }
}

#[cfg(test)]
const _: () = {
    use quote::ToTokens;

    impl ToTokens for Repeat {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            tokens.extend(self.body.to_token_stream());
        }
    }
};

#[cfg(test)]
mod tests {
    use crate::{
        compiler::{group, repeat as repeated, Compiler},
        generate_parse_quote, generate_parse_str,
    };

    use super::Repeat;
    use crate::token::Token::*;

    generate_parse_str!(repeat, Repeat);
    generate_parse_quote!(ident, proc_macro2::Ident);
    generate_parse_quote!(tree, proc_macro2::TokenTree);

    #[test]
    fn parse_repeat() {
        let _ = repeat!(#(a b c)*).unwrap();
    }

    #[test]
    fn parse_repeat_error() {
        let err = repeat!(#(a b)).unwrap_err();
        assert_eq!(err.to_string(), "expected `*`", "{:#?}", err);
    }

    #[test]
    fn parse_repeat_with_brackets_error() {
        let err = repeat!(#[a b]*).unwrap_err();
        assert_eq!(err.to_string(), "expected parentheses", "{:#?}", err);
    }

    #[test]
    fn prase_nested_repeat_error() {
        let err = repeat!(#(#(b)*)*).unwrap_err();
        assert_eq!(err.to_string(), "unexpected token", "{:#?}", err);
    }

    #[test]
    fn compile_repeat() {
        let repeat = repeat!(#(a b c)*).unwrap();

        assert_eq!(
            repeat.compile(&ident!(b), false),
            group([
                repeated(Tree(tree!(a))),
                repeated(Var(None, ident!(b))),
                repeated(Tree(tree!(c)))
            ])
        );
    }

    #[test]
    fn compile_once() {
        let b = ident!(b);
        let repeat = repeat!(#(a b c)*).unwrap();

        assert_eq!(repeat.compile(&b, true), repeat.compile(&b, false));
    }
}
