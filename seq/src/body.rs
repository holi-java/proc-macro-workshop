use proc_macro2::{Delimiter, Group as SynGroup, Ident, Span};
use syn::parse::{Parse, ParseStream};

use crate::{
    compiler::{repeat, Compiler, Context},
    part::Part,
    repeat::Repeat,
    token::Token,
};

pub(crate) struct Body {
    parts: Vec<BodyPart>,
}

impl Body {
    pub fn compile(&self, var: &Ident) -> Vec<Context<Token>> {
        self.parts
            .compile(var, self.parts.iter().any(BodyPart::has_repeat))
    }
}

impl Parse for Body {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let body;
        syn::braced!(body in input);
        let mut parts = vec![];
        while !body.is_empty() {
            parts.push(body.parse()?);
        }
        Ok(Body { parts })
    }
}

#[derive(Debug)]
pub(crate) enum BodyPart {
    // Normal
    Normal(Part),
    // #(..)*
    Repeat(Repeat),
    // (..), {..}, [..]
    Group(Delimiter, Span, Group),
}

impl BodyPart {
    pub fn has_repeat(&self) -> bool {
        match self {
            BodyPart::Normal(_) => false,
            BodyPart::Repeat(_) => true,
            BodyPart::Group(_, _, group) => group.has_repeat(),
        }
    }
}

impl Parse for BodyPart {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(group) = input.parse::<SynGroup>() {
            return Ok(Self::Group(
                group.delimiter(),
                group.span(),
                syn::parse2(group.stream())?,
            ));
        }
        if let Ok(repeat) = input.parse::<Repeat>() {
            return Ok(Self::Repeat(repeat));
        }
        Ok(Self::Normal(input.parse::<Part>()?))
    }
}

impl Compiler for BodyPart {
    type Output = Context<Token>;

    fn compile(&self, var: &Ident, once: bool) -> Self::Output {
        match self {
            BodyPart::Normal(part) => part.compile(var, once),
            BodyPart::Repeat(part) => part.compile(var, once),
            BodyPart::Group(delim, span, group) => {
                repeat(Token::Group(*delim, *span, once, group.compile(var, once)))
            }
        }
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub(crate) struct Group {
    parts: Vec<BodyPart>,
}

impl Group {
    pub fn has_repeat(&self) -> bool {
        self.parts.iter().any(BodyPart::has_repeat)
    }
}

impl Compiler for Group {
    type Output = Vec<Context<Token>>;

    fn compile(&self, var: &Ident, once: bool) -> Self::Output {
        self.parts.compile(var, once)
    }
}

impl Parse for Group {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut parts = vec![];
        while !input.is_empty() {
            parts.push(input.parse()?);
        }
        Ok(Group { parts })
    }
}

#[cfg(test)]
const _: () = {
    use proc_macro2::TokenStream;
    use quote::{ToTokens, TokenStreamExt};

    use crate::stream::Stream;

    impl ToTokens for BodyPart {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                BodyPart::Normal(part) => tokens.extend(part.stream()),
                BodyPart::Repeat(part) => tokens.extend(part.stream()),
                BodyPart::Group(delim, span, group) => {
                    let mut group = SynGroup::new(*delim, group.stream());
                    group.set_span(*span);
                    tokens.append(group);
                }
            }
        }
    }

    impl ToTokens for Group {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.extend(self.parts.stream());
        }
    }

    impl PartialEq for BodyPart {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (Self::Normal(l0), Self::Normal(r0)) => l0 == r0,
                (Self::Repeat(l0), Self::Repeat(r0)) => l0 == r0,
                (Self::Group(l0, _, l2), Self::Group(r0, _, r2)) => l0 == r0 && l2 == r2,
                _ => false,
            }
        }
    }
};

#[cfg(test)]
mod tests {

    use proc_macro2::{Delimiter, Span};

    use crate::body::BodyPart;
    use crate::compiler::{group as grouped, once, repeat as repeated, Compiler};
    use crate::token::Token::*;
    use crate::{assert_token_stream_eq, generate_parse_quote, generate_parse_str};

    generate_parse_str!(body_part, super::BodyPart);
    generate_parse_str!(part, super::Part);
    generate_parse_str!(group, super::Group);
    generate_parse_str!(repeat, super::Repeat);
    generate_parse_str!(body, super::Body);
    generate_parse_quote!(ident, proc_macro2::Ident);
    generate_parse_quote!(tree, proc_macro2::TokenTree);

    #[test]
    fn compile_empty_body() {
        let body = body!({}).unwrap();
        assert_eq!(body.compile(&ident!(N)), []);
    }

    #[test]
    fn compile_body_with_single_part() {
        let body = body!({ a }).unwrap();
        assert_eq!(body.parts, [BodyPart::Normal(part!(a).unwrap())]);
    }

    #[test]
    fn compile_body_contains_many_parts() {
        let body = body!({ a #(b)* c}).unwrap();

        assert_eq!(
            body.parts,
            [
                BodyPart::Normal(part!(a).unwrap()),
                BodyPart::Repeat(repeat!(#(b)*).unwrap()),
                BodyPart::Normal(part!(c).unwrap())
            ]
        );
    }

    #[test]
    fn parse_body_contains_nested_parts() {
        let body = body!({ nested( a #(b)* c ) }).unwrap();

        assert_eq!(
            body.parts,
            [
                BodyPart::Normal(part!(nested).unwrap()),
                BodyPart::Group(
                    Delimiter::Parenthesis,
                    Span::call_site(),
                    group!(a #(b)* c).unwrap()
                ),
            ]
        );
    }

    #[test]
    fn parse_simple_part() {
        let part = body_part!(a b c).unwrap();
        assert_token_stream_eq!(part, {a b c});
    }

    #[test]
    fn parse_repeat_part() {
        let part = body_part!(#(a b c)*).unwrap();
        assert_token_stream_eq!(part, {a b c});
    }

    #[test]
    fn parse_macros() {
        let pound = tree!(#);
        let part = body_part!(#[deirve(Debug, Clone)]).unwrap();

        assert_token_stream_eq!(part, {#pound [deirve(Debug, Clone)]});
    }

    #[test]
    fn parse_nested_repeat_part() {
        let part = body_part!(( #(a b c)* )).unwrap();
        assert_token_stream_eq!(part, { (a b c) });

        let part = body_part!(( B #(a b c)* )).unwrap();
        assert_token_stream_eq!(part, { (B a b c) });
    }

    #[test]
    fn compile_simple_part() {
        let part = body_part!(N).unwrap();
        assert_eq!(
            part.compile(&ident!(N), false),
            grouped([repeated(Var(None, ident!(N)))])
        );
    }

    #[test]
    fn compile_repeat_part() {
        let part = body_part!(#(N)*).unwrap();
        assert_eq!(
            part.compile(&ident!(N), false),
            grouped([repeated(Var(None, ident!(N)))])
        );
    }

    #[test]
    fn compile_nested_repeat_part() {
        let part = body_part!((B #(N)*)).unwrap();
        assert_eq!(
            part.compile(&ident!(N), false),
            repeated(Group(
                Delimiter::Parenthesis,
                Span::call_site(),
                false,
                vec![
                    grouped([repeated(Tree(tree!(B)))]),
                    grouped([repeated(Var(None, ident!(N)))])
                ]
            ))
        );
        assert_eq!(
            part.compile(&ident!(N), true),
            repeated(Group(
                Delimiter::Parenthesis,
                Span::call_site(),
                true,
                vec![
                    grouped([once(Tree(tree!(B)))]),
                    grouped([repeated(Var(None, ident!(N)))])
                ]
            ))
        );
    }

    #[test]
    fn part_contains_repeat() {
        assert!(!body_part!(a).unwrap().has_repeat());
        assert!(body_part!(#(a)*).unwrap().has_repeat());
        assert!(body_part!((#(a)*)).unwrap().has_repeat());
    }
}
