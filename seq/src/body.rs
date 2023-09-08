use proc_macro2::{Ident, Punct, TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};

use crate::section::{Eval, Section};

pub(crate) struct Body {
    body: TokenStream,
}

impl Body {
    pub fn eval<I>(&self, var: &Ident, range: I) -> TokenStream
    where
        I: IntoIterator<Item = i32>,
    {
        let sections = self.compile(var);
        range.into_iter().map(|n| sections.eval(n)).collect()
    }

    pub fn compile(&self, var: &Ident) -> Vec<Section> {
        let mut sections = vec![];
        collect(self.body.clone(), var, &mut sections).unwrap();
        return sections;

        fn collect(
            tokens: TokenStream,
            var: &Ident,
            sections: &mut Vec<Section>,
        ) -> syn::Result<bool> {
            const CAT: char = '~';
            let mut downgrade = false;
            let (mut cat, mut prev) = (None::<Punct>, None::<Ident>);
            for tt in tokens {
                let mut current = None::<Ident>;

                if let TokenTree::Punct(punct) = &tt {
                    if punct.as_char() == CAT && cat.is_none() {
                        cat.replace(punct.clone());
                        continue;
                    }
                }

                if let TokenTree::Ident(ident) = &tt {
                    if ident == var {
                        if let Some(tt) = prev.take() {
                            if cat.take().is_some() {
                                sections.push(Section::concat(tt));
                                continue;
                            }
                            sections.push(Section::repeat(tt.into_token_stream()));
                        }

                        sections.push(Section::var(ident.span()));
                        continue;
                    }
                    current = Some(ident.clone());
                }

                if let Some(tt) = prev.take() {
                    sections.push(Section::repeat(tt.into_token_stream()));
                }
                if let Some(tt) = cat.take() {
                    sections.push(Section::repeat(tt.into_token_stream()));
                }
                if let Some(tt) = current.take() {
                    prev = Some(tt);
                    continue;
                }

                if let TokenTree::Group(group) = &tt {
                    let mut sub = vec![];
                    downgrade |= collect(group.stream(), var, &mut sub)?;
                    sections.push(Section::group(group.delimiter(), group.span(), sub));
                    continue;
                }

                sections.push(Section::repeat(tt.into_token_stream()));
            }

            if let Some(tt) = prev.take() {
                sections.push(Section::repeat(tt.into_token_stream()));
            }
            if let Some(tt) = cat.take() {
                sections.push(Section::repeat(tt.into_token_stream()));
            }
            Ok(downgrade)
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
    use crate::{assert_token_stream_eq, generate_macro, section::Section};
    use quote::quote;
    generate_macro!(ident, syn::Ident);
    generate_macro!(body, super::Body);

    #[test]
    fn parse_body() {
        assert_token_stream_eq!(body!({}).body, {});
        assert_token_stream_eq!(body!({ N }).body, { N });
    }

    #[test]
    fn compile_static_only() {
        assert_eq!(
            body!({ f }).compile(&ident!(N)),
            [Section::repeat(quote!(f))]
        );
        assert_eq!(
            body!({ ~ }).compile(&ident!(N)),
            [Section::repeat(quote!(~))]
        );
        assert_eq!(
            body!({ ~f }).compile(&ident!(N)),
            [Section::repeat(quote!(~)), Section::repeat(quote!(f))]
        );
        assert_eq!(
            body!({ f b }).compile(&ident!(N)),
            [Section::repeat(quote!(f)), Section::repeat(quote!(b))]
        );
    }

    #[test]
    fn compile_vars_only() {
        let n = ident!(N);
        let body = body!({ #n #n });

        let sections = body.compile(&ident!(N));
        assert_eq!(sections, [Section::var(n.span()), Section::var(n.span())]);
    }

    #[test]
    fn eval_body() {
        assert_token_stream_eq!(body!({}).eval(&ident!(N), 0..2), {});
        assert_token_stream_eq!(body!({N}).eval(&ident!(N), 0..2), {0 1});
    }

    #[test]
    fn eval_body_concat_variable() {
        assert_token_stream_eq!(body!({f~N}).eval(&ident!(N), 0..2), { f0 f1 });
    }
}
