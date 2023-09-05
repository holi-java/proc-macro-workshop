use proc_macro::TokenStream;
mod field;
mod generator;
mod setter;
#[cfg(test)]
use syn_test::assert_token_stream_eq;
use generator::Generator;
use quote::ToTokens;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn builder(input: TokenStream) -> TokenStream {
    syn::parse::<Generator>(input)
        .unwrap()
        .into_token_stream()
        .into()
}
