use proc_macro::TokenStream;
mod seq;
mod body;
mod section;
use quote::ToTokens;
use seq::Seq;
#[cfg(test)]
use syn_test::*;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    syn::parse::<Seq>(input).unwrap().into_token_stream().into()
}
