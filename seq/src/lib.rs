use proc_macro::TokenStream;
mod body;
mod compiler;
mod part;
mod repeat;
mod seq;
#[cfg(test)]
mod stream;
mod then;
mod token;
use quote::ToTokens;
use seq::Seq;
#[cfg(test)]
use syn_test::*;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    syn::parse::<Seq>(input).unwrap().into_token_stream().into()
}
