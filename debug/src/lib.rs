#![feature(type_name_of_val)]
use proc_macro::TokenStream;
mod debug;
mod fields;
mod generics;
use debug::Debug;
use quote::ToTokens;
#[cfg(test)]
use syn_test::*;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    syn::parse::<Debug>(input)
        .unwrap()
        .into_token_stream()
        .into()
}
