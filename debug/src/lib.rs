use proc_macro::TokenStream;
mod debug;
use debug::Debug;
use quote::ToTokens;
#[cfg(test)]
use syn_test::assert_token_stream_eq;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    syn::parse::<Debug>(input)
        .unwrap()
        .into_token_stream()
        .into()
}
