use proc_macro::TokenStream;
use proc_macro2::Span;
use syn::{Error, ItemEnum};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    if let Err(_) = syn::parse::<ItemEnum>(input) {
        return Error::new(Span::call_site(), "expected enum or match expression")
            .into_compile_error()
            .into();
    }
    TokenStream::new()
}
