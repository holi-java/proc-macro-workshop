use proc_macro2::TokenStream;
use quote::ToTokens;

pub(crate) trait Stream {
    fn stream(&self) -> TokenStream;
}

impl<T: ToTokens> Stream for T {
    fn stream(&self) -> TokenStream {
        self.to_token_stream()
    }
}

impl<T: Stream> Stream for [T] {
    fn stream(&self) -> TokenStream {
        self.iter().map(Stream::stream).collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::stream::Stream;
    use syn_test::{assert_token_stream_eq, generate_parse_quote};
    generate_parse_quote!(ident, syn::Ident);

    #[test]
    fn stream() {
        assert_token_stream_eq!(ident!(N).stream(), { N });
    }

    #[test]
    fn slice_to_stream() {
        assert_token_stream_eq!([ident!(N), ident!(B)].stream(), { N B });
    }
}
