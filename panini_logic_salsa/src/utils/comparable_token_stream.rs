use proc_macro2::TokenStream;

#[derive(Clone)]
pub struct ComparableTokenStream(TokenStream);

impl ComparableTokenStream {
    pub fn token_stream(self) -> TokenStream {
        self.0
    }
}

impl std::fmt::Debug for ComparableTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::fmt::Display for ComparableTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl PartialEq for ComparableTokenStream {
    fn eq(&self, other: &Self) -> bool {
        self.to_string().eq(&other.to_string())
    }
}

impl Eq for ComparableTokenStream {
    fn assert_receiver_is_total_eq(&self) {}
}

impl std::hash::Hash for ComparableTokenStream {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state)
    }
}

impl From<TokenStream> for ComparableTokenStream {
    fn from(token_stream: TokenStream) -> Self {
        ComparableTokenStream(token_stream)
    }
}

impl From<ComparableTokenStream> for TokenStream {
    fn from(token_stream: ComparableTokenStream) -> Self {
        token_stream.0
    }
}
