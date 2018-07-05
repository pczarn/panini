use std::iter::FromIterator;
use rs;

#[derive(Clone, Debug)]
pub struct Lexer {
    pub name: rs::Term,
    pub tts: rs::TokenStream,
}

impl Lexer {
    pub fn new(name: rs::Term, tts: Vec<rs::TokenTree>) -> Self {
        Lexer {
            name: name,
            tts: rs::TokenStream::from_iter(tts.into_iter())
        }
    }

    pub fn name(&self) -> rs::Term {
        self.name
    }

    pub fn tts(&self) -> rs::TokenStream {
        self.tts.clone()
    }
}
