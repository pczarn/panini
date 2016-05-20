use rs;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Lexer {
    pub name: rs::Name,
    pub tts: Vec<rs::TokenTree>,
}

impl Lexer {
    pub fn new(name: rs::Name, tts: Vec<rs::TokenTree>) -> Self {
        Lexer {
            name: name,
            tts: tts
        }
    }

    pub fn name(&self) -> rs::Name {
        self.name
    }

    pub fn tts(&self) -> Vec<rs::TokenTree> {
        self.tts.clone()
    }
}
