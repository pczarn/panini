use std::rc::Rc;

use panini_codegen::rs;

pub fn delimit(tts: Vec<rs::TokenTree>, delimiter: rs::DelimToken) -> rs::TokenTree {
    rs::TokenTree::Delimited(rs::DUMMY_SP, Rc::new(rs::Delimited {
        delim: delimiter,
        open_span: rs::DUMMY_SP,
        tts: tts,
        close_span: rs::DUMMY_SP,
    }))
}
