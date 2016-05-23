#![feature(plugin, plugin_registrar, rustc_private)]
#![plugin(quasi_macros, panini_macros_snapshot)]

extern crate syntax;
extern crate rustc_plugin;

extern crate aster;
extern crate quasi;

extern crate panini;

extern crate enum_stream_codegen;
extern crate panini_codegen;

mod enum_stream_parser;
mod grammar_parser;
mod util;

pub use self::enum_stream_parser::Parser as EnumStreamParser;
pub use self::grammar_parser::Parser;
pub use panini_codegen::front::ast;
use panini_codegen::rs;

pub type Name = rs::Spanned<rs::Name>;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut ::rustc_plugin::Registry) {
    reg.register_macro("grammar", expand_grammar);
    reg.register_macro("enum_stream", expand_enum_stream);
}

fn expand_grammar<'cx>(ecx: &'cx mut rs::ExtCtxt,
                       sp: rs::Span,
                       tts: &[rs::TokenTree]) -> Box<rs::MacResult + 'cx> {
    let stmts = Parser::new().parse_grammar_from_tts(ecx, tts);
    panini_codegen::codegen(ecx, sp, stmts)
}

fn expand_enum_stream<'cx>(ecx: &'cx mut rs::ExtCtxt,
                           sp: rs::Span,
                           tts: &[rs::TokenTree]) -> Box<rs::MacResult + 'cx> {
    let stmts = EnumStreamParser::new().parse_from_tts(ecx, tts);
    enum_stream_codegen::codegen(ecx, sp, stmts)
}
