// run macro at runtime
#![allow(dead_code, unused_macros)]
#![feature(plugin, plugin_registrar, rustc_private)]

extern crate proc_macro;
extern crate rustc_plugin;

extern crate panini_logic;

#[macro_use]
pub mod runtime_enum_stream;
#[macro_use]
pub mod runtime_grammar;

pub use enum_stream::*;
pub use grammar::*;

// grammar & enum_stream

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut ::rustc_plugin::Registry) {
    reg.register_macro("grammar", expand_grammar);
    reg.register_macro("enum_stream", expand_enum_stream);
}

fn expand_grammar<'cx>(ecx: &'cx mut rs::ExtCtxt,
                       sp: rs::Span,
                       tts: &[rs::TokenTree]) -> Box<rs::MacResult + 'cx> {
    let stmts = GrammarParser::new().parse_grammar_from_tts(ecx, tts);
    panini_codegen::codegen(ecx, sp, stmts)
}

fn expand_enum_stream<'cx>(ecx: &'cx mut rs::ExtCtxt,
                           sp: rs::Span,
                           tts: &[rs::TokenTree]) -> Box<rs::MacResult + 'cx> {
    let stmts = EnumStreamParser::new().parse_from_tts(ecx, tts);
    enum_stream_codegen::codegen(ecx, sp, stmts)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let parser = runtime_grammar! {
            sub runtime_enum_stream -> (()) {
                b = ();
            }

            a -> (()) ::= ((x:b) b b) => { x };
        };
        let tokens = vec![(), (), ()];
        parser.parse(&tokens[..]);

        // let mut expected_grammar = Grammar::new(EnumStreamGrammar::new());
        // assert_eq!(grammar, expected_grammar);
    }
}
