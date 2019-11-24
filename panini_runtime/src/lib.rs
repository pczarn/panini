// run macro at runtime
#![allow(dead_code, unused_macros)]

extern crate panini_logic;

#[macro_use]
pub mod enum_stream;
#[macro_use]
pub mod grammar;

pub use enum_stream::*;
pub use grammar::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let parser = grammar! {
            sub enum_stream -> (()) {
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
