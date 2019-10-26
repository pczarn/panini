// run macro at runtime
#![allow(dead_code, unused_macros)]

pub use enum_stream::*;
pub use grammar::*;

#[macro_use]
pub mod enum_stream;
#[macro_use]
pub mod grammar;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        grammar! {
            sub enum_stream -> (()) {
                b = ();
            }

            a -> (()) ::= (x:b) => { x };
        };

        // let mut expected_grammar = Grammar::new(EnumStreamGrammar::new());
        // assert_eq!(grammar, expected_grammar);
    }
}
