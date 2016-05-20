#![feature(plugin)]
#![plugin(panini_macros)]
//~^^ error: mismatched types

extern crate panini;

fn main() {
    grammar! {
        a ::= b | c;
        b -> u32 ::= foo => { 0u32 };
        c -> u64 ::= bar => { 0u64 };
        sub enum_stream {
            foo = 0u32;
            bar = 1u32;
        }
    };
}
