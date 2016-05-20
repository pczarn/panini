#![feature(plugin)]
#![plugin(panini_macros)]

fn main() {
    grammar! {
        #![forbid(dead_code)]
        #![allow(dead_code)]
        //~^ lint levels

        start ::= foo;
    };
}
