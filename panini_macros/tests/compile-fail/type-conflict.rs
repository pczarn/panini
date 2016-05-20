#![feature(plugin)]
#![plugin(panini_macros)]

fn main() {
    grammar! {
        //~^ error: type mismatch
        top ::= bad bad | bad;
        bad ::= foo;
    };
}
