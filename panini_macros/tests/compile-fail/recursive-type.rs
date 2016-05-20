#![feature(plugin)]
#![plugin(panini_macros)]

fn main() {
    grammar! {
        //~^ grammar is empty

        top ::= bad bad;
        //~^ error: recursive type
        //~| note: these symbols have recursive types
        //~| warning: unproductive rule
        //~| note: these symbols are unproductive

        bad ::= top good;
        //~^ error: recursive type
        //~| note: this symbol has a recursive type
        //~| warning: unproductive rule
        //~| note: this symbol is unproductive

        good ::= foo;
    };
}
