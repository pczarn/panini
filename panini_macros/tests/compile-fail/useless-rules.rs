#![feature(plugin)]
#![plugin(panini_macros)]

fn main() {
    grammar! {
        //~^ grammar is empty

        #![deny(unproductive_rules)]

        a ::= b b => {};
        //~^ error: unproductive rule
        //~| note: these symbols are unproductive

        b ::= a good => {};
        //~^ error: unproductive rule
        //~| note: this symbol is unproductive

        good ::= foo => {};
    };
}
