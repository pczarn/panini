extern crate panini_logic_salsa;
extern crate quote;

use panini_logic_salsa::{input, verify};

fn main() {
    input!(
        start ::= (s:((a:a) (b:b)));
    );
    verify! {
        input == {
            start ::= s:(a:a b:b);
        }
        flattened == {
            start Stmt.0 Bind(s) Product.0 Bind(a) a;
            start Stmt.0 Bind(s) Product.1 Bind(b) b;
        }
        rules == {
            start Stmt.0 => Rule { start ::= 0 a 1 b 2; };
        }
        trace == {
            // 0:
            start Stmt.0 Bind(s) => s:;
            start Stmt.0 Bind(s) Product => LParen;
            start Stmt.0 Bind(s) Product.0 Bind(a) => a:;
            start Stmt.0 Bind(s) Product.0 Bind(a) a => a;
            start Stmt.0 Bind(s) Product.1 Bind(b) => b:;
            start Stmt.0 Bind(s) Product.1 Bind(b) b => b;
            start Stmt.0 Bind(s) Product Max => RParen;
        }
        types == {
            start .0: {
                s: {
                    a: typeof a,
                    b: typeof b,
                }
            }
        }
        typedefs == {
            struct Gen_start_0 {
                a: Terminal,
                b: Terminal,
            }

            struct Gen_start_0_s {
                s: Gen0,
            }
        }
        typedefs_and_rules == {
            start -> Type1 ::= s:[sym0]* => { Type1 { s } };
            sym0 -> Type0 ::= a:a b:b => { Type0 { a, b } };
            struct Type0 {
                a: Terminal,
                b: Terminal,
            }
            struct Type1 {
                s: Vec<Type0>,
            }
        }
        stage3 == {
            #![terminals(a, b)]
            start -> Type1 ::= bind0:sym1 => { bind0 };
            sym1 -> Type1 ::= s:sym2 => { Type1 { s } };
            sym2 -> Vec<Type0> ::= bind0:sym2 bind1:sym0 => { bind0.push(bind1); bind0 }
                | [] => { Vec::new() };
            sym0 -> Type0 ::= a:a b:b => { Type0 { a, b } };
            struct Type0 {
                a: Terminal,
                b: Terminal,
            }
            struct Type1 {
                s: Vec<Type0>,
            }
        }
    }
}
