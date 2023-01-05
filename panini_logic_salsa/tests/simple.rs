extern crate panini_logic_salsa;
extern crate quote;

use panini_logic_salsa::{input, verify};

fn main() {
    verify(
        input!(
            start ::= (a b);
        ),
        quote! {
            input == {
                start ::= a b;
            }
            flattened == {
                start Stmt.0 Product.0 a;
                start Stmt.0 Product.1 b;
            }
            rules == {
                start Stmt.0 => Rule { start ::= a b; };
            }
            trace == {
                start Stmt.0 Product.0 a => a;
                start Stmt.0 Product.1 b => b;
            }
            rewritten == {
                start ::= 0 a 1 b 2;
            }
            types == {
                start: (typeof a, typeof b);
            }
            program == {

            }
        },
    );
}
