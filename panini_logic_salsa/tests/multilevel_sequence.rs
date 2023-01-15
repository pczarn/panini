extern crate panini_logic_salsa;
extern crate quote;

use panini_logic_salsa::{input, verify};

fn main() {
    verify(
        input!(
            start ::= ((((a:a) (b:b))*)*);
        ),
        quote! {
            input == {
                start ::= (a:a b:b)**;
            }
            flattened == {
                start Stmt.0 * * Product.0 a;
                start Stmt.0 * * Product.1 b;
            }
            rules == {
                generated[0] => start Stmt.0 *;
                start Stmt.0 => Rule { start ::= 0 generated[start Stmt.0 *]* 6; };
                start Stmt.0 * => Rule { generated[start Stmt.0 *] ::= generated[start Stmt]* 5; }
                start Stmt.0 * * => Rule { generated[1] ::= 1 a 2 b 3; };
            }
            trace == {
                // 0
                start Sum.0 * * => LParen;
                // 1
                start Sum.0 * * Product.0 => a;
                // 2
                start Sum.0 * * Product.1 => b;
                // 3
                start Sum.0 * * SequenceEnd => RParen;
                // 4
                start Sum.0 * * SequenceToken => *;
                // 5
                start Sum.0 * SequenceToken => *;
                // 6
            }
            rewritten == {
                start .0 * => {
                    start ::= 0 generated[3]* 6;
                    * => {
                        generated[3] ::= (1 a 2 b 3)* 5;
                    }
                }
            }
            types == {
                start .0: {
                    * * a: [[typeof a]],
                    * * b: [[typeof b]],
                };
                start .0 *: {
                    * a: [typeof a],
                    * b: [typeof b],
                };
                start .0 * *: {
                    a: typeof a,
                    b: typeof b,
                };
                start .0 * * a: typeof a;
                start .0 * * b: typeof b;
            }
        },
    );
}
