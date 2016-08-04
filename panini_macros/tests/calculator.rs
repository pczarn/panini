#![feature(plugin)]
#![plugin(panini_macros)]

extern crate panini;

use self::Token::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Token {
    Plus,
    Minus,
    Multiply,
    Divide,
    Zero,
    One,
}

#[test]
fn test_calculator_with_precedenced_rules() {
    let cases = &[
        (vec![One, Plus, One, Multiply, One], 2),
        (vec![One, Plus, One, Multiply, One, Plus, One], 3),
        (vec![One, Divide, One, Plus, One, Multiply, One, Plus, One], 3),
        (vec![Zero, Divide, One], 0),
        (vec![One, Plus, One, Minus, One], 1),
    ];

    for (input, expected) in cases.iter().cloned() {
        let mut parser = grammar! {
            // Doesn't work with 'expression' as the first (start) rule.
            // That gives a runtime error 'Null eval for action 13'. Why?
            start ::= expression;
            expression ::=  Zero => { 0 }
                         |  One => { 1 }
                         |> a:expression Mul   b:expression => { a * b }
                         |  a:expression Div   b:expression => { a / b }
                         |> a:expression Plus  b:expression => { a + b }
                         |  a:expression Minus b:expression => { a - b };

            sub enum_stream {
                Plus = &Plus;
                Minus = &Minus;
                Mul = &Multiply;
                Div = &Divide;
                Zero = &Zero;
                One = &One;
            }
        };
        let mut iter = parser.traced_parse(&input).unwrap();
        assert_eq!(iter.next(), Some(&expected));
        assert_eq!(iter.next(), None);
    }
}
