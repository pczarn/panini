#![feature(plugin)]
#![plugin(panini_macros)]

extern crate panini;

use self::Word::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Word {
    A,
    The,
    Man,
    Dog,
    Bites,
    Pets,
}

#[test]
fn test_simple_english() {
    for &verb in &[Bites, Pets] {
        let input = [The, Dog, verb, A, Man];
        let mut parser = grammar! {
            sentence ::= subject predicate => {};
            subject ::= article noun;
            predicate ::= verb direct_object;
            direct_object ::= article noun;
            article ::= The | A;
            noun ::= Man | Dog;
            verb ::= Bites | Pets;

            sub enum_stream {
                A = &A;
                The = &The;
                Man = &Man;
                Dog = &Dog;
                Bites = &Bites;
                Pets = &Pets;
            }
        };
        let mut iter = parser.traced_parse(&input).unwrap();
        assert_eq!(iter.next(), Some(&()));
        assert_eq!(iter.next(), None);
    }
}
