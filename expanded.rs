#![feature(no_std, prelude_import, main)]
#![no_std]
#![feature(plugin)]
#![plugin(panini)]
#[prelude_import]
use std::prelude::v1::*;
#[macro_use]
extern crate std as std;

extern crate gearley;
extern crate cfg;

#[test]
pub fn test_simple() {
    let parser =
        {
            use gearley::grammar;
            use gearley::recognizer::Recognizer;
            use gearley::forest::{Forest, NullForest};
            use gearley::bocage::Bocage;
            use gearley::bocage::Link;
            use gearley::traversal::Traversal;
            use gearley::evaluator::StackEvaluator;
            use cfg::symbol::NumericSymbol as Symbol;
            static STORAGE: &'static [u8] =
                &[3u8, 0u8, 0u8, 0u8, 11u8, 0u8, 0u8, 0u8, 12u8, 0u8, 0u8,
                  0u8, 7u8, 0u8, 0u8, 0u8, 9u8, 0u8, 0u8, 0u8, 10u8, 0u8, 0u8,
                  0u8, 10u8, 0u8, 0u8, 0u8, 7u8, 0u8, 0u8, 0u8, 6u8, 0u8, 0u8,
                  0u8, 11u8, 0u8, 0u8, 0u8, 7u8, 0u8, 0u8, 0u8, 12u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 5u8, 0u8, 0u8, 0u8, 8u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 4u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 6u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 7u8, 0u8,
                  0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 3u8, 0u8, 0u8, 0u8, 4u8, 0u8,
                  0u8, 0u8, 4u8, 0u8, 0u8, 0u8, 6u8, 0u8, 0u8, 0u8, 9u8, 0u8,
                  0u8, 0u8, 9u8, 0u8, 0u8, 0u8, 10u8, 0u8, 0u8, 0u8, 11u8,
                  0u8, 0u8, 0u8, 11u8, 0u8, 0u8, 0u8, 12u8, 0u8, 0u8, 0u8,
                  12u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8,
                  1u8, 0u8, 0u8, 0u8, 3u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8,
                  3u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8,
                  1u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 202u8, 14u8, 0u8,
                  0u8, 4u8, 0u8, 0u8, 0u8, 200u8, 14u8, 0u8, 0u8, 144u8, 16u8,
                  0u8, 0u8, 32u8, 0u8, 0u8, 0u8, 192u8, 6u8, 0u8, 0u8, 128u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 128u8, 6u8, 0u8, 0u8,
                  128u8, 4u8, 0u8, 0u8, 192u8, 14u8, 0u8, 0u8, 128u8, 16u8,
                  0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 24u8, 0u8, 0u8, 0u8, 11u8,
                  0u8, 0u8, 0u8, 32u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8,
                  15u8, 0u8, 0u8, 0u8, 10u8, 0u8, 0u8, 0u8, 19u8, 0u8, 0u8,
                  0u8, 12u8, 0u8, 0u8, 0u8, 34u8, 0u8, 0u8, 0u8, 6u8, 0u8,
                  0u8, 0u8, 16u8, 0u8, 0u8, 0u8, 9u8, 0u8, 0u8, 0u8, 29u8,
                  0u8, 0u8, 0u8, 9u8, 0u8, 0u8, 0u8, 18u8, 0u8, 0u8, 0u8, 3u8,
                  0u8, 0u8, 0u8, 25u8, 0u8, 0u8, 0u8, 11u8, 0u8, 0u8, 0u8,
                  21u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8, 26u8, 0u8, 0u8,
                  0u8, 12u8, 0u8, 0u8, 0u8, 23u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 1u8, 0u8,
                  0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 1u8, 0u8,
                  0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 5u8, 0u8,
                  0u8, 0u8, 5u8, 0u8, 0u8, 0u8, 5u8, 0u8, 0u8, 0u8, 5u8, 0u8,
                  0u8, 0u8, 6u8, 0u8, 0u8, 0u8, 6u8, 0u8, 0u8, 0u8, 7u8, 0u8,
                  0u8, 0u8, 8u8, 0u8, 0u8, 0u8, 9u8, 0u8, 0u8, 0u8, 10u8, 0u8,
                  0u8, 0u8, 12u8, 0u8, 0u8, 0u8, 12u8, 0u8, 0u8, 0u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8,
                  255u8, 255u8, 255u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8,
                  1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8,
                  3u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8,
                  2u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8,
                  2u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8,
                  1u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 3u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8,
                  1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 3u8, 0u8, 0u8, 0u8,
                  1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8,
                  2u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8,
                  4u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8,
                  1u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 3u8, 0u8, 0u8, 0u8,
                  1u8, 0u8, 0u8, 0u8, 3u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 3u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 4u8, 0u8, 0u8, 0u8,
                  1u8, 127u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
                  0u8, 0u8, 0u8, 0u8, 0u8];
            struct Parser {
                grammar: grammar::Grammar,
            }
            struct Parses<'g> {
                recognizer: Recognizer<'g, 'static, Bocage<'static, ()>>,
                bocage: Box<Bocage<'static, ()>>,
                traversal: Traversal<'static, 'g,
                           StackEvaluator<fn(Symbol, Option<&()>) -> (),
                                          fn(u32, &mut [()]) -> (), (), ()>,
                           Link<'static, ()>>,
            }
            fn sym(_: Symbol, _: Option<&()>) -> () { }
            fn rule(_: u32, _: &mut [()]) -> () { }
            impl Parser {
                fn new() -> Self {
                    let grammar =
                        grammar::Grammar::from_parts(grammar::GrammarParts{storage:
                                                                               ::std::borrow::Cow::Borrowed(STORAGE),
                                                                           num_syms:
                                                                               13usize,
                                                                           num_rules:
                                                                               12usize,
                                                                           num_nulling_rules:
                                                                               3usize,
                                                                           start_sym:
                                                                               1u32,
                                                                           trivial_derivation:
                                                                               true,});
                    Parser{grammar: grammar,}
                }
                fn parses<'g>(&'g self, input: &[u32]) -> Parses<'g> {
                    let mut bocage = Box::new(Bocage::new());
                    let bocage_ref: &'static Bocage<()> =
                        unsafe { &*(&*bocage as *const _) };
                    let bocage_mut: &'static mut Bocage<()> =
                        unsafe { &mut *(&mut *bocage as *mut _) };
                    let mut rec:
                            Recognizer<'g, 'static, Bocage<'static, ()>> =
                        unsafe {
                            let rec: Recognizer<Bocage> =
                                Recognizer::new(&self.grammar, bocage_mut);
                            ::std::mem::transmute(rec)
                        };
                    let rec_ref:
                            &mut Recognizer<'g, 'static, Bocage<'static>> =
                        unsafe { &mut *(&mut rec as *mut _) };
                    rec_ref.parse(input);
                    let rec_ref: &Recognizer<'g, 'static, Bocage<'static>> =
                        unsafe { &*(&rec as *const _) };
                    let traversal =
                        Traversal::new(&self.grammar,
                                       bocage_ref.get(&rec.finished_label()),
                                       StackEvaluator::new(sym as
                                                               fn(Symbol,
                                                                  Option<&()>)
                                                                   -> (),
                                                           rule as
                                                               fn(u32,
                                                                  &mut [()])
                                                                   -> ()));
                    Parses{recognizer: rec,
                           bocage: unsafe { ::std::mem::transmute(bocage) },
                           traversal: traversal,}
                }
            }
            impl <'g> Iterator for Parses<'g> {
                type
                Item
                =
                ();
                fn next(&mut self) -> Option<()> { self.traversal.next() }
            }
            Parser::new()
        };
    let mut iter = parser.parses(&[3, 3, 3, 3]);
    {
        match (&(iter.next()), &(Some(()))) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    {
                        ::std::rt::begin_unwind_fmt(::std::fmt::Arguments::new_v1({
                                                                                      static __STATIC_FMTSTR:
                                                                                             &'static [&'static str]
                                                                                             =
                                                                                          &["assertion failed: `(left == right)` (left: `",
                                                                                            "`, right: `",
                                                                                            "`)"];
                                                                                      __STATIC_FMTSTR
                                                                                  },
                                                                                  &match (&*left_val,
                                                                                          &*right_val)
                                                                                       {
                                                                                       (__arg0,
                                                                                        __arg1)
                                                                                       =>
                                                                                       [::std::fmt::ArgumentV1::new(__arg0,
                                                                                                                    ::std::fmt::Debug::fmt),
                                                                                        ::std::fmt::ArgumentV1::new(__arg1,
                                                                                                                    ::std::fmt::Debug::fmt)],
                                                                                   }),
                                                    {
                                                        static _FILE_LINE:
                                                               (&'static str,
                                                                u32) =
                                                            ("tests/simple.rs",
                                                             17u32);
                                                        &_FILE_LINE
                                                    })
                    }
                }
            }
        }
    };
    {
        match (&(iter.next()), &(None)) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    {
                        ::std::rt::begin_unwind_fmt(::std::fmt::Arguments::new_v1({
                                                                                      static __STATIC_FMTSTR:
                                                                                             &'static [&'static str]
                                                                                             =
                                                                                          &["assertion failed: `(left == right)` (left: `",
                                                                                            "`, right: `",
                                                                                            "`)"];
                                                                                      __STATIC_FMTSTR
                                                                                  },
                                                                                  &match (&*left_val,
                                                                                          &*right_val)
                                                                                       {
                                                                                       (__arg0,
                                                                                        __arg1)
                                                                                       =>
                                                                                       [::std::fmt::ArgumentV1::new(__arg0,
                                                                                                                    ::std::fmt::Debug::fmt),
                                                                                        ::std::fmt::ArgumentV1::new(__arg1,
                                                                                                                    ::std::fmt::Debug::fmt)],
                                                                                   }),
                                                    {
                                                        static _FILE_LINE:
                                                               (&'static str,
                                                                u32) =
                                                            ("tests/simple.rs",
                                                             18u32);
                                                        &_FILE_LINE
                                                    })
                    }
                }
            }
        }
    };
}
pub mod __test_reexports {
    #[prelude_import]
    use std::prelude::v1::*;
    pub use super::test_simple;
}
pub mod __test {
    #[prelude_import]
    use std::prelude::v1::*;
    extern crate test;
    #[main]
    pub fn main() -> () { test::test_main_static(TESTS); }
    const TESTS: &'static [self::test::TestDescAndFn] =
        &[self::test::TestDescAndFn{desc:
                                        self::test::TestDesc{name:
                                                                 self::test::StaticTestName("test_simple"),
                                                             ignore: false,
                                                             should_panic:
                                                                 self::test::ShouldPanic::No,},
                                    testfn:
                                        self::test::StaticTestFn(::__test_reexports::test_simple),}];
}
