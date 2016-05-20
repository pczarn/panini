#![allow(non_snake_case)]

use aster::ident::ToIdent;
use aster::AstBuilder;

use front;
use middle::Ir;
use rs;

pub struct IrTranslator {
    ir: Ir,
}

impl IrTranslator {
    pub fn new(ir: Ir) -> Self {
        IrTranslator { ir: ir }
    }

    pub fn translate(&self, cx: &mut rs::ExtCtxt) -> Vec<rs::Stmt> {
        let uniq_id = self.ir.level;
        let upper_id = self.ir.level - 1;
        let UpperParse = rs::str_to_ident(&*format!("Parse{}", upper_id));
        let UpperValue = rs::str_to_ident(&*format!("Value{}", upper_id));
        let UpperLayerParam = rs::str_to_ident(&*format!("LayerParam{}", upper_id));
        let UpperParams = rs::str_to_ident(&*format!("Params{}", upper_id));
        let UpperInfer = rs::str_to_ident(&*format!("Infer{}", upper_id));
        let UpperParseFactory = rs::str_to_ident(&*format!("ParseFactory{}", upper_id));
        let UpperEvalArg = rs::str_to_ident(&*format!("EvalArg{}", upper_id));
        let UpperTerminalAccessor = rs::str_to_ident(&*format!("TerminalAccessor{}", upper_id));
        let Infer = rs::str_to_ident(&*format!("Infer{}", uniq_id));
        let layer_macro = rs::str_to_ident(&*format!("layer_macro{}", uniq_id));
        let EvalArg = rs::str_to_ident(&*format!("EvalArg{}", uniq_id));
        let UpperInferTree = rs::str_to_ident(&*format!("InferTree{}", upper_id));
        let UpperInferTreeVal = rs::str_to_ident(&*format!("InferTreeVal{}", upper_id));
        let InferConstraint = rs::str_to_ident(&*format!("InferConstraint{}", uniq_id));

        let terminal = self.ir.rules.iter().map(|rule| rule.lhs.to_ident());
        let token_id = 0 .. self.ir.rules.len();

        let start = self.ir.rules.iter().map(|rule| rule.start);
        let end = self.ir.rules.iter().map(|rule| rule.end);

        let dol = rs::ast::TokenTree::Token(rs::DUMMY_SP, rs::Token::Dollar);

        let definition = quote_tokens! {cx,
            struct CharClassifierBuilder;

            struct CharClassifier<D> {
                eval_closure: D,
                builder: $UpperParseFactory,
            }

            struct Parse<'g, I> where I: $UpperInferTree<'g> + 'g {
                parse: Box<$UpperParse<'g, I>>,
            }

            impl CharClassifierBuilder {
                fn new() -> Self {
                    CharClassifierBuilder
                }

                fn with_parse_builder_and_eval_closure<'g, D, I>(
                    self,
                    builder: $UpperParseFactory,
                    eval_closure: D)
                    -> CharClassifier<D>
                    where D: FnMut(&'g mut $UpperParse<'g, I>),
                          I: $UpperInferTree<'g> + 'g
                {
                    CharClassifier {
                        eval_closure: eval_closure,
                        builder: builder,
                    }
                }
            }

            impl<D> CharClassifier<D> {
                fn parse<'g, I>(&'g mut self, input: &str) -> Parse<'g, I>
                    where D: FnMut(&'g mut $UpperParse<'g, I>),
                          I: $UpperInferTree<'g, Node = ()> + 'g,
                {
                    self.common_parse(input, false)
                }

                fn traced_parse<'g, I>(&'g mut self, input: &str) -> Parse<'g, I>
                    where D: FnMut(&'g mut $UpperParse<'g, I>),
                          I: $UpperInferTree<'g, Node = ()> + 'g,
                {
                    self.common_parse(input, true)
                }

                #[inline]
                fn common_parse<'g, I>(&'g mut self, input: &str, traced: bool) -> Parse<'g, I>
                    where D: FnMut(&'g mut $UpperParse<'g, I>),
                          I: $UpperInferTree<'g, Node = ()> + 'g,
                {
                    let tokens = &[
                        $(self.builder.terminal_accessor().$terminal(),)*
                    ];
                    let mut parse_box = Box::new(self.builder.new_parse());
                    let parse: &'g mut $UpperParse<'g, I>;
                    unsafe {
                        parse = &mut *(&mut *parse_box as *mut _);
                    }
                    let mut char_index = 0;
                    for character in input.chars() {
                        let len = character.len_utf8();
                        parse.begin_earleme();
                        match character {
                            $(
                                $start ... $end => {
                                    parse.scan_tok(tokens[$token_id], (char_index, len));
                                }
                            )*
                        }
                        if traced {
                            assert!(parse.traced_advance(), "classifier::parse: parse error");
                        } else {
                            assert!(parse.advance(), "classifier::parse: parse error");
                        }
                        char_index += len;
                    }
                    parse.end_of_input();
                    (self.eval_closure)(
                        parse
                    );
                    Parse {
                        parse: parse_box,
                    }
                }
            }

            impl<'g, I> Iterator for Parse<'g, I>
                where I: $UpperInferTree<'g> + 'g
            {
                type Item = <$UpperParse<'g, I> as Iterator>::Item;
                fn next(&mut self) -> Option<Self::Item> {
                    self.parse.next()
                }
            }

            trait $InferConstraint<'g, Node>: $UpperInfer<T = Node>
                where Node: Copy
            {}

            impl<'g, Node, T> $InferConstraint<'g, Node> for T
                where Node: Copy, T: $UpperInfer<T = Node>
            {}
        };

        let block = quote_block!{cx, {
            $definition

            macro_rules! $layer_macro {
                (
                    @closure
                    $dol upper_builder:expr,
                    $dol ignored__parse:expr,
                    $dol node:expr;
                ) => ({
                    let upper_builder = &mut $dol upper_builder;
                    upper_builder.reserve(1);
                    upper_builder.push(());
                });
                (@builder @factory [$dol factory:expr] @closure [$dol closure:expr]) => (
                    CharClassifierBuilder::new().with_parse_builder_and_eval_closure(
                        $dol factory,
                        $dol closure
                    )
                );
                (@get $dol parse:expr) => (
                    $dol parse
                )
            }
        }};

        block.unwrap().unwrap().stmts
    }
}
