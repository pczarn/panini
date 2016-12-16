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

        let id = 0 .. self.ir.rules.len();
        let id2 = id.clone();
        let id3 = id.clone();
        let id4 = id.clone();
        let id5 = id.clone();
        let id6 = id.clone();
        let id7 = id.clone();
        let terminal = self.ir.rules.iter().map(|rule| rule.name.to_ident());
        let terminal2 = self.ir.rules.iter().map(|rule| rule.name.to_ident());
        let terminal3 = self.ir.rules.iter().map(|rule| rule.name.to_ident());

        let builder = AstBuilder::new();

        let negative_arm = self.ir.rules.iter().map(|rule| {
            rule.negative.iter().cloned().map(|(pat, guard)| {
                ////////
                // Some(NEGATIVE_PAT) if GUARD => { return false }
                ////////
                builder.arm()
                    .pat().enum_()
                        .id("Some").build()
                        .pat().build(pat).build()
                    .with_guard(guard)
                    .body()
                        .return_expr().false_()
            })
        });

        let positive_match = self.ir.rules.iter().enumerate().map(|(id, rule)| {
            let mut positive = rule.positive.clone();
            if positive.is_empty() {
                positive.push((builder.pat().wild(), None));
            }
            let mut iter = positive.into_iter().rev();
            let (first_pat, first_guard) = iter.next().unwrap();
            //////// Build an expression
            // match item {
            //     Some(FIRST_PAT) if FIRST_GUARD => { return true }
            //     _ => ()
            // }
            ////////
            let mut match_stmt = builder.expr().match_()
                .id("item")
                .arm()
                    .pat().enum_()
                        .id("Some").build()
                        .pat().build(first_pat).build()
                    .with_guard(first_guard)
                    .body()
                        .return_expr().true_()
                .arm()
                    .pat().wild()
                    .body().unit()
                .build();
            ////////
            // match item {
            //     Some(PAT) if GUARD => { MATCH_STMT }
            //     _ => ()
            // }
            ////////
            for (pat, guard) in iter {
                match_stmt = builder.expr().match_()
                    .id("item")
                    .arm()
                        .pat().enum_()
                            .id("Some").build()
                            .pat().build(pat).build()
                        .build_arm_(guard, match_stmt)
                    .arm()
                        .pat().wild()
                        .body().unit()
                    .build();
            }
            match_stmt
        }).collect::<Vec<_>>();
        let positive_match = positive_match.into_iter();

        let variant = self.ir.rules.iter().map(|rule| {
            let capitalized: String = rule.name.as_str().split('_').flat_map(|word| {
                let mut chars = word.chars();
                let letter = chars.next();
                letter.into_iter().flat_map(|ch| ch.to_uppercase()).chain(chars)
            }).collect();
            rs::str_to_ident(&capitalized[..])
        }).collect::<Vec<_>>();
        let variant = variant.into_iter();
        let dol = rs::TokenTree::Token(rs::DUMMY_SP, rs::Token::Dollar);

        let definition = quote_tokens! {cx,
            struct EnumStream<C> {
                closure: C,
            }

            struct EnumStreamParser<C, D> {
                closure: C,
                eval_closure: D,
                builder: $UpperParseFactory,
            }

            struct Parse<'g, I> where I: $UpperInferTree<'g> + 'g {
                parse: Box<$UpperParse<'g, I>>,
            }

            struct ExhaustedParse<'g, I> where I: $UpperInferTree<'g> + 'g {
                parse: Box<$UpperParse<'g, I>>,
                input_pos: usize,
            }

            impl<C> EnumStream<C> {
                fn new(closure: C) -> Self {
                    EnumStream {
                        closure: closure,
                    }
                }

                fn with_parse_builder_and_eval_closure<'g, D, I>(
                    self,
                    builder: $UpperParseFactory,
                    eval_closure: D)
                    -> EnumStreamParser<C, D>
                    where D: FnMut(&'g mut $UpperParse<'g, I>),
                          I: $UpperInferTree<'g> + 'g
                {
                    EnumStreamParser {
                        closure: self.closure,
                        eval_closure: eval_closure,
                        builder: builder,
                    }
                }
            }

            // Either `parse` or `traced_parse` may be dead code.
            #[allow(dead_code)]
            impl<C, D> EnumStreamParser<C, D> {
                fn parse<'g, I, Iter>(&'g mut self, into_iter: Iter)
                    -> Result<Parse<'g, I>, ExhaustedParse<'g, I>>
                    where C: Fn(usize, Iter::Item) -> bool,
                          D: FnMut(&'g mut $UpperParse<'g, I>),
                          Iter: IntoIterator<Item = I::Node>,
                          Iter::Item: Copy,
                          I: $UpperInferTree<'g> + 'g,
                {
                    self.common_parse(into_iter, false)
                }

                fn traced_parse<'g, I, Iter>(&'g mut self, into_iter: Iter)
                    -> Result<Parse<'g, I>, ExhaustedParse<'g, I>>
                    where C: Fn(usize, Iter::Item) -> bool,
                          D: FnMut(&'g mut $UpperParse<'g, I>),
                          Iter: IntoIterator<Item = I::Node>,
                          Iter::Item: Copy,
                          I: $UpperInferTree<'g> + 'g,
                {
                    self.common_parse(into_iter, true)
                }

                #[inline]
                fn common_parse<'g, I, Iter>(&'g mut self, into_iter: Iter, traced: bool)
                    -> Result<Parse<'g, I>, ExhaustedParse<'g, I>>
                    where C: Fn(usize, Iter::Item) -> bool,
                          D: FnMut(&'g mut $UpperParse<'g, I>),
                          Iter: IntoIterator<Item = I::Node>,
                          Iter::Item: Copy,
                          I: $UpperInferTree<'g> + 'g,
                {
                    let tokens = &[
                        $(self.builder.terminal_accessor().$terminal(),)*
                    ];
                    let mut parse_box = Box::new(self.builder.new_parse());
                    let parse: &'g mut $UpperParse<'g, I>;
                    unsafe {
                        parse = &mut *(&mut *parse_box as *mut _);
                    }
                    let iter = into_iter.into_iter();
                    for (i, elem) in iter.enumerate() {
                        if traced {
                            parse.traced_begin_earleme();
                        } else {
                            parse.begin_earleme();
                        }
                        scan_elem(&mut self.closure, tokens, parse, elem);
                        let success = if traced {
                            parse.traced_advance()
                        } else {
                            parse.advance()
                        };
                        if !success {
                            return Err(ExhaustedParse {
                                parse: parse_box,
                                input_pos: i,
                            });
                        }
                    }
                    if traced {
                        parse.traced_end_of_input();
                    } else {
                        parse.end_of_input();
                    }
                    (self.eval_closure)(
                        parse
                    );
                    Ok(Parse {
                        parse: parse_box,
                    })
                }
            }

            fn scan_elem<'g, C, I>(
                closure: &mut C,
                tokens: &[Symbol],
                parse: &mut $UpperParse<'g, I>,
                elem: I::Node)
                where C: Fn(usize, I::Node) -> bool,
                      I: $UpperInferTree<'g> + 'g,
            {
                $(
                    if closure($id, elem) {
                        let token = tokens[$id3];
                        parse.scan_tok(token, elem);
                    }
                )*
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

            impl<'g, I> fmt::Debug for ExhaustedParse<'g, I>
                where I: $UpperInferTree<'g> + 'g
            {
                fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
                    self.parse.fmt_exhaustion(fmt, self.input_pos)
                }
            }
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
                    let terminal_accessor = $UpperTerminalAccessor;
                    let upper_builder = &mut $dol upper_builder;
                    let sym = ($dol node).terminal;
                    let value = ($dol node).value;
                    let tokens = &[
                        $(terminal_accessor.$terminal3(),)*
                    ];
                    let value = $(
                        if sym == tokens[$id6] {
                            $UpperValue::$variant(value)
                        } else
                    )+ {
                        unreachable!()
                    };
                    upper_builder.reserve(1);
                    upper_builder.push(value);
                });
                (@builder @factory [$dol factory:expr] @closure [$dol closure:expr]) => (
                    EnumStream::new(|id: usize, item| {
                        $(
                            if id == $id2 {
                                let item = Some(item);
                                match item {
                                    $($negative_arm)*
                                    _ => {}
                                }
                                $positive_match
                            }
                        )*
                        false
                    }).with_parse_builder_and_eval_closure($dol factory, $dol closure)
                );
                (@get $dol parse:expr) => (
                    $dol parse
                )
            }
        }};

        block.unwrap().unwrap().stmts
    }
}
