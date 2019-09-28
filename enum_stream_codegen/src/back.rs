#![allow(non_snake_case)]

use panini_common::rs;
use enum_coder::enum_coder;

use quote::quote;

use front;
use middle::Ir;

use self::Instruction::*;

pub struct IrTranslator {
    ir: Ir,
}

struct InterpreterState {
  stack: Vec<rs::TokenStream>,
}

pub struct InstructionList {
    list: Vec<Instruction>,
}

enum_coder! {
    enum Instruction {
        Concat(usize),
        ConcatSeparated(usize, rs::TokenStream),
        Dup,

        // defines:
        //
        // - struct EnumStream<C>
        // - struct EnumStreamParser<C, D>
        // - struct Parse<'g, I>
        // - struct ExhaustedParse<'g, I>
        // - trait InferConstraint<'g, Node>
        // - fn scan_elem
        MakeCommonDefinitions {
            UpperParseFactory: rs::Ident,
            UpperParse: rs::Ident,
            UpperInferTree: rs::Ident,
            InferConstraint: rs::Ident,
            UpperInfer: rs::Ident,
        },

        MakeTerminalAccess {
            terminal_name: rs::Ident,
        },

        // defines:
        //
        // impl EnumStreamParser<C, D> {
        //     fn common_parse<Iter>(&'g mut self, into_iter: Iter, traced: bool) 
        // }
        MakeEnumStreamParserCommonParseImpl {
            UpperParse: rs::Ident,
            UpperInferTree: rs::Ident,
        },

        MakeValueConditionally {
            UpperValue: rs::Ident,
            terminal_id: usize,
            variant: rs::Ident,
        },

        MakeValueConditionalsTail,

        MakeNegativePatternMatchArm {
            pattern: rs::Pat,
        },

        MakePositiveMatch {
            patterns: Vec<rs::Pat>,
            guard: Option<rs::Expr>,
        },

        MakeTerminalAction {
            terminal_id: usize,
        },

        MakeLayerMacroDefinition {
            layer_macro: rs::Ident,
            UpperTerminalAccessor: rs::Ident,
        },
    }

    #[missing_field(ir)]
    fn make_ident(ir: &Ir, field_name: &str) -> rs::Ident {
        let upper_id = ir.level - 1;
        rs::str_to_ident(format!("{}{}", field_name, upper_id))
    }

    #[generate_list]
    fn translate_ir(ir: &Ir) -> Vec<Instruction> {
        MakeCommonDefinitions();
        for rule in &ir.rules {
            MakeTerminalAccess {
                terminal_name: rule.name.clone()
            }
        }
        ConcatSeparated(ir.rules.len(), quote! { , });
        Dup();
        MakeEnumStreamParserCommonParseImpl();
        for (n, rule) in ir.rules.iter().enumerate() {
            let capitalized = capitalize(&rule.name.to_string()[..]);
            let variant = rs::str_to_ident(&capitalized[..]);
            MakeValueConditionally {
                terminal_id: n,
                variant,
            };
        }
        MakeValueConditionalsTail();
        Concat(ir.rules.len() + 1);
        for (n, rule) in ir.rules.iter().enumerate() {
            for negative_pattern in &rule.negative {
                MakeNegativePatternMatchArm {
                    pattern: negative_pattern.clone()
                }
            }
            Concat(rule.negative.len());
            MakePositiveMatch {
                patterns: rule.positive.clone(),
                guard: rule.guard.clone(),
            };
            MakeTerminalAction { terminal_id: n }
        }
        Concat(ir.rules.len());
        MakeLayerMacroDefinition();
    }
}

fn capitalize(snake_case: &str) -> String {
    snake_case.split('_').flat_map(|word| {
        let mut chars = word.chars();
        let letter = chars.next();
        letter.into_iter().flat_map(|ch| ch.to_uppercase()).chain(chars)
    }).collect()
}

impl IrTranslator {
    pub fn new(ir: Ir) -> Self {
        IrTranslator { ir: ir }
    }

    pub fn translate(&self) -> InstructionList {
        InstructionList {
            list: translate_ir(&self.ir),
        }
    }
}

impl InstructionList {
    pub fn generate(self) -> rs::TokenStream {
        let mut state = InterpreterState {
            stack: vec![],
        };
        for instruction in self.list {
            run_instruction(&mut state, instruction);
        }
        let result = state.stack.pop().expect("interpreter expected at least one result");
        assert!(state.stack.is_empty(), "interpreter expected exactly one result");
        result
    }
}

impl InterpreterState {
    fn pop(&mut self) -> rs::TokenStream {
        self.stack.pop().expect("stack is empty")
    }

    fn pop_n(&mut self, count: usize) -> Vec<rs::TokenStream> {
        let n = self.stack.len().checked_sub(count).expect("stack underflow??");
        self.stack.split_off(n)
    }

    fn peek(&mut self) -> rs::TokenStream {
        self.stack.last().expect("stack is empty").clone()
    }
}

fn eval_instruction(state: &mut InterpreterState, instruction: Instruction) -> rs::TokenStream {
    match instruction {
        Concat(count) => {
            let list = state.pop_n(count);
            quote! { #(#list)* }
        }
        ConcatSeparated(count, separator) => {
            let list = state.pop_n(count);
            let mut tokens = rs::TokenStream::new();
            let mut iter = list.into_iter();
            let first = iter.next();
            for elem in iter.rev() {
                tokens.extend(elem);
                tokens.extend(separator.clone());
            }
            if let Some(last) = first {
                tokens.extend(last);
            }
            tokens
        }
        Dup => {
            state.peek()
        }
        MakeCommonDefinitions {
            UpperParseFactory,
            UpperParse,
            UpperInferTree,
            UpperInfer,
            InferConstraint,
        } => {
            quote! {
                struct EnumStream<'g, C, D, I> {
                    closure: C,
                    eval_closure: D,
                    #(#language: #Language<'g, I>,)*
                }

                struct Parse<'g, I> where I: Infer {
                    #(
                        #sub_parse: #SubParse<'g, I>,
                    )*
                    traced: bool,
                }

                struct ExhaustedParse<'g, I> where I: Infer {
                    parse: Parse<'g, I>,
                    input_pos: usize,
                }

                // Either `parse` or `traced_parse` may be dead code.
                #[allow(dead_code)]
                impl<'g, C, D, I> EnumStream<'g, C, D, I>
                    where
                        D: FnMut(&'g mut Parse<'g, I>),
                        I: Infer,
                {
                    fn parse<'g, I, Iter>(&'g mut self, into_iter: Iter)
                        -> Result<Parse<'g, I>, ExhaustedParse<'g, I>>
                        where
                            Iter: IntoIterator<Item = I::#UpperInfer::Leaf>,
                            Iter::Item: Copy,
                            C: Fn(usize, Iter::Item) -> bool,
                            
                    {
                        self.common_parse(into_iter, false)
                    }

                    fn traced_parse<'g, I, Iter>(&'g mut self, into_iter: Iter)
                        -> Result<Parse<'g, I>, ExhaustedParse<'g, I>>
                        where
                            Iter: IntoIterator<Item = I::#UpperInfer::Leaf>,
                            Iter::Item: Copy,
                            C: Fn(usize, Iter::Item) -> bool,
                    {
                        self.common_parse(into_iter, true)
                    }
                }
                
                trait Infer {
                    #(
                        type #SubInfer: #SubInfer;
                    )*
                }

                struct InferVal<#SubInferVars>(
                    ::std::marker::PhantomData<(#SubInferVars)>
                );

                impl<#SubInferVars> Infer for InferVal<#SubInferVars>
                {
                    #(
                        type #SubInfer = #SubInferVal;
                    )*
                }

                impl<'g, I> fmt::Debug for ExhaustedParse<'g, I>
                    where I: #UpperInferTree<'g> + 'g
                {
                    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
                        self.parse.fmt_exhaustion(fmt, self.input_pos)
                    }
                }
            }
        }
        MakeTerminalAccess {
            terminal_name,
        } => {
            quote! {
                terminal_accessor.#terminal_name()
            }
        }
        MakeEnumStreamParserCommonParseImpl {
            UpperParse,
            UpperInferTree,
        } => {
            let tokens = state.pop();
            quote! {
                impl<C, D> EnumStream<C, D> {
                    #[inline]
                    fn common_parse<'g, I, Iter>(&'g mut self, into_iter: Iter, traced: bool)
                        -> Result<Parse<'g, I>, ExhaustedParse<'g, I>>
                        where C: Fn(usize, Iter::Item) -> bool,
                            D: FnMut(&'g mut Parse<'g, I>),
                            Iter: IntoIterator<Item = I::#UpperInfer::Leaf>,
                            Iter::Item: Copy,
                            I: Infer,
                    {
                        let terminal_accessor = self.#upper_language.terminal_accessor();
                        let tokens = &[#tokens];
                        let mut parse = Parse {
                            #(
                                #parse: self.#language.parse()
                            ),*
                            traced: traced,
                        };
                        parse.#upper_begin_input();
                        let iter = into_iter.into_iter();
                        for (i, elem) in iter.enumerate() {
                            parse.#upper_begin_earleme();
                            for (id, &token) in tokens.iter().enumerate() {
                                if self.closure(id, elem) {
                                    self.#upper_scan_tok(token, elem);
                                }
                            }
                            let success = parse.#upper_end_earleme();
                            if !success {
                                return Err(ExhaustedParse {
                                    parse: parse,
                                    input_pos: i,
                                });
                            }
                        }
                        parse.#upper_end_input();
                        (self.eval_closure)(
                            &mut parse
                        );
                        Ok(parse)
                    }
                }
            }
        }
        MakeValueConditionally {
            terminal_id,
            UpperValue,
            variant,
        } => {
            quote! {
                if sym == tokens[#terminal_id] {
                    #UpperValue::#variant(value)
                } else
            }
        }
        MakeValueConditionalsTail => {
            quote! (
                {
                    unreachable!()
                }
            )
        }
        MakeNegativePatternMatchArm {
            pattern
        } => {
            quote! {
                Some(#pattern) => return false,
            }
        }
        MakePositiveMatch {
            patterns,
            guard,
        } => {
            let mut stmt = if let Some(guard) = guard {
                quote! {
                    if #guard {
                        return true
                    }
                }
            } else {
                quote! { return true }
            };
            for pattern in patterns.into_iter().rev() {
                stmt = quote! {
                    match item {
                        Some(#pattern) => { #stmt }
                        _ => {}
                    }
                }
            }
            stmt
        },
        MakeTerminalAction {
            terminal_id,
        } => {
            let negative_pattern_match_arms = state.pop();
            let positive_match = state.pop();
            quote! {
                if id == #terminal_id {
                    let item = Some(item);
                    match item {
                        #negative_pattern_match_arms
                        _ => {}
                    }
                    #positive_match
                }
            }
        }
        MakeLayerMacroDefinition {
            layer_macro,
            UpperTerminalAccessor,
        } => {
            let tokens = state.pop();
            let value_conditionals = state.pop();
            let terminal_actions = state.pop();
            quote! {
                struct EnumResult(#UpperValue);
                struct EnumResultOnly(#UpperValue);

                impl EnumResult {
                    const fn len() -> usize {
                        1
                    }

                    fn into_iter_summands(self) -> iter::Once<EnumResultOnly> {
                        iter::once(EnumResultOnly(self.0))
                    }
                }

                impl EnumResultOnly {
                    #(
                        fn #terminal(self) -> #T {
                            self.0
                        }
                    )*
                }

                let #eval_closure = |_, sym, value| {
                    let sym = ($node).terminal;
                    let value = ($node).value;
                    let terminal_accessor = #UpperTerminalAccessor;
                    let tokens = &[#tokens];
                    EnumResult(#value_conditionals)
                };

                let top_result = |top_eval_closure| {
                    EnumStream {
                        closure: |id: usize, item| {
                            #terminal_actions
                            false
                        },
                        eval_closure: top_eval_closure,
                        #(
                            #language: #Language::new(),
                        )*
                    }
                };
            }
        }
    }
}

fn run_instruction(state: &mut InterpreterState, instruction: Instruction) {
    let val = eval_instruction(state, instruction);
    state.stack.push(val);
}
