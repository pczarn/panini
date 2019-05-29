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
        Concat(ir.rules.len());
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
                struct EnumStream<C> {
                    closure: C,
                }

                struct EnumStreamParser<C, D> {
                    closure: C,
                    eval_closure: D,
                    builder: #UpperParseFactory,
                }

                struct Parse<'g, I> where I: #UpperInferTree<'g> + 'g {
                    parse: Box<#UpperParse<'g, I>>,
                }

                struct ExhaustedParse<'g, I> where I: #UpperInferTree<'g> + 'g {
                    parse: Box<#UpperParse<'g, I>>,
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
                        builder: #UpperParseFactory,
                        eval_closure: D)
                        -> EnumStreamParser<C, D>
                        where D: FnMut(&'g mut #UpperParse<'g, I>),
                            I: #UpperInferTree<'g> + 'g
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
                            D: FnMut(&'g mut #UpperParse<'g, I>),
                            Iter: IntoIterator<Item = I::Node>,
                            Iter::Item: Copy,
                            I: #UpperInferTree<'g> + 'g,
                    {
                        self.common_parse(into_iter, false)
                    }

                    fn traced_parse<'g, I, Iter>(&'g mut self, into_iter: Iter)
                        -> Result<Parse<'g, I>, ExhaustedParse<'g, I>>
                        where C: Fn(usize, Iter::Item) -> bool,
                            D: FnMut(&'g mut #UpperParse<'g, I>),
                            Iter: IntoIterator<Item = I::Node>,
                            Iter::Item: Copy,
                            I: #UpperInferTree<'g> + 'g,
                    {
                        self.common_parse(into_iter, true)
                    }
                }

                fn scan_elem<'g, C, I>(
                    closure: &mut C,
                    tokens: &[Symbol],
                    parse: &mut #UpperParse<'g, I>,
                    elem: I::Node)
                    where C: Fn(usize, I::Node) -> bool,
                        I: #UpperInferTree<'g> + 'g,
                {
                    for (id, &token) in tokens.iter().enumerate() {
                        if closure(id, elem) {
                            parse.scan_tok(token, elem);
                        }
                    }
                }

                impl<'g, I> Iterator for Parse<'g, I>
                    where I: #UpperInferTree<'g> + 'g
                {
                    type Item = <#UpperParse<'g, I> as Iterator>::Item;
                    fn next(&mut self) -> Option<Self::Item> {
                        self.parse.next()
                    }
                }

                trait #InferConstraint<'g, Node>: #UpperInfer<T = Node>
                    where Node: Copy
                {}

                impl<'g, Node, T> #InferConstraint<'g, Node> for T
                    where Node: Copy, T: #UpperInfer<T = Node>
                {}

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
                impl<C, D> EnumStreamParser<C, D> {
                    #[inline]
                    fn common_parse<'g, I, Iter>(&'g mut self, into_iter: Iter, traced: bool)
                        -> Result<Parse<'g, I>, ExhaustedParse<'g, I>>
                        where C: Fn(usize, Iter::Item) -> bool,
                            D: FnMut(&'g mut #UpperParse<'g, I>),
                            Iter: IntoIterator<Item = I::Node>,
                            Iter::Item: Copy,
                            I: #UpperInferTree<'g> + 'g,
                    {
                        let terminal_accessor = self.builder.terminal_accessor();
                        let tokens = &[#tokens];
                        let mut parse_box = Box::new(self.builder.new_parse());
                        let parse: &'g mut #UpperParse<'g, I>;
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
                macro_rules! #layer_macro {
                    (
                        @closure
                        $upper_builder:expr,
                        $ignored__parse:expr,
                        $node:expr;
                    ) => ({
                        let terminal_accessor = #UpperTerminalAccessor;
                        let upper_builder = &mut $upper_builder;
                        let sym = ($node).terminal;
                        let value = ($node).value;
                        let tokens = &[#tokens];
                        let value = #value_conditionals {
                            unreachable!()
                        };
                        upper_builder.reserve(1);
                        upper_builder.push(value);
                    });
                    (@builder @factory [$factory:expr] @closure [$closure:expr]) => (
                        EnumStream::new(|id: usize, item| {
                            #terminal_actions
                            false
                        }).with_parse_builder_and_eval_closure($factory, $closure)
                    );
                    (@get $parse:expr) => (
                        $parse
                    )
                }
            }
        }
    }
}

fn run_instruction(state: &mut InterpreterState, instruction: Instruction) {
    let val = eval_instruction(state, instruction);
    state.stack.push(val);
}
