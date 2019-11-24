#![allow(non_snake_case)]

use std::iter;

use cfg::symbol::Symbol;
use cfg_regex::Class;
use gearley::grammar::InternalGrammar;

use instruction::{LowerInstructionList, LowerInstruction::{self, *}};
use rs;
use proc_macro2::{Literal, TokenTree};
// use quote::ToTokens;

// Info for generation.

struct InterpreterState {
  stack: Vec<rs::TokenStream>,
}

impl LowerInstructionList {
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

fn run_instruction(state: &mut InterpreterState, instruction: LowerInstruction) {
    let val = eval_instruction(state, instruction);
    state.stack.push(val);
}

fn eval_instruction(state: &mut InterpreterState, instruction: LowerInstruction) -> rs::TokenStream {
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
        // Literal(lit) => {
        //     lit
        // }

        MakeTerminalAccessorFn { name, id } => {
            // Id is internal symbol id.
            quote! {
                #[inline]
                fn #name(&self) -> Symbol {
                    Symbol::from(#id as u32)
                }
            }
        }
        MakeTerminalAccessorStruct => {
            let fns = state.pop();
            quote! {
                struct TerminalAccessor;

                #[allow(non_snake_case)]
                impl TerminalAccessor {
                    #fns
                }
            }
        }

        // value enum
        // MakeEnumVariant {
        //     name,
        //     ty,
        // } => {
        //     quote! {
        //         #name(#ty),
        //     }
        // }
        // MakeValueEnum => {
        //     let variants = state.pop();
        //     quote! {
        //         #[derive(Clone)]
        //         #[allow(non_camel_case_types)]
        //         enum Value<I> where I: Infer {
        //             #variants
        //         }
        //     }
        // }

        // // epsilon rules
        // MakeEpsilonRuleActionDefinition {
        //     name,
        //     continuation_label,
        // } => {
        //     let actions = state.pop();
        //     quote! {
        //         macro_rules! #name {
        //             ($x:expr) => {{
        //                 let mut #continuation_label = $x;
        //                 #actions
        //             }}
        //         }
        //     }
        // }

        // MakeLanguage {
        //     ParseFactory,
        //     TerminalAccessor,
        //     InferTree,
        //     Value,
        //     Parse,
        //     storage_str,
        //     trace_ids,
        //     trace_pos,
        //     trace_tokens,
        //     sym_names,
        // } => {
        //     let trace_ids = trace_ids.iter();
        //     let trace_map = trace_pos.iter().map(|v| v.iter());
        //     let trace_tokens = trace_tokens.iter().map(|rule_tokens| {
        //         rule_tokens.iter().map(|tok| &tok[..])
        //     });
        //     // TODO: put in the same structure as TRACE_INFO
        //     let sym_names = sym_names.iter().map(|name| &name[..]);
        //     quote! {
        //         static #SERIALIZED_GRAMMAR: &'static [u8] = #storage_str;
        //         static #TRACE_INFO: TraceInfo = TraceInfo {
        //             ids: &[
        //                 #(#trace_ids),*
        //             ],
        //             map: &[
        //                 #(&[#(#trace_map),*]),*
        //             ],
        //             tokens: &[
        //                 #(&[#(#trace_tokens),*]),*
        //             ],
        //         };
        //         static #SYM_NAMES: &'static [&'static str] = &[
        //             #(#sym_names),*
        //         ];

        //         struct Language<'g, I> {
        //             grammar: Rc<grammar::InternalGrammar>,
        //             bocage: Bocage<'g, 'g, Rc<grammar::InternalGrammar>, I::Node, #Value<I::Infer>>>,
        //         }

        //         impl<'g, I> Language<'g, I> {
        //             fn new() -> Language<'g, I> {
        //                 let grammar: grammar::InternalGrammar = serde_cbor_from_slice(SERIALIZED_GRAMMAR).unwrap();
        //                 let grammar = Rc::new(grammar);
        //                 let bocage = Bocage::new(grammar.clone());
        //                 #Language {
        //                     grammar: grammar,
        //                     bocage: bocage,
        //                 }
        //             }

        //             fn parse<'g, I>(&'g self) -> SubParse<'g, I>
        //                 where I: InferTree<'g> + 'g
        //             {
        //                 let recognizer = Recognizer::new(&*self.grammar, &self.bocage);
        //                 let traversal = Traversal::new(&self.bocage, NullOrder::new());

        //                 SubParse {
        //                     grammar: &*self.grammar,
        //                     exhausted: false,
        //                     finished_node: None,
        //                     store: Arena::new(),
        //                     recognizer: recognizer,
        //                     bocage: bocage,
        //                     traversal: traversal,
        //                     inference_marker: ::std::marker::PhantomData,
        //                 }
        //             }
        //         }
        //     }
        // }
        // MakeInferTypeVars {
        //     // count,
        //     infer,
        // } => {
        //     quote! {
        //         #(#infer),*
        //     }
        // }
        // MakeInfer {
        //     Infer,
        //     ValueInfer,
        //     InferTree,
        //     InferTreeVal,
        //     LowerInferConstraint,
        //     infer,
        // } => {
        //     let Vars = state.pop();
        //     let Var = infer.iter();
        //     quote! {
        //         trait #SubInfer {
        //             #(type #Var;)*
        //         }

        //         #[derive(Clone, Copy)]
        //         struct #SubInferVal<#Vars>(
        //             ::std::marker::PhantomData<(#Vars)>
        //         );

        //         impl<#Vars> #SubInfer for #SubInferVal<#Vars> {
        //             #(type #Var = #Var;)*
        //         }
        //     }
        // }

        // MakeParseDef {
        //     start_sym,
        //     start_type,
        //     start_variant,
        // } => {
        //     quote! {
        //         struct SubParse<'g, I> where I: SubInfer {
        //             grammar: &'g grammar::InternalGrammar,
        //             checkpoint: Checkpoint<I::Leaf, Value<I>>,
        //             current_num_scanned: usize,
        //             store: Arena<Value<I::Infer>>,
        //             recognizer: Recognizer<'g, 'g, Bocage<'g, 'g, 'g, I::Leaf, Value<I>>>,
        //             traversal: TraversalUnordered<'g, I::Leaf, Value<I>>,
        //             inference_marker: ::std::marker::PhantomData<I>,
        //         }

        //         impl<'g, I> Parse<'g, I>
        //             where I: Infer
        //         {
        //             fn #begin_input(&mut self) {
        //                 self.#upper_begin_input();
        //             }

        //             // LATM handling.
        //             fn #begin_earleme(&mut self) {
        //                 if self.checkpoint.is_empty() {
        //                     for sym in self.#upper_predicted_symbols() {
        //                         self.recognizer.predict(sym);
        //                     }
        //                 }
        //             }

        //             fn #scan_tok(&mut self, token: Symbol, value: I::Node) {
        //                 self.recognizer.scan(token, value);
        //                 self.checkpoint.scan(token, value);
        //             }

        //             fn #end_earleme(&mut self) -> bool {
        //                 // TODO: trace this layer.
        //                 let exhausted = !self.recognizer.advance();
        //                 if self.recognizer.is_finished() {
        //                     self.checkpoint.checkpoint(self.recognizer.finished_node());
        //                 } else {
        //                     self.checkpoint.end_earleme(self.current_num_scanned);
        //                 }
        //                 if exhausted {
        //                     if self.checkpoint.is_present() {
        //                         self.#upper_begin_earleme();
        //                         self.checkpoint.result(self.#upper_scan_tok);
        //                         let success = self.#upper_end_earleme();
        //                         if !success {
        //                             return false;
        //                         }
        //                         self.recognizer.reset();
        //                         let scanned = self.checkpoint.take_scanned();
        //                         let num_scanned = self.checkpoint.take_num_scanned();
        //                         let mut scanned_iter = scanned.iter();
        //                         for &count in &num_scanned {
        //                             self.#begin_earleme();
        //                             for (token, value) in (&mut scanned_iter).take(count) {
        //                                 self.#scan_tok(token, value);
        //                             }
        //                             let success = self.#end_earleme();
        //                             if !success {
        //                                 return false;
        //                             }
        //                         }
        //                     } else {
        //                         // leave in exhausted state
        //                         return false;
        //                     }
        //                 }
        //             }

        //             fn #end_input(&mut self) {
        //                 if self.recognizer.is_finished() {
        //                     self.checkpoint.finished_node = Some(self.recognizer.finished_node());
        //                 }
        //                 self.#upper_end_input();
        //             }
        //         }
        //     }
        // }

        // MakeClosure {
        //     parse,
        //     action_id,
        //     rule_variant,
        //     rules_expr,
        //     arg_num,
        //     arg_variant,
        //     arg_pat,
        //     seq_action_id,
        //     seq_element_variant,
        //     seq_variant,
        //     null_symbol_id,
        //     null_num_summands,
        //     null_sym_name,
        //     null_variant,
        // } => {
        //     let parse = quote! { parse.#parse };

        //     quote! {
        //         let #eval_closure = |parse, _, root| {
        //             // Assist the inference.
        //             {
        //                 let _: &::std::marker::PhantomData<
        //                     SubInferVal<#(#infer_wildcards)*>
        //                 > = &#parse.inference_marker;
        //             };
        //             let mut cartesian_product = CartesianProduct::new();
        //             #parse.traversal.traverse(root);
        //             loop {
        //                 if let Some(deps) = #parse.traversal.traverse_deps() {
        //                     for node in deps {
        //                         match node {
        //                             TraversalBottom::Leaf(node) => {
        //                                 let result = #lower_eval_closure(
        //                                     parse,
        //                                     node.terminal,
        //                                     node.value,
        //                                 );
        //                                 let mut builder = SliceBuilder::new(&#store, result.len());
        //                                 #(
        //                                     if node.terminal == #terminal_sym {
        //                                         for value in result {
        //                                             builder.push(#Value::#variant(value.#terminal_name()));
        //                                         }
        //                                     }
        //                                 )else*
        //                                 else {
        //                                     unreachable!("wrong sym")
        //                                 }
        //                                 node.result(builder.into_slice());
        //                             }
        //                             TraversalBottom::Null(nulling) => {
        //                                 // The builder may be unused.
        //                                 let mut _builder = SliceBuilder::new(&#store, 0);
        //                                 // Use external symbols.
        //                                 match nulling.symbol.usize() {
        //                                     #(
        //                                         #null_symbol_id => {
        //                                             _builder.reserve(#null_num_summands);
        //                                             #null_sym_name!(|result| {
        //                                                 _builder.push(#ValueRep::#null_variant(result));
        //                                             });
        //                                         }
        //                                     )*
        //                                     id => unreachable!("nulling id {}", id)
        //                                 }
        //                                 let slice = _builder.into_slice();
        //                                 assert!(!slice.is_empty(), "built slice is empty");
        //                                 nulling.result(slice);
        //                             }
        //                         }
        //                     }
        //                 } else {
        //                     break;
        //                 }
        //                 for node in #parse.traversal.traverse_sum() {
        //                     let count = node.iter().map(|alt| alt.len()).fold(0, |acc, elem| acc + elem);
        //                     let mut slice_builder = SliceBuilder::new(&#store, count);
        //                     // ... eval:
        //                     for alt in node.iter() {
        //                         cartesian_product.from_production(&alt);
        //                         // let mut finished = false;
        //                         loop {
        //                             let result = {
        //                                 let args = cartesian_product.as_slice();
        //                                 match alt.action() {
        //                                     #(
        //                                         #action_id => {
        //                                             // `true` is to avoid irrefutable patterns
        //                                             let val = (true, #(args[#arg_num].clone(),)*);
        //                                             if let (true,
        //                                                     #(Value::#arg_variant(#arg_pat),)*) = val {
        //                                                 Value::#rule_variant(#rules_expr)
        //                                             } else {
        //                                                 unreachable!()
        //                                             }
        //                                         }
        //                                     )*
        //                                     #(
        //                                         #seq_action_id => {
        //                                             let seq_vec = args.iter().map(|arg| {
        //                                                 let val = (true, (*arg).clone());
        //                                                 if let (true,
        //                                                         Value::#seq_element_variant(elem)) = val {
        //                                                     elem
        //                                                 } else {
        //                                                     unreachable!()
        //                                                 }
        //                                             }).collect::<Vec<_>>();
        //                                             Value::#seq_variant(seq_vec)
        //                                         }
        //                                     )*
        //                                     _ => unreachable!("rule id {}", alt.action())
        //                                 }
        //                             };
        //                             // placement new?
        //                             slice_builder.push(result);
        //                             if cartesian_product.next().is_none() {
        //                                 break;
        //                             }
        //                         }
        //                     }
        //                     node.result(slice_builder.advance_slice());
        //                 }
        //             }
        //             ParseResultSum(root.values().unwrap())
        //         };
        //     }
        // }
        // MakeParseResult {
        //     infer_wildcards,
        // } => {
        //     quote! {
        //         struct ParseResultSum<'a>(&'a [Value]);
        //         struct ParseResult<'a>(&'a Value);

        //         impl ParseResult {
        //             fn len(&self) -> usize {
        //                 self.0.len()
        //             }
        //         }

        //         impl<'a> Iterator for ParseResultSum<'a> {
        //             type Item = ParseResult<'a>;

        //             fn next(&self) -> Self::Item {
        //                 self.0.next().map(ParseResult)
        //             }
        //         }

        //         impl<'a> ParseResult<'a> {
        //             #(
        //                 fn #terminal(&self) -> #T {
        //                     match self.0 {
        //                         &Value::#variant(inner) => inner.clone(),
        //                         _ => unreachable!()
        //                     }
        //                 }
        //             )*
        //         }
        //     }
        // }

        // MakeCharRange {
        //     start: rs::Literal,
        //     end: rs::Literal,
        // } => {
        //     quote! {
        //         // Implicit char range rule RHS
        //         #start ... #end
        //     }
        // }
        // MakeBnfRule {
        //     lhs: rs::Ident,
        // } => {
        //     let rhs = state.pop();
        //     quote! {
        //         // Implicit char ranges. Present with a char_classifier lower layer.
        //         #lhs ::= #rhs;
        //     }
        // }
        // MakeLowerLayer {
        //     lexer_attr,
        //     terminal_names,
        //     lexer_name,
        //     explicit_lexer_invocation_content,
        // } => {
        //     let implicit_rules = state.pop();
        //     // The lexer invocation.
        //     quote! {
        //         // ########### QUOTED CODE #########################
        //         // Inner layer.
        //         #lexer_name! {
        //             // Arguments for the inner layer.
        //             #![#lexer_attr(#(#terminal_names),*)]
        //             #implicit_rules
        //             #explicit_lexer_invocation_content
        //         }
        //         // ########### END QUOTED CODE
        //     }
        // }
        // MakeLowerLayerAsIdentity {
        //     lower_eval_closure,
        // } => {
        //     let #lower_eval_closure = |_, _, _| {
        //         unimplemented!()
        //     };
        // }

        // MakeOutermostLayerDefs {
        //     start_type,
        //     start_variant,
        //     eval_closure,
        // } => {
        //     quote! {
        //         impl Parse<'g, I> {
        //             fn fmt_exhaustion(&self, fmt: &mut fmt::Formatter, input_pos: usize)
        //                 -> Result<(), fmt::Error>
        //             {
        //                 try!(write!(fmt, "Parse error at {}:\nexpected", input_pos));
        //                 let mut terminals = self.#top_parse.recognizer.expected_terminals();
        //                 let last = terminals.next();
        //                 for terminal in terminals {
        //                     try!(write!(fmt, " `{}`,", #SYM_NAMES[terminal.usize()]));
        //                 }
        //                 if let Some(last) = last {
        //                     write!(fmt, " or `{}`.", #SYM_NAMES[last.usize()])
        //                 } else {
        //                     write!(fmt, "end of input.")
        //                 }
        //             }
        //         }

        //         impl<'g, I> Iterator for Parse<'g, I>
        //             where I: Infer,
        //         {
        //             type Item = &'g #start_type;
        //             fn next(&mut self) -> Option<Self::Item> {
        //                 match self.result.next() {
        //                     Some(&#Value::#start_variant(ref value)) => Some(value),
        //                     _ => None,
        //                 }
        //             }
        //         }

        //         top_result(#eval_closure)
        //     }
        // }

        // // final expression
        // OutermostLayer => {
        //     let defs = state.pop();
        //     quote! (
        //         { use ::panini::*; #defs }
        //     )
        // }
    }
}
