//! Translate from IR to the lowest structures.
//! Should this module have another name? The parallel to rustc trans will be lost on some people,
//! and is unclear even to me.

#![allow(non_snake_case)]

use std::iter;
use std::collections::HashSet;
use std::collections::HashMap;
use std::collections::BTreeMap;
use std::fmt::Write;
use std::u32;
use std::mem;

use quote::ToTokens;

use cfg::symbol::Symbol;
use cfg::ContextFreeRef;
// use cfg::remap::Mapping;
use cfg::rule::GrammarRule;
use cfg::rule::container::RuleContainer;
use cfg::symbol::SymbolBitSet;
// use gearley::grammar::InternalGrammar;

// use middle::{Ir, Ty, AutoTy};
// use middle::action::{Action, ActionExpr};
use middle::ir::Ir;
use output::instruction::{Instruction, InstructionList, translate_ir};

const CHAR_CLASSIFIER_MACRO_NAME: &'static str = "char_classifier";

pub struct IrTranslator {
    // Intermediate representation (higher-level).
    pub ir: Ir,

    // temporary maps
    // pub type_map: BTreeMap<Symbol, ComputedType>,
    // pub variant_names: BTreeMap<Symbol, rs::Term>,

    // Compile-time assertions.
    // assert_type_equality: Vec<(rs::TokenStream, rs::TokenStream)>,
    // Terminal symbols.
    pub terminals: Vec<Symbol>,
    // Evaluated node variants with their inner types.
    // pub variant_map: Vec<(rs::Term, rs::TokenStream)>,
    // pub infer: BTreeMap<Symbol, u32>,
}

type ItemDefId = u32;
type RustTyId = u32;

// Type

// #[derive(Clone)]
// pub enum ComputedType {
//     RustTy(RustTyId),
//     Vec {
//         ty: Box<ComputedType>,
//         rhs_sym: Symbol,
//     },
//     Unit,
//     Tuple {
//         types: Vec<ComputedType>,
//         fields: Vec<Symbol>,
//     },
//     Struct {
//         nonterminal: Symbol,
//         fields: Vec<Symbol>,
//     },
//     Infer {
//         nonterminal: Symbol,   
//     },
//     Terminal,
// }

// Creation

impl IrTranslator {
    pub fn new(ir: Ir) -> Self {
        // let layer_id = ir.arguments_from_outer_layer.as_ref()
        //                                             .map_or(0, |layer| layer.current_level());

        let mut this = IrTranslator {
            ir: ir,
            // type_map: BTreeMap::new(),
            // variant_names: BTreeMap::new(),
            // variant_map: vec![],
            // assert_type_equality: vec![],
            // infer: vec![],
            terminals: vec![],
        };
        this.compute_variant_map();
        this
    }

    fn compute_variant_map(&mut self) {
        self.compute_terminals();
        // self.compute_variant_names();
        // self.compute_types();
        // self.compute_type_equality_assertions();
        // self.compute_infer();

        // assert_eq!(self.variant_names.len(), self.type_map.len());

        // self.variant_map.extend(
        //     self.variant_names.iter().zip(self.type_map.iter()).map(|((sym1, name), (sym2, ty))| {
        //         assert_eq!(sym1, sym2);
        //         (*name, ty.generate())
        //     })
        // );
    }

    fn compute_terminals(&mut self) {
        for terminal in SymbolBitSet::terminal_set(&*self.ir.grammar).iter() {
            // Use external symbols, but translate later.
            let terminal = self.ir.externalize(terminal);
            self.terminals.push(terminal);
            // self.ir.type_map.insert(terminal, Ty::RustTerminalTy);
            // self.type_map.insert(terminal, ComputedType::Terminal);
        }
    }

    // fn compute_variant_names(&mut self) {
    //     let ir = &self.ir;
    //     let terminals = &self.terminals;
    //     self.variant_names = ir.type_map.iter().map(|(&external_sym, _ty)| {
    //         let name = ir.name_of_external(external_sym).unwrap();
    //         let opt_id = if terminals.iter().find(|&&sym| sym == external_sym).is_some() {
    //             None
    //         } else {
    //             Some(name.0)
    //         };
    //         let capitalized = self.capitalized_name(&name.as_str()[..], opt_id);
    //         (external_sym, rs::Ident::new(&capitalized[..], rs::Span::call_site()))
    //     }).collect();
    // }

    // fn compute_types(&mut self) {
    //     let mut work: Vec<_> = self.ir.type_map.iter().filter_map(|(sym, ty)|
    //         if ty.is_terminal() {
    //             None
    //         } else {
    //             Some((sym.clone(), ty.clone()))
    //         }
    //     ).collect();
    //     while !work.is_empty() {
    //         let work_len = work.len();
    //         work.retain(|&(nonterminal, ref ty)| {
    //             if let Some(computed_type) = self.try_compute_type(nonterminal, ty) {
    //                 self.type_map.insert(nonterminal, computed_type);
    //                 false
    //             } else {
    //                 true
    //             }
    //         });
    //         if work.len() == work_len {
    //             panic!("cycle detected");
    //         }
    //     }
    // }

    // fn compute_type_equality_assertions(&mut self) {
    //     let assert_type_equality = mem::replace(&mut self.ir.assert_type_equality, vec![]);
    //     let assert_type_equality = assert_type_equality.into_iter().filter_map(|(sym, ty)| {
    //         if let Ty::Infer = ty {
    //             None
    //         } else {
    //             let computed = self.try_compute_type(sym, &ty).expect("type not computed yet");
    //             Some((
    //                 self.type_map[&sym].generate(),
    //                 computed.generate()
    //             ))
    //         }
    //     }).collect::<Vec<_>>();
    //     self.assert_type_equality.extend(assert_type_equality);
    // }

    // fn compute_infer(&self) {
    //     for (nonterminal, ty) in self.type_map.iter() {
    //         match ty {
    //             &ComputedType::Infer { nonterminal } => {
    //                 let num = self.infer.len();
    //                 self.infer.insert(nonterminal, num as u32);
    //             }
    //             _ => {}
    //         }
    //     }
    // }

    // fn direct_dependencies<F>(&self, ty: &ComputedType) -> Vec<Option<&ComputedType>> {
    //     match ty {
    //         &ComputedType::Unit
    //         | &ComputedType::Infer { .. }
    //         | &ComputedType::RustTy(_)
    //         | &ComputedType::Terminal => {
    //             vec![]
    //         }
    //         &ComputedType::Tuple { ref fields, .. } | &ComputedType::Struct { ref fields, .. } => {
    //             fields.iter().map(|sym| self.type_map.get(sym)).collect()
    //         }
    //         &ComputedType::Vec { rhs_sym, .. } => {
    //             vec![self.type_map.get(rhs_sym)]
    //         }
    //     }
    // }

    // fn transitive_dependencies(&self, ty: &ComputedType) -> Vec<&ComputedType> {
    //     let mut dependencies = self.direct_dependencies(ty).into_iter().flat_map(|ty| ty).collect();
    //     let mut next_dependencies = vec![];
    //     let mut final_dependencies = vec![];
    //     while !dependencies.is_empty() {
    //         for ty in dependencies.into_iter() {
    //             let direct_dependencies = self.direct_dependencies(ty).into_iter().flat_map(|ty| ty).collect();
    //             if direct_dependencies.is_empty() {
    //                 final_dependencies.push(ty);
    //             } else {
    //                 next_dependencies.extend(direct_dependencies.into_iter());
    //             }
    //         }
    //         dependencies = next_dependencies;
    //     }
    //     final_dependencies
    // }

    // fn try_compute_type(&mut self, nonterminal: Symbol, ty: &Ty<Symbol>) -> Option<ComputedType> {
    //     let ty = self.compute_type(nonterminal, ty);
    //     if self.direct_dependencies(&ty).into_iter().all(|maybe_ty| maybe_ty.is_some()) {
    //         Some(ty)
    //     } else {
    //         None
    //     }
    // }

    // fn compute_type(&mut self, nonterminal: Symbol, ty: &Ty<Symbol>) -> ComputedType {
    //     match ty {
    //         &Ty::Auto(AutoTy::Tuple { ref fields }) => {
    //             match fields.len() {
    //                 0 => {
    //                     ComputedType::Unit
    //                 }
    //                 1 => {
    //                     self.type_map[&fields[0]].clone()
    //                 }
    //                 _ => {
    //                     ComputedType::Tuple {
    //                         types: fields.iter().map(|sym| self.type_map[sym].clone()).collect(),
    //                         fields: fields.clone(),
    //                     }
    //                 }
    //             }
    //         }
    //         &Ty::Auto(AutoTy::Struct { ref members }) => {
    //             // let capitalized_name = self.variant_names[&nonterminal];
    //             ComputedType::Struct {
    //                 nonterminal,
    //                 fields: members.iter().map(|(_, sym)| sym).collect(),
    //             }
    //             // let member_defs = members.iter().map(|(&name, &sym)| {
    //             //     let ty = self.type_map[&sym].generate();
    //             //     quote! { #name: #ty }
    //             // });
    //             // let member_clones = members.iter().map(|(&name, _)| {
    //             //     quote! { #name: #name.clone() }
    //             // });
    //             // let item_defs = quote! {
    //             //     struct #capitalized_name<#generics> {
    //             //         #(#member_defs)*
    //             //     }

    //             //     impl<#generics> Clone for #capitalized_name {
    //             //         fn clone(&self) -> #ty {
    //             //             #capitalized_name {
    //             //                 #(#member_clones)*
    //             //             }
    //             //         }
    //             //     }
    //             // };
    //         }
    //         &Ty::RustTy(rust_ty_id) => {
    //             ComputedType::RustTy(rust_ty_id)
    //         }
    //         &Ty::SequenceVec(rhs_sym) => {
    //             ComputedType::Vec {
    //                 ty: Box::new(self.type_map[&rhs_sym].clone()),
    //                 rhs_sym,
    //             }
    //         }
    //         &Ty::Infer => {
    //             ComputedType::Infer { nonterminal }
    //         }
    //     }
    // }

    // fn capitalized_name(&self, name: &str, opt_id: Option<u32>) -> String {
    //     let mut capitalized: String = name.split('_').flat_map(|word| {
    //         let mut chars = word.chars();
    //         let letter = chars.next();
    //         letter.into_iter().flat_map(|ch| ch.to_uppercase()).chain(chars)
    //     }).collect();
    //     // Optionally add the unique id.
    //     if let Some(id) = opt_id {
    //         write!(capitalized, "_{}", id).unwrap();
    //     }
    //     capitalized
    // }
}

// Generation, after the following things are computed:
// * variant map, with variant names
// * type equality assertions

impl IrTranslator {
    pub fn generate(&mut self) -> InstructionList {
        let list = translate_ir(self);
        InstructionList { list }
        // let epsilon_actions = self.generate_epsilon_actions();

        // let internal_grammar = InternalGrammar::from_processed_grammar_with_maps(
        //     self.ir.grammar.clone(),
        //     &Mapping::new(0),
        //     &self.ir.nulling_grammar,
        // );

        // let mut processed_rule = vec![];
        // let mut processed_sequences = vec![];

        // let mut seen_origin = HashSet::new();
        // // For generating actions.
        // let external_origins = self.ir.grammar.rules().filter_map(|rule| {
        //     if let Some(origin) = rule.history().origin() {
        //         if seen_origin.insert(origin) {
        //             Some(origin)
        //         } else {
        //             None
        //         }
        //     } else {
        //         // Skip this rule.
        //         None
        //     }
        // });

        // for origin in external_origins {
        //     // Get the basic rule.
        //     let basic_rule = &self.ir.basic_rules[origin as usize];
        //     // The basic rule's lhs is often equal to the processed rule's rhs.
        //     // They are not equal for precedenced rules, due to their rewrite.
        //     // We should use the basic rule's lhs. (It is already external.)
        //     let rule_lhs = basic_rule.lhs.elem;
        //     let variant = self.variant_names[&rule_lhs];

        //     // Diverge on sequence rules
        //     if let Some((rust_expr, patterns)) = self.get_action(origin as usize) {
        //         processed_rule.push(GenRule {
        //             id: origin as u32,
        //             variant: variant,
        //             action: rust_expr,
        //             args: patterns
        //         });
        //     } else {
        //         let elem_variant = self.variant_names[&basic_rule.rhs[0].elem];
        //         processed_sequences.push(GenSequence {
        //             id: origin as u32,
        //             variant: variant,
        //             elem_variant: elem_variant,
        //         });
        //     }
        // }

        // let terminal_names = self.terminals.iter().map(|&terminal| {
        //     self.ir.name_of_external(terminal).unwrap().to_ident()
        // }).collect();
        // let terminal_ids = self.terminals.iter().map(|&terminal| {
        //     self.ir.internalize(terminal).unwrap().usize()
        // }).collect();

        // let start = self.ir.grammar.start();
        // let external_start = self.ir.externalize(start);

        // let start_variant = self.variant_names[&external_start];
        // let start_type = self.type_map[&external_start].clone();

        // let grammar_parts = internal_grammar.to_parts();

        // let outer_layer = self.ir.arguments_from_outer_layer.as_ref();
        // let inner_layer_level = outer_layer.map_or(0, |layer| layer.current_level() as u32) + 1;

        // let arguments_from_outer_layer_opt = self.ir.arguments_from_outer_layer.as_ref().map(|arg| {
        //     let terminal_names = arg.terminals().iter().map(|&sym| {
        //         self.ir.name_of_external(sym).unwrap().to_ident()
        //     }).collect();
        //     let terminal_variants = arg.terminals().iter().map(|&sym| {
        //         self.variant_names[&sym]
        //     }).collect::<Vec<_>>();
        //     let terminal_bare_variants = terminal_variants.iter().map(|name| {
        //         rs::Ident::new(&name[.. name.rfind("_").unwrap()], rs::Span::call_site())
        //     }).collect();
        //     GenArgumentsFromOuterLayer {
        //         terminal_names: terminal_names,
        //         terminal_variants: terminal_variants,
        //         terminal_bare_variants: terminal_bare_variants,
        //     }
        // });

        // let inner_layer = match &self.ir.invocation_of_inner_layer {
        //     &InvocationOfInnerLayer::Invoke { ref lexer_invocation, ref embedded_strings } => {
        //         Some(GenInvocationOfInnerLayer {
        //             lexer_name: lexer_invocation.name(),
        //             lexer_tts: lexer_invocation.tts(),
        //             str_lhs: embedded_strings.iter().map(|embed| {
        //                 self.ir.name_of_external(embed.symbol.elem).unwrap().to_ident()
        //             }).collect(),
        //             str_rhs: embedded_strings.iter().map(|embed| {
        //                 embed.string.node
        //             }).collect(),
        //             char_range_lhs: vec![],
        //             char_ranges: vec![],
        //         })
        //     }
        //     &InvocationOfInnerLayer::CharClassifier(ref char_ranges) => {
        //         Some(GenInvocationOfInnerLayer {
        //             lexer_name: rs::Ident::new(CHAR_CLASSIFIER_MACRO_NAME, rs::Span::call_site()),
        //             lexer_tts: rs::TokenStream::new(),
        //             str_lhs: vec![],
        //             str_rhs: vec![],
        //             char_range_lhs: char_ranges.iter().map(|&(_, sym)| {
        //                 self.ir.name_of_external(sym).unwrap().to_ident()
        //             }).collect(),
        //             char_ranges: char_ranges.iter().map(|&(range, _)| {
        //                 range
        //             }).collect(),
        //         })
        //     }
        //     &InvocationOfInnerLayer::None => {
        //         None
        //     }
        // };
        // // Names of all internal symbols
        // let sym_names = (0 .. grammar_parts.num_syms).map(|sym_id| {
        //     let internal_sym = Symbol::from(sym_id);
        //     let external_sym = self.ir.externalize(internal_sym);
        //     let sym_name = self.ir.name_of_external(external_sym);
        //     if let Some(sym_name) = sym_name {
        //         sym_name.as_str().to_string()
        //     } else {
        //         format!("g{}", sym_id)
        //     }
        // }).collect();
        // // Per-rule IDs
        // let trace_rule_ids = self.ir.trace_sources.iter().map(|source| {
        //     source.rule_id
        // }).collect();
        // // Positions
        // let trace_rule_pos = self.ir.trace_sources.iter().map(|source| {
        //     source.rule_pos.clone()
        // }).collect();
        // // Tokens
        // let trace_tokens = self.ir.trace_tokens.clone();
        // // Construct result
        // GenParser {
        //     grammar_parts: grammar_parts,

        //     start_variant: start_variant,
        //     start_type: start_type,

        //     epsilon_actions: epsilon_actions,
        //     rules: processed_rule,
        //     sequences: processed_sequences,

        //     variant_map: self.variant_map.clone(),

        //     terminal_names: terminal_names,
        //     terminal_ids: terminal_ids,

        //     arguments_from_outer_layer: arguments_from_outer_layer_opt,

        //     inner_layer_level: inner_layer_level,
        //     inner_layer: inner_layer,

        //     trace_rule_ids: trace_rule_ids,
        //     trace_rule_pos: trace_rule_pos,
        //     trace_tokens: trace_tokens,

        //     sym_names: sym_names,

        //     item_definitions: self.item_definitions.clone(),

        //     infer: self.infer.clone(),

        //     unique_names: self.unique_names,
        // }
    }

    // fn get_action(
    //     &self,
    //     origin: usize)
    //     -> Option<Vec<GenArg>>
    // {
    //     let basic_rule = &self.ir.basic_rules[origin];
    //     let mut patterns = vec![];

    //     match &basic_rule.action {
    //         &Action::Struct { ref deep_binds, ref shallow_binds, ref expr } => {
    //             if !deep_binds.is_empty() {
    //                 for &rhs_pos in deep_binds {
    //                     let rhs_sym = basic_rule.rhs[rhs_pos].elem;
    //                     let variant = self.variant_names[&rhs_sym];
    //                     let pat = self.get_auto_pattern(rhs_sym).expect("auto pattern not found");
    //                     patterns.push(GenArg {
    //                         num: rhs_pos,
    //                         nonterminal: rhs_sym,
    //                     });
    //                 }
    //             } else if !shallow_binds.is_empty() {
    //                 for &(rhs_pos, ident) in shallow_binds {
    //                     let rhs_sym = basic_rule.rhs[rhs_pos].elem;
    //                     // let variant = self.variant_names[&rhs_sym];
    //                     // let pat = ident.into_token_stream();

    //                     patterns.push(GenArg {
    //                         num: rhs_pos,
    //                         nonterminal: rhs_sym,
    //                     });
    //                 }
    //             }

    //             match expr {
    //                 &ActionExpr::Auto => {
    //                     self.get_auto_expr(basic_rule.lhs.node)
    //                 }
    //                 &ActionExpr::Inline { ref expr } => {
    //                     expr.clone()
    //                 }
    //             }
    //         }
    //         &Action::Tuple { ref tuple_binds } => {
    //             let idents: Vec<_>;
    //             idents = tuple_binds.iter().map(|&pos| {
    //                 rs::Ident::new(format!("arg{}", pos), rs::Span::call_site())
    //             }).collect();
    //             patterns = tuple_binds.iter().zip(idents.iter()).map(|(&pos, &ident)| {
    //                 let variant = self.variant_names[&basic_rule.rhs[pos].node];
    //                 let pat = ident.into_token_stream();
    //                 GenArg {
    //                     num: pos,
    //                     variant: variant,
    //                     pat: pat
    //                 }
    //             }).collect();
    //             match idents.len() {
    //                 0 => {
    //                     quote! { () }
    //                 }
    //                 1 => {
    //                     idents[0].into_token_stream()
    //                 }
    //                 _ => {
    //                     quote! { ( #(#idents),* ) }
    //                 }
    //             }
    //         }
    //         &Action::Sequence => {
    //             return None;
    //         }
    //     };
    //     Some((rust_expr, patterns))
    // }

    // fn get_auto_expr(&self, nonterminal: Symbol) -> rs::TokenStream {
    //     match &self.ir.type_map[&nonterminal] {
    //         &Ty::Auto(AutoTy::Struct { ref members }) => {
    //             let name = self.variant_names[&nonterminal];
    //             let members2 = members;
    //             quote! {
    //                 #name {
    //                     #(#members: #members2),*
    //                 }
    //             }
    //         }
    //         _ => unreachable!()
    //     }
    // }

    // fn get_auto_pattern(&self, nonterminal: Symbol) -> Option<rs::TokenStream> {
    //     match &self.ir.type_map[&nonterminal] {
    //         &Ty::Auto(AutoTy::Struct { ref members }) => {
    //             let name = self.variant_names[&nonterminal];
    //             let mut pats = vec![];
    //             for (&id, &sym) in members {
    //                 // Recursion
    //                 if let Some(pat) = self.get_auto_pattern(sym) {
    //                     pats.push(quote! { #id: #pat });
    //                 } else {
    //                     pats.push(quote! { #id });
    //                 }
    //             }
    //             Some(quote! { #name { #(),* } })
    //         }
    //         _ => {
    //             None
    //         }
    //     }
    // }

    // fn generate_epsilon_actions(&mut self) -> GenEpsilonActions {
    //     // Nulling rules
    //     // are these equal?
    //     let num_nulling_syms = self.ir.grammar.num_syms();
    //     let num_all_nulling_syms = self.ir.nulling_grammar.sym_source().num_syms();
    //     // Declarations
    //     let mut null_rules = iter::repeat(vec![]).take(num_nulling_syms).collect::<Vec<_>>();
    //     let mut null_num = iter::repeat(0).take(num_nulling_syms).collect::<Vec<_>>();
    //     let mut null_order = iter::repeat(u32::MAX).take(num_nulling_syms).collect::<Vec<_>>();
    //     // These vectors may be longer than other vectors.
    //     let mut null_deps = iter::repeat(0).take(num_all_nulling_syms).collect::<Vec<_>>();
    //     let mut null_intermediate = iter::repeat(None)
    //                                 .take(num_all_nulling_syms)
    //                                 .collect::<Vec<_>>();
    //     // Temporary variables.
    //     let mut null_work = vec![];
    //     let mut null_num_rules = 0;
    //     // Here, the name must start with "_" so that we don't get "unnecessary mut"
    //     // warnings later on. Yes, underscore prefix works for ignoring more than just
    //     // "unused variable" warnings.
    //     let continuation_label = rs::gensym("_cont");
    //     for rule in self.ir.nulling_grammar.rules() {
    //         if rule.rhs().len() == 0 {
    //             // Can `origin` be None? In sequences? No.
    //             let origin = rule.history().origin().unwrap() as usize;
    //             let basic_rule = self.ir.basic_rules.get(origin);
    //             let action = basic_rule.map(|basic_rule| &basic_rule.action);
    //             let action_expr = match action {
    //                 Some(&Action::Tuple { .. }) => {
    //                     unreachable!("found nulling rule that has a tuple type")
    //                 }
    //                 Some(&Action::Struct { expr: ActionExpr::Inline { ref expr }, .. }) => {
    //                     expr.clone()
    //                 }
    //                 // A sequence rule.
    //                 Some(&Action::Sequence) => {
    //                     quote! { Vec::new() }
    //                 }
    //                 _ => unreachable!("found unknown action")
    //             };
    //             let inner = quote! { #continuation_label(#action_expr) };
    //             null_rules[rule.lhs().usize()].push(inner);
    //             null_num[rule.lhs().usize()] += 1;
    //             if null_order[rule.lhs().usize()] > null_num_rules {
    //                 null_order[rule.lhs().usize()] = null_num_rules;
    //                 null_num_rules += 1;
    //             }
    //         } else {
    //             if rule.history().origin().is_none() {
    //                 null_intermediate[rule.lhs().usize()] = Some(rule.rhs().to_owned());
    //             }
    //             null_work.push((
    //                 rule.lhs(),
    //                 rule.rhs()[0],
    //                 rule.rhs().get(1).cloned(),
    //                 rule.history.origin()
    //             ));
    //             null_deps[rule.lhs().usize()] += 1;
    //         }
    //     }
    //     // Generate code that uses macros and a continuation-passing style.
    //     while !null_work.is_empty() {
    //         null_work.retain(|&(lhs, rhs0, rhs1, action)| {
    //             let rhs1_is_done = rhs1.map_or(true, |rhs1| null_deps[rhs1.usize()] == 0);
    //             let rhs0_is_done = null_deps[rhs0.usize()] == 0;
    //             if !rhs0_is_done || !rhs1_is_done {
    //                 // Process this later.
    //                 true
    //             } else if let Some(origin) = action {
    //                 // There are no sequence rules among nulling rules, so unwrapping is ok.
    //                 let (action_expr, patterns) = self.get_action(origin as usize).unwrap();
    //                 let mut pats = HashMap::new();
    //                 for arg in patterns.into_iter() {
    //                     pats.insert(arg.num, arg.pat);
    //                 }
    //                 let mut factors = vec![];
    //                 let mut factor_stack = vec![];
    //                 if let Some(rhs1) = rhs1 {
    //                     factor_stack.push(rhs1);
    //                 }
    //                 factor_stack.push(rhs0);
    //                 while let Some(sym) = factor_stack.pop() {
    //                     if let &Some(ref rhs) = &null_intermediate[sym.usize()] {
    //                         factor_stack.extend(rhs.iter().cloned());
    //                     } else {
    //                         factors.push(sym);
    //                     }
    //                 }
    //                 let mut inner_layer = quote! { #continuation_label(#action_expr) };
    //                 for (i, &factor) in factors.iter().enumerate().rev() {
    //                     let name = self.lowercase_name(self.ir.externalize(factor));
    //                     let pat = pats.get(&i).cloned().unwrap_or_else(|| quote! { _ });
    //                     inner_layer = quote! {
    //                         #name!(
    //                             |#pat| {
    //                                 #inner_layer
    //                             }
    //                         )
    //                     };
    //                 }
    //                 null_rules[lhs.usize()].push(inner_layer);
    //                 null_num[lhs.usize()] += factors.iter().fold(1, |acc, &factor| {
    //                     acc * null_num[factor.usize()]
    //                 });
    //                 // what is this order for?
    //                 if null_order[lhs.usize()] > null_num_rules {
    //                     null_order[lhs.usize()] = null_num_rules;
    //                     null_num_rules += 1;
    //                 }
    //                 null_deps[lhs.usize()] -= 1;
    //                 false
    //             } else {
    //                 null_deps[lhs.usize()] -= 1;
    //                 false
    //             }
    //         });
    //         // check if fixpoint is reached?
    //     }

    //     let mut null = null_rules.into_iter().zip(null_num).enumerate().collect::<Vec<_>>();
    //     null.sort_by(|&(left, _), &(right, _)| {
    //         // Those that are used by other symbols come first.
    //         null_order[left].cmp(&null_order[right])
    //     });

    //     let mut rules = vec![];
    //     let mut roots = vec![];
    //     for (i, (blocks, num)) in null {
    //         let lhs_sym = Symbol::from(i);
    //         let external_lhs = self.ir.externalize(lhs_sym);
    //         if !blocks.is_empty() {
    //             let ident = self.lowercase_name(external_lhs);
    //             rules.push(GenEpsilonIntermediateRule {
    //                 name: ident,
    //                 blocks: blocks
    //             });
    //         }
    //         if num != 0 {
    //             // This nulling forest is not empty.
    //             let ident = self.lowercase_name(external_lhs);
    //             roots.push(GenEpsilonRootAction {
    //                 // This symbol must be internal
    //                 sym: lhs_sym,
    //                 num: num,
    //                 name: ident,
    //                 variant_name: self.variant_names[&external_lhs],
    //             });
    //         }
    //     }
    //     GenEpsilonActions {
    //         rules: rules,
    //         roots: roots,
    //         continuation_label: continuation_label,
    //     }
    // }

    // fn lowercase_name(&self, sym: Symbol) -> rs::Ident {
    //     let rs_name = self.ir.name_of_external(sym).unwrap();
    //     let mut name = rs_name.as_str().to_string();
    //     write!(name, "_{}", rs_name.0).unwrap();
    //     rs::gensym(&name[..])
    // }
}
