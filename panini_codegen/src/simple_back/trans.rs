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

use cfg::symbol::Symbol;
use cfg::ContextFreeRef;
use cfg::remap::Mapping;
use cfg::rule::GrammarRule;
use cfg::rule::container::RuleContainer;
use cfg::symbol::SymbolBitSet;
use gearley::grammar::InternalGrammar;

use rs;
use middle::{Ir, Ty, AutoTy};
use middle::action::{Action, ActionExpr};
use middle::ir::InvocationOfInnerLayer;
use back::generate::{
    GenParser,
    GenEpsilonActions,
    GenEpsilonIntermediateRule,
    GenEpsilonRootAction,
    GenRule,
    GenArg,
    GenSequence,
    GenType,
    GenArgumentsFromOuterLayer,
    GenInvocationOfInnerLayer,
    UniqueNames,
};

const CHAR_CLASSIFIER_MACRO_NAME: &'static str = "char_classifier";

pub struct IrTranslator {
    // Intermediate representation (higher-level).
    pub ir: Ir,

    // temporary maps
    pub type_map: BTreeMap<Symbol, GenType>,
    type_with_inference: HashSet<Symbol>,
    pub variant_names: BTreeMap<Symbol, rs::Term>,

    // Compile-time assertions.
    assert_type_equality: Vec<(rs::TokenStream, rs::TokenStream)>,
    // Terminal symbols.
    pub terminals: Vec<Symbol>,
    // Evaluated node variants with their inner types.
    pub variant_map: Vec<(rs::Term, rs::TokenStream)>,
    // Automatic item definitions for AST representation.
    pub item_definitions: Vec<rs::TokenStream>,
    // Types omitted by user are inferred.
    pub infer: Vec<rs::Term>,
    // Names of generated items
    unique_names: UniqueNames,
}

struct ComputedType {
    // Generated type.
    ty: GenType,
    // Automatic item definitions for AST representation needed by the current
    // type.
    item_def: Vec<rs::P<rs::Item>>,
    // Whether this type contains omitted types that will be inferred.
    has_inference: bool,
}

// Creation

impl IrTranslator {
    pub fn new(ir: Ir) -> Self {
        let layer_id = ir.arguments_from_outer_layer.as_ref()
                                                    .map_or(0, |layer| layer.current_level());

        let mut this = IrTranslator {
            ir: ir,
            type_map: BTreeMap::new(),
            variant_names: BTreeMap::new(),
            variant_map: vec![],
            item_definitions: vec![],
            type_with_inference: HashSet::new(),
            assert_type_equality: vec![],
            infer: vec![],
            terminals: vec![],
            unique_names: UniqueNames::new(layer_id),
        };
        this.compute_variant_map();
        this
    }

    fn compute_variant_map(&mut self) {
        self.compute_terminals();
        self.compute_variant_names();
        self.compute_types();
        self.compute_type_equality_assertions();

        assert_eq!(self.variant_names.len(), self.type_map.len());

        let builder = self.builder;

        self.variant_map.extend(
            self.variant_names.iter().zip(self.type_map.iter()).map(|((sym1, name), (sym2, ty))| {
                assert_eq!(sym1.as_str(), sym2.as_str());
                (*name, ty.generate(builder))
            })
        );
    }

    fn compute_terminals(&mut self) {
        for terminal in SymbolBitSet::terminal_set(&*self.ir.grammar).iter() {
            // Use external symbols, but translate later.
            let terminal = self.ir.externalize(terminal);
            self.terminals.push(terminal);
            let ty = quote! { I::T };
            self.ir.type_map.insert(terminal, Ty::RustTy(ty));
            self.type_map.insert(terminal, GenType::Terminal);
            self.type_with_inference.insert(terminal);
        }
    }

    fn compute_types(&mut self) {
        let mut work: Vec<_> = self.ir.type_map.iter().map(|(k, v)| {
            (k.clone(), v.clone())
        }).collect();
        // Work is sorted.
        while !work.is_empty() {
            let work_len = work.len();
            work.retain(|&(nonterminal, ref ty)| {
                if self.type_map.contains_key(&nonterminal) {
                    // This is a terminal, since terminals were already computed.
                    return false;
                }
                if let Some(gen_type) = self.try_compute_type(nonterminal, ty) {
                    self.insert_type(nonterminal, gen_type);
                    false
                } else {
                    true
                }
            });
            if work.len() == work_len {
                panic!("cycle detected");
            }
        }
    }

    fn compute_type_equality_assertions(&mut self) {
        let assert_type_equality = mem::replace(&mut self.ir.assert_type_equality, vec![]);
        let assert_type_equality = assert_type_equality.into_iter().filter_map(|(sym, ty)| {
            if ty == Ty::Infer {
                None
            } else {
                let computed = self.try_compute_type(sym, &ty).expect("type not computed yet");
                Some((
                    self.type_map[&sym].generate(self.builder),
                    computed.ty.generate(self.builder)
                ))
            }
        }).collect::<Vec<_>>();
        self.assert_type_equality.extend(assert_type_equality);
    }

    fn type_dependencies<F>(&self, ty: &Ty<Symbol>, f: F) -> bool
        where F: Fn(Symbol) -> bool
    {
        match ty {
            &Ty::Auto(AutoTy::Tuple { ref fields }) => {
                fields.iter().all(|&sym| f(sym))
            }
            &Ty::Auto(AutoTy::Struct { ref members }) => {
                members.iter().all(|(_, &sym)| f(sym))
            }
            &Ty::SequenceVec(rhs_sym) => {
                f(rhs_sym)
            }
            &Ty::Infer | &Ty::RustTy(_) => {
                true
            }
        }
    }

    fn try_compute_type(&mut self, nonterminal: Symbol, ty: &Ty<Symbol>) -> Option<ComputedType> {
        if self.type_dependencies(ty, |sym| self.type_map.contains_key(&sym)) {
            Some(self.compute_type(nonterminal, ty))
        } else {
            None
        }
    }

    fn compute_type(&mut self, nonterminal: Symbol, ty: &Ty<Symbol>) -> ComputedType {
        // Do not mutate state here, unless `ty` is Infer.
        match ty {
            &Ty::Auto(AutoTy::Tuple { ref fields }) => {
                let ty = match fields.len() {
                    0 => {
                        GenType::Unit
                    }
                    1 => {
                        self.type_map[&fields[0]].clone()
                    }
                    _ => {
                        GenType::Tuple(
                            fields.iter().map(|sym| self.type_map[sym].clone()).collect()
                        )
                    }
                };
                ComputedType {
                    ty: ty,
                    item_def: vec![],
                    has_inference: fields.iter().any(|sym| self.type_with_inference.contains(sym)),
                }
            }
            &Ty::Auto(AutoTy::Struct { ref members }) => {
                let capitalized_name = self.variant_names[&nonterminal];
                let has_inference = members.iter().any(|(_, sym)|
                    self.type_with_inference.contains(sym)
                );
                let (ty, generics) = if has_inference {
                    let infer = self.unique_names.Infer;
                    (
                        GenType::Item(capitalized_name),
                        quote! { I: #infer }
                    )
                } else {
                    (
                        GenType::Identifier(capitalized_name),
                        quote! {}
                    )
                };
                let ty = ty.generate();
                let member_defs = members.iter().map(|(&name, &sym)| {
                    let ty = self.type_map[&sym].generate();
                    quote! { #name: #ty }
                });
                let member_clones = members.iter().map(|(&name, _)| {
                    quote! { #name: #name.clone() }
                });
                let item_defs = quote! {
                    struct #capitalized_name<#generics> {
                        #(#member_defs)*
                    }

                    impl<#generics> Clone for #capitalized_name {
                        fn clone(&self) -> #ty {
                            #capitalized_name {
                                #(#member_clones)*
                            }
                        }
                    }
                };
                ComputedType {
                    ty: ty,
                    item_defs: item_defs,
                    has_inference: has_inference,
                }
            }
            &Ty::RustTy(ref rust_ty) => {
                ComputedType {
                    ty: GenType::RustTy(rust_ty.clone()),
                    item_def: vec![],
                    has_inference: false,
                }
            }
            &Ty::SequenceVec(rhs_sym) => {
                ComputedType {
                    ty: GenType::Vec(Box::new(self.type_map[&rhs_sym].clone())),
                    item_def: vec![],
                    has_inference: self.type_with_inference.contains(&rhs_sym),
                }
            }
            &Ty::Infer => {
                let identifier = format!("V{}", self.infer.len());
                self.infer.push(self.builder.id(&identifier));
                ComputedType {
                    ty: GenType::Infer(identifier.to_ident()),
                    item_def: vec![],
                    has_inference: true,
                }
            }
        }
    }

    fn insert_type(&mut self, nonterminal: Symbol, comp_type: ComputedType) {
        self.type_map.insert(nonterminal, comp_type.ty);
        self.item_definitions.extend(comp_type.item_def);
        if comp_type.has_inference {
            self.type_with_inference.insert(nonterminal);
        }
    }

    fn compute_variant_names(&mut self) {
        let ir = &self.ir;
        let terminals = &self.terminals;
        self.variant_names = ir.type_map.iter().map(|(&external_sym, _ty)| {
            let name = ir.name_of_external(external_sym).unwrap();
            let opt_id = if terminals.iter().find(|&&sym| sym == external_sym).is_some() {
                None
            } else {
                Some(name.0)
            };
            let capitalized = self.capitalized_name(&name.as_str()[..], opt_id);
            (external_sym, rs::Ident::from_str(&capitalized[..]))
        }).collect();
    }

    fn capitalized_name(&self, name: &str, opt_id: Option<u32>) -> String {
        let mut capitalized: String = name.split('_').flat_map(|word| {
            let mut chars = word.chars();
            let letter = chars.next();
            letter.into_iter().flat_map(|ch| ch.to_uppercase()).chain(chars)
        }).collect();
        // Optionally add the unique id.
        if let Some(id) = opt_id {
            write!(capitalized, "_{}", id).unwrap();
        }
        capitalized
    }
}

// Generation, after the following things are computed:
// * variant map, with variant names
// * type equality assertions

impl IrTranslator {
    pub fn generate(&mut self) -> GenParser {
        let epsilon_actions = self.generate_epsilon_actions();

        let internal_grammar = InternalGrammar::from_processed_grammar_with_maps(
            self.ir.grammar.clone(),
            Mapping::new(0),
            self.ir.nulling_grammar.clone(),
        );

        let mut processed_rule = vec![];
        let mut processed_sequences = vec![];

        let mut seen_origin = HashSet::new();
        // For generating actions.
        let external_origins = self.ir.grammar.rules().filter_map(|rule| {
            if let Some(origin) = rule.history().origin() {
                if seen_origin.insert(origin) {
                    Some(origin)
                } else {
                    None
                }
            } else {
                // Skip this rule.
                None
            }
        });

        for origin in external_origins {
            // Get the basic rule.
            let basic_rule = &self.ir.basic_rules[origin as usize];
            // The basic rule's lhs is often equal to the processed rule's rhs.
            // They are not equal for precedenced rules, due to their rewrite.
            // We should use the basic rule's lhs. (It is already external.)
            let rule_lhs = basic_rule.lhs.node;
            let variant = self.variant_names[&rule_lhs];

            // Diverge on sequence rules
            if let Some((rust_expr, patterns)) = self.get_action(origin as usize) {
                processed_rule.push(GenRule {
                    id: origin as u32,
                    variant: variant,
                    action: rust_expr,
                    args: patterns
                });
            } else {
                let elem_variant = self.variant_names[&basic_rule.rhs[0].node];
                processed_sequences.push(GenSequence {
                    id: origin as u32,
                    variant: variant,
                    elem_variant: elem_variant,
                });
            }
        }

        let terminal_names = self.terminals.iter().map(|&terminal| {
            self.ir.name_of_external(terminal).unwrap().to_ident()
        }).collect();
        let terminal_ids = self.terminals.iter().map(|&terminal| {
            self.ir.internalize(terminal).unwrap().usize()
        }).collect();

        let start = self.ir.grammar.get_start();
        let external_start = self.ir.externalize(start);

        let start_variant = self.variant_names[&external_start];
        let start_type = self.type_map[&external_start].clone();

        let grammar_parts = internal_grammar.to_parts();

        let outer_layer = self.ir.arguments_from_outer_layer.as_ref();
        let inner_layer_level = outer_layer.map_or(0, |layer| layer.current_level() as u32) + 1;

        let arguments_from_outer_layer_opt = self.ir.arguments_from_outer_layer.as_ref().map(|arg| {
            let terminal_names = arg.terminals().iter().map(|&sym| {
                self.ir.name_of_external(sym).unwrap().to_ident()
            }).collect();
            let terminal_variants = arg.terminals().iter().map(|&sym| {
                self.variant_names[&sym].as_str()
            }).collect::<Vec<_>>();
            let terminal_bare_variants = terminal_variants.iter().map(|name| {
                (&name[.. name.rfind("_").unwrap()]).to_ident()
            }).collect();
            GenArgumentsFromOuterLayer {
                terminal_names: terminal_names,
                terminal_variants: terminal_variants,
                terminal_bare_variants: terminal_bare_variants,
            }
        });

        let inner_layer = match &self.ir.invocation_of_inner_layer {
            &InvocationOfInnerLayer::Invoke { ref lexer_invocation, ref embedded_strings } => {
                Some(GenInvocationOfInnerLayer {
                    lexer_name: lexer_invocation.name().to_ident(),
                    lexer_tts: lexer_invocation.tts(),
                    str_lhs: embedded_strings.iter().map(|embed| {
                        self.ir.name_of_external(embed.symbol.node).unwrap().to_ident()
                    }).collect(),
                    str_rhs: embedded_strings.iter().map(|embed| {
                        embed.string.node
                    }).collect(),
                    char_range_lhs: vec![],
                    char_ranges: vec![],
                })
            }
            &InvocationOfInnerLayer::CharClassifier(ref char_ranges) => {
                Some(GenInvocationOfInnerLayer {
                    lexer_name: CHAR_CLASSIFIER_MACRO_NAME.to_ident(),
                    lexer_tts: vec![],
                    str_lhs: vec![],
                    str_rhs: vec![],
                    char_range_lhs: char_ranges.iter().map(|&(_, sym)| {
                        self.ir.name_of_external(sym).unwrap().to_ident()
                    }).collect(),
                    char_ranges: char_ranges.iter().map(|&(range, _)| {
                        range
                    }).collect(),
                })
            }
            &InvocationOfInnerLayer::None => {
                None
            }
        };
        // Names of all internal symbols
        let sym_names = (0 .. grammar_parts.num_syms).map(|sym_id| {
            let internal_sym = Symbol::from(sym_id);
            let external_sym = self.ir.externalize(internal_sym);
            let sym_name = self.ir.name_of_external(external_sym);
            if let Some(sym_name) = sym_name {
                sym_name.as_str().to_string()
            } else {
                format!("g{}", sym_id)
            }
        }).collect();
        // Per-rule IDs
        let trace_rule_ids = self.ir.trace_sources.iter().map(|source| {
            source.rule_id
        }).collect();
        // Positions
        let trace_rule_pos = self.ir.trace_sources.iter().map(|source| {
            source.rule_pos.clone()
        }).collect();
        // Tokens
        let trace_tokens = self.ir.trace_tokens.clone();
        // Construct result
        GenParser {
            grammar_parts: grammar_parts,

            start_variant: start_variant,
            start_type: start_type,

            epsilon_actions: epsilon_actions,
            rules: processed_rule,
            sequences: processed_sequences,

            variant_map: self.variant_map.clone(),

            terminal_names: terminal_names,
            terminal_ids: terminal_ids,

            arguments_from_outer_layer: arguments_from_outer_layer_opt,

            inner_layer_level: inner_layer_level,
            inner_layer: inner_layer,

            trace_rule_ids: trace_rule_ids,
            trace_rule_pos: trace_rule_pos,
            trace_tokens: trace_tokens,

            sym_names: sym_names,

            item_definitions: self.item_definitions.clone(),

            infer: self.infer.clone(),

            unique_names: self.unique_names,

            builder: self.builder,
        }
    }

    fn get_action(
        &self,
        origin: usize)
        -> Option<(rs::P<rs::Expr>, Vec<GenArg>)>
    {
        let basic_rule = &self.ir.basic_rules[origin];
        let mut patterns = vec![];

        let rust_expr = match &basic_rule.action {
            &Action::Struct { ref deep_binds, ref shallow_binds, ref expr } => {
                if !deep_binds.is_empty() {
                    for &rhs_pos in deep_binds {
                        let rhs_sym = basic_rule.rhs[rhs_pos].node;
                        let variant = self.variant_names[&rhs_sym];
                        let pat = self.get_auto_pattern(rhs_sym).expect("auto pattern not found");
                        patterns.push(GenArg {
                            num: rhs_pos,
                            variant: variant,
                            pat: pat
                        });
                    }
                } else if !shallow_binds.is_empty() {
                    for &(rhs_pos, ident) in shallow_binds {
                        let rhs_sym = basic_rule.rhs[rhs_pos].node;
                        let variant = self.variant_names[&rhs_sym];
                        let pat = self.builder.pat().id(ident);
                        patterns.push(GenArg {
                            num: rhs_pos,
                            variant: variant,
                            pat: pat
                        });
                    }
                }

                match expr {
                    &ActionExpr::Auto => {
                        self.get_auto_expr(basic_rule.lhs.node)
                    }
                    &ActionExpr::Inline { ref expr } => {
                        expr.clone()
                    }
                }
            }
            &Action::Tuple { ref tuple_binds } => {
                let idents: Vec<_>;
                idents = tuple_binds.iter().map(|&pos| {
                    self.builder.id(format!("arg{}", pos))
                }).collect();
                patterns = tuple_binds.iter().zip(idents.iter()).map(|(&pos, &ident)| {
                    let variant = self.variant_names[&basic_rule.rhs[pos].node];
                    let pat = self.builder.pat().id(ident);
                    GenArg {
                        num: pos,
                        variant: variant,
                        pat: pat
                    }
                }).collect();
                match idents.len() {
                    0 => {
                        quote! { () }
                    }
                    1 => {

                        self.builder.expr().id(idents[0])
                    }
                    _ => {
                        quote! { ( #(#idents),* ) }
                    }
                }
            }
            &Action::Sequence => {
                return None;
            }
        };
        Some((rust_expr, patterns))
    }

    fn get_auto_expr(&self, nonterminal: Symbol) -> rs::P<rs::Expr> {
        match &self.ir.type_map[&nonterminal] {
            &Ty::Auto(AutoTy::Struct { ref members }) => {
                self.builder.expr().struct_id(self.variant_names[&nonterminal]).with_id_exprs(
                    members.iter().map(|(id, _)| {
                        (*id, self.builder.expr().id(id))
                    })
                ).build()
            }
            _ => unreachable!()
        }
    }

    fn get_auto_pattern(&self, nonterminal: Symbol) -> Option<rs::P<rs::Pat>> {
        match &self.ir.type_map[&nonterminal] {
            &Ty::Auto(AutoTy::Struct { ref members }) => {
                let name = self.variant_names[&nonterminal];
                let mut builder = self.builder.pat().struct_().id(name).build();
                for (&id, &sym) in members {
                    // Recursion
                    if let Some(pat) = self.get_auto_pattern(sym) {
                        builder = builder.pat(id).build(pat);
                    } else {
                        builder = builder.id(id);
                    }
                }
                Some(builder.build())
            }
            _ => {
                None
            }
        }
    }

    fn generate_epsilon_actions(&mut self) -> GenEpsilonActions {
        // Nulling rules
        // are these equal?
        let num_nulling_syms = self.ir.grammar.num_syms();
        let num_all_nulling_syms = self.ir.nulling_grammar.sym_source().num_syms();
        // Declarations
        let mut null_rules = iter::repeat(vec![]).take(num_nulling_syms).collect::<Vec<_>>();
        let mut null_num = iter::repeat(0).take(num_nulling_syms).collect::<Vec<_>>();
        let mut null_order = iter::repeat(u32::MAX).take(num_nulling_syms).collect::<Vec<_>>();
        // These vectors may be longer than other vectors.
        let mut null_deps = iter::repeat(0).take(num_all_nulling_syms).collect::<Vec<_>>();
        let mut null_intermediate = iter::repeat(None)
                                    .take(num_all_nulling_syms)
                                    .collect::<Vec<_>>();
        // Temporary variables.
        let mut null_work = vec![];
        let mut null_num_rules = 0;
        // Here, the name must start with "_" so that we don't get "unnecessary mut"
        // warnings later on. Yes, underscore prefix works for ignoring more than just
        // "unused variable" warnings.
        let continuation_label = rs::Symbol::gensym("_cont");
        for rule in self.ir.nulling_grammar.rules() {
            if rule.rhs().len() == 0 {
                // Can `origin` be None? In sequences? No.
                let origin = rule.history().origin().unwrap() as usize;
                let basic_rule = self.ir.basic_rules.get(origin);
                let action = basic_rule.map(|basic_rule| &basic_rule.action);
                let action_expr = match action {
                    Some(&Action::Tuple { .. }) => {
                        unreachable!("found nulling rule that has a tuple type")
                    }
                    Some(&Action::Struct { expr: ActionExpr::Inline { ref expr }, .. }) => {
                        expr.clone()
                    }
                    // A sequence rule.
                    Some(&Action::Sequence) => {
                        self.builder.expr().call().path().ids(&["Vec", "new"]).build().build()
                    }
                    _ => unreachable!("found unknown action")
                };
                let inner = self.builder.block()
                        .stmt().expr().call().id(continuation_label).with_arg(action_expr)
                        .build()
                    .build();
                null_rules[rule.lhs().usize()].push(inner);
                null_num[rule.lhs().usize()] += 1;
                if null_order[rule.lhs().usize()] > null_num_rules {
                    null_order[rule.lhs().usize()] = null_num_rules;
                    null_num_rules += 1;
                }
            } else {
                if rule.history().origin().is_none() {
                    null_intermediate[rule.lhs().usize()] = Some(rule.rhs().to_owned());
                }
                null_work.push((
                    rule.lhs(),
                    rule.rhs()[0],
                    rule.rhs().get(1).cloned(),
                    rule.history.origin()
                ));
                null_deps[rule.lhs().usize()] += 1;
            }
        }
        // Generate code that uses macros and a continuation-passing style.
        while !null_work.is_empty() {
            null_work.retain(|&(lhs, rhs0, rhs1, action)| {
                let rhs1_is_done = rhs1.map_or(true, |rhs1| null_deps[rhs1.usize()] == 0);
                let rhs0_is_done = null_deps[rhs0.usize()] == 0;
                if !rhs0_is_done || !rhs1_is_done {
                    // Process this later.
                    true
                } else if let Some(origin) = action {
                    // There are no sequence rules among nulling rules, so unwrapping is ok.
                    let (action_expr, patterns) = self.get_action(origin as usize).unwrap();
                    let mut pats = HashMap::new();
                    for arg in patterns.into_iter() {
                        pats.insert(arg.num, arg.pat);
                    }
                    let mut factors = vec![];
                    let mut factor_stack = vec![];
                    if let Some(rhs1) = rhs1 {
                        factor_stack.push(rhs1);
                    }
                    factor_stack.push(rhs0);
                    while let Some(sym) = factor_stack.pop() {
                        if let &Some(ref rhs) = &null_intermediate[sym.usize()] {
                            factor_stack.extend(rhs.iter().cloned());
                        } else {
                            factors.push(sym);
                        }
                    }
                    let mut inner_layer = quote! { #continuation_label(#action_expr) };
                    for (i, &factor) in factors.iter().enumerate().rev() {
                        let name = self.lowercase_name(self.ir.externalize(factor));
                        let pat = pats.get(&i).cloned().unwrap_or_else(|| quote! { _ });
                        inner_layer = quote! {
                            #name!(
                                |#pat| {
                                    #inner_layer
                                }
                            )
                        };
                    }
                    null_rules[lhs.usize()].push(inner_layer);
                    null_num[lhs.usize()] += factors.iter().fold(1, |acc, &factor| {
                        acc * null_num[factor.usize()]
                    });
                    // what is this order for?
                    if null_order[lhs.usize()] > null_num_rules {
                        null_order[lhs.usize()] = null_num_rules;
                        null_num_rules += 1;
                    }
                    null_deps[lhs.usize()] -= 1;
                    false
                } else {
                    null_deps[lhs.usize()] -= 1;
                    false
                }
            });
            // check if fixpoint is reached?
        }

        let mut null = null_rules.into_iter().zip(null_num).enumerate().collect::<Vec<_>>();
        null.sort_by(|&(left, _), &(right, _)| {
            // Those that are used by other symbols come first.
            null_order[left].cmp(&null_order[right])
        });

        let mut rules = vec![];
        let mut roots = vec![];
        for (i, (blocks, num)) in null {
            let lhs_sym = Symbol::from(i);
            let external_lhs = self.ir.externalize(lhs_sym);
            if !blocks.is_empty() {
                let ident = self.lowercase_name(external_lhs);
                rules.push(GenEpsilonIntermediateRule {
                    name: ident,
                    blocks: blocks
                });
            }
            if num != 0 {
                // This nulling forest is not empty.
                let ident = self.lowercase_name(external_lhs);
                roots.push(GenEpsilonRootAction {
                    // This symbol must be internal
                    sym: lhs_sym,
                    num: num,
                    name: ident,
                    variant_name: self.variant_names[&external_lhs],
                });
            }
        }
        GenEpsilonActions {
            rules: rules,
            roots: roots,
            continuation_label: continuation_label,
        }
    }

    fn lowercase_name(&self, sym: Symbol) -> rs::ast::Ident {
        let rs_name = self.ir.name_of_external(sym).unwrap();
        let mut name = rs_name.as_str().to_string();
        write!(name, "_{}", rs_name.0).unwrap();
        rs::Symbol::gensym(&name[..])
    }
}
