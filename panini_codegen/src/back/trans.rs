#![allow(non_snake_case)]

use std::iter;
use std::collections::HashSet;
use std::collections::HashMap;
use std::collections::BTreeMap;
use std::fmt::Write;
use std::u32;
use std::mem;

use aster;
use aster::AstBuilder;
use aster::ident::ToIdent;

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
use back::generate::{
    GenParser,
    GenNulling,
    GenNullingRule,
    GenNullingRoot,
    GenRule,
    GenArg,
    GenSequence,
    UniqueNames
};

pub struct IrTranslator {
    // Intermediate representation (higher-level).
    pub ir: Ir,
    // A builder for Rust code.
    pub builder: AstBuilder,

    // temporary maps
    pub type_map: BTreeMap<Symbol, GenType>,
    type_with_inference: HashSet<Symbol>,
    pub variant_names: BTreeMap<Symbol, rs::ast::Ident>,

    // Compile-time assertions.
    assert_type_equality: Vec<(rs::P<rs::Ty>, rs::P<rs::Ty>)>,
    // Terminal symbols.
    pub terminals: Vec<Symbol>,
    // Evaluated node variants with their inner types.
    pub variant_map: Vec<(rs::ast::Ident, rs::P<rs::Ty>)>,
    // Automatic item definitions for AST representation.
    pub item_definitions: Vec<rs::P<rs::Item>>,
    // Types omitted by user are inferred.
    pub infer: Vec<rs::ast::Ident>,
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

#[derive(Clone)]
pub enum GenType {
    RustTy(rs::P<rs::Ty>),
    Vec(Box<GenType>),
    Unit,
    Tuple(Vec<GenType>),
    Identifier(rs::ast::Ident),
    Item(rs::ast::Ident),
    Infer(rs::ast::Ident),
    Terminal,
}

impl GenType {
    pub fn generate(&self, builder: AstBuilder) -> rs::P<rs::Ty> {
        match self {
            &GenType::RustTy(ref ty) => ty.clone(),
            &GenType::Unit => builder.ty().unit(),
            &GenType::Identifier(name) => builder.ty().id(name),
            &GenType::Tuple(ref fields) => {
                let mut ty_build = builder.ty().tuple();
                for field in fields {
                    ty_build = ty_build.with_ty(field.generate(builder));
                }
                ty_build.build()
            }
            &GenType::Item(name) => {
                builder.ty().path().segment(name).with_generics(
                    builder.generics().ty_param("I").build().build()
                ).build().build()
            }
            &GenType::Vec(ref elem_ty) => {
                builder.ty()
                    .path()
                        .segment("Vec").with_ty(elem_ty.generate(builder)).build()
                    .build()
            }
            &GenType::Infer(name) => {
                builder.ty().path().id("I").id(&name).build()
            }
            &GenType::Terminal => {
                builder.ty().path().id("I").id("T").build()
            }
        }
    }

    pub fn generate_qualified(&self, builder: AstBuilder, infer_trait: rs::ast::Ident)
        -> rs::P<rs::Ty>
    {
        match self {
            &GenType::Tuple(ref fields) => {
                let mut ty_build = builder.ty().tuple();
                for field in fields {
                    ty_build = ty_build.with_ty(field.generate_qualified(builder, infer_trait));
                }
                ty_build.build()
            }
            &GenType::Item(name) => {
                builder.ty().
                    path().segment(name).ty().path().id("I").id("Infer").build().build()
                .build()
            }
            &GenType::Vec(ref elem_ty) => {
                builder.ty()
                    .path()
                        .segment("Vec")
                            .with_ty(elem_ty.generate_qualified(builder, infer_trait))
                        .build()
                    .build()
            }
            &GenType::Infer(name) => {
                builder.ty()
                    .qpath().ty().path().id("I").id("Infer").build().as_().id(infer_trait).build()
                    .id(name)
            }
            &GenType::Terminal => {
                builder.ty()
                    .qpath().ty().path().id("I").id("Infer").build().as_().id(infer_trait).build()
                    .id("T")
            }
            _ => self.generate(builder)
        }
    }
}

impl IrTranslator {
    pub fn new(ir: Ir) -> Self {
        let uniq_id = ir.lexer_for_upper.as_ref().map_or(0, |lexer| lexer.level());

        let mut this = IrTranslator {
            ir: ir,
            type_map: BTreeMap::new(),
            variant_names: BTreeMap::new(),
            variant_map: vec![],
            item_definitions: vec![],
            type_with_inference: HashSet::new(),
            assert_type_equality: vec![],
            infer: vec![],
            builder: AstBuilder::new(),
            terminals: vec![],
            unique_names: UniqueNames::new(uniq_id),
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
                assert_eq!(sym1, sym2);
                (*name, ty.generate(builder))
            })
        );
    }

    fn compute_terminals(&mut self) {
        for terminal in SymbolBitSet::terminal_set(&*self.ir.grammar).iter() {
            // Use external symbols, but translate later.
            let terminal = self.ir.to_external(terminal);
            self.terminals.push(terminal);
            let ty = self.builder.ty().path().id("I").id("T").build();
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
                    (GenType::Item(capitalized_name),
                    self.builder.generics()
                            .ty_param("I").build()
                            .predicate().bound().id("I")
                                .trait_(self.unique_names.Infer).build()
                            .build()
                        .build())
                } else {
                    (GenType::Identifier(capitalized_name),
                    self.builder.generics().build())
                };
                let clone_item_impl = self.builder.item().impl_()
                    .trait_().id("Clone").build()
                    .with_generics(generics.clone())
                    .method("clone")
                        .fn_decl()
                            .self_().ref_()
                            .build_return(ty.generate(self.builder))
                    .block().expr().struct_id(capitalized_name).with_id_exprs(
                        members.iter().map(|(&name, _sym)| {
                            (name,
                             self.builder.expr()
                                .method_call("clone")
                                .field(name)
                                .self_()
                             .build())
                        })
                    ).build()
                    .build_ty(ty.generate(self.builder));
                let item_def = self.builder.item().struct_(capitalized_name)
                    .with_generics(generics)
                    .with_fields(
                        members.iter().map(|(&name, &sym)| {
                            self.builder
                                .struct_field(name)
                                .build_ty(self.type_map[&sym].generate(self.builder))
                        })
                    ).build();
                ComputedType {
                    ty: ty,
                    item_def: vec![item_def, clone_item_impl],
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
            (external_sym, rs::str_to_ident(&capitalized[..]))
        }).collect();
    }

    fn lowercase_name(&self, sym: Symbol) -> rs::ast::Ident {
        let rs_name = self.ir.name_of_external(sym).unwrap();
        let mut name = rs_name.as_str().to_string();
        write!(name, "_{}", rs_name.0).unwrap();
        rs::str_to_ident(&name[..])
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

    fn get_action(
        &self,
        origin: usize)
        -> (rs::P<rs::Expr>, Vec<GenArg>)
    {
        let (rule_lhs, ref rule_rhs) = self.ir.external_grammar[origin];
        let mut patterns = vec![];

        let rust_expr = match &self.ir.actions[origin] {
            &Action::Struct { ref deep_binds, ref shallow_binds, ref expr } => {
                if !deep_binds.is_empty() {
                    for &rhs_pos in deep_binds {
                        let rhs_sym = rule_rhs[rhs_pos];
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
                        let rhs_sym = rule_rhs[rhs_pos];
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
                        self.get_auto_expr(rule_lhs)
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
                    let variant = self.variant_names[&rule_rhs[pos]];
                    let pat = self.builder.pat().id(ident);
                    GenArg {
                        num: pos,
                        variant: variant,
                        pat: pat
                    }
                }).collect();
                match idents.len() {
                    0 => {
                        self.builder.expr().unit()
                    }
                    1 => {
                        self.builder.expr().id(idents[0])
                    }
                    _ => {
                        self.builder.expr().tuple().with_exprs(
                            idents.iter().map(|&ident| self.builder.expr().id(ident))
                        ).build()
                    }
                }
            }
        };
        (rust_expr, patterns)
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

    pub fn generate(&mut self) -> GenParser {
        let null = self.generate_nulling();

        let internal_grammar = InternalGrammar::from_processed_grammar_with_maps(
            self.ir.grammar.clone(),
            Mapping::new(0),
            self.ir.nulling_grammar.clone(),
        );

        let mut processed_rule = vec![];

        let mut seen_origin = HashSet::new();
        let rules_with_external_origin = self.ir.grammar.rules().filter_map(|rule| {
            if let Some(origin) = rule.history().origin() {
                if (origin as usize) < self.ir.actions.len() && seen_origin.insert(origin) {
                    Some((rule, origin as u32))
                } else {
                    None
                }
            } else {
                // Skip this rule.
                None
            }
        });

        for (rule, origin) in rules_with_external_origin {
            // Translate to external.
            let rule_lhs = self.ir.to_external(rule.lhs());
            let variant = self.variant_names[&rule_lhs];

            let (rust_expr, patterns) = self.get_action(origin as usize);
            processed_rule.push(GenRule {
                id: origin,
                variant: variant,
                action: rust_expr,
                args: patterns
            });
        }

        let mut processed_sequences = vec![];
        for sequence in &self.ir.sequences {
            processed_sequences.push(GenSequence {
                elem_variant: self.variant_names[&sequence.rhs.node],
                variant: self.variant_names[&sequence.lhs.node],
            });
        }

        GenParser {
            trans: self,
            grammar_parts: internal_grammar.to_parts(),
            null: null,
            rules: processed_rule,
            sequences: processed_sequences,
            unique_names: self.unique_names,
        }
    }

    fn generate_nulling(&mut self) -> GenNulling {
        // Nulling rules
        let num_nulling_syms = self.ir.name_map().names().len();
        let num_all_nulling_syms = self.ir.nulling_grammar.sym_source().num_syms();
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
        let continuation_label = rs::gensym_ident("_cont");
        for rule in self.ir.nulling_grammar.rules() {
            if rule.rhs().len() == 0 {
                // Can `origin` be None? In sequences? No.
                let origin = rule.history().origin().unwrap() as usize;
                let action_expr = match self.ir.actions.get(origin) {
                    Some(&Action::Tuple { .. }) => {
                        unreachable!("found nulling rule that has a tuple type")
                    }
                    Some(&Action::Struct { expr: ActionExpr::Inline { ref expr }, .. }) => {
                        expr.clone()
                    }
                    // A sequence rule.
                    _ => {
                        self.builder.expr().call().path().ids(&["Vec", "new"]).build().build()
                    }
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
                    rule.lhs(), rule.rhs()[0],
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
                    let (action_expr, patterns) = self.get_action(origin as usize);
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
                    let mut inner_layer = self.builder.block().stmt().expr()
                        .call().id(continuation_label).with_arg(action_expr).build().build();
                    for (i, &factor) in factors.iter().enumerate().rev() {
                        let name = self.lowercase_name(factor);
                        let pat = pats.get(&i).cloned()
                                              .unwrap_or_else(|| self.builder.pat().wild());
                        let fn_decl = self.builder.fn_decl()
                                .arg().with_pat(pat).ty().infer()
                                .default_return();
                        let closure = rs::ast::ExprKind::Closure(
                            rs::ast::CaptureBy::Ref, // or Value
                            fn_decl,
                            inner_layer,
                            rs::DUMMY_SP,
                        );
                        let closure = self.builder.expr().build_expr_kind(closure);
                        inner_layer =
                            self.builder.block().stmt().build_expr(
                                    MacExprBuilder::build()
                                        .path(self.builder.path().id(name).build())
                                        .expr().build(closure).build()
                                ).build();
                    }
                    null_rules[lhs.usize()].push(inner_layer);
                    null_num[lhs.usize()] += factors.iter().fold(1, |acc, &factor| {
                        acc * null_num[factor.usize()]
                    });
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
            })
        }

        let mut null = null_rules.into_iter().zip(null_num).enumerate().collect::<Vec<_>>();
        null.sort_by(|&(left, _), &(right, _)| {
            // Those that are used by other symbols come first.
            null_order[left].cmp(&null_order[right])
        });

        let mut rules = vec![];
        let mut roots = vec![];
        for (i, (blocks, num)) in null {
            let ident = self.lowercase_name(Symbol::from(i));
            if !blocks.is_empty() {
                rules.push(GenNullingRule {
                    name: ident,
                    blocks: blocks
                });
            }
            if num != 0 {
                let lhs_sym = Symbol::from(i);
                roots.push(GenNullingRoot {
                    sym: lhs_sym,
                    num: num,
                    name: ident
                });
            }
        }
        GenNulling {
            rules: rules,
            roots: roots,
            continuation_label: continuation_label,
        }
    }
}

struct MacExprBuilder;

impl MacExprBuilder {
    fn build() -> aster::mac::MacBuilder<Self> {
        aster::mac::MacBuilder::with_callback(MacExprBuilder)
    }
}

impl aster::invoke::Invoke<rs::ast::Mac> for MacExprBuilder {
    type Result = rs::P<rs::Expr>;
    fn invoke(self, arg: rs::ast::Mac) -> rs::P<rs::Expr> {
        AstBuilder::new().expr().build_expr_kind(rs::ast::ExprKind::Mac(arg))
    }
}
