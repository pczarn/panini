use std::collections::HashMap;
use std::cmp::Ordering;
use std::mem;
use std::iter;

use bit_matrix::BitMatrix;
use gearley::grammar::{Grammar, BinarizedGrammar};
use cfg::cycles::Cycles;
use cfg::remap::{Remap, Mapping};
use cfg::rule::container::RuleContainer;
use cfg::usefulness::Usefulness;
use cfg::*;
use cfg_regex::{RegexTranslation, ClassRange};

use rs;
use front::ast;
use middle::{Action, ActionExpr, Ty, Lexer, Hir, Folder, FoldHir, AutoTy};
use middle::hir::{self, SequenceRule};
use middle::error::TransformationError;
use middle::trace::{Trace, SourceOrigin};
use middle::attr::{Attrs, LexerForUpper};
use middle::warn::WarningCauses;

pub struct Ir {
    pub grammar: BinarizedGrammar,
    pub nulling_grammar: BinarizedGrammar,
    pub external_grammar: Vec<(Symbol, Vec<Symbol>)>,
    pub sequences: Vec<SequenceRule<Symbol>>,
    pub trivial_derivation: bool,
    pub actions: Vec<Action>,
    pub type_map: HashMap<Symbol, Ty<Symbol>>,
    pub maps: InternalExternalNameMap,
    pub assert_type_equality: Vec<(Symbol, Ty<Symbol>)>,
    pub lexer_for_upper: Option<LexerForUpper<Symbol>>,
    pub lower_level: LowerLevel,
    pub trace_tokens: Vec<Vec<String>>,
    pub trace_sources: Vec<SourceOrigin>,
}

#[derive(Clone)]
pub struct NameMap {
    pub sym_map: HashMap<rs::Name, Symbol>,
    pub sym_vec: Vec<Option<rs::Name>>,
}

pub struct InternalExternalNameMap {
    internal_external: Mapping,
    name_map: NameMap,
}

impl NameMap {
    fn insert_padded(&mut self, sym: Symbol, name: rs::Name) {
        if self.sym_vec.len() <= sym.usize() {
            let pad_len = sym.usize() - self.sym_vec.len() + 1;
            self.sym_vec.extend(iter::repeat(None).take(pad_len));
        }
        self.sym_vec[sym.usize()] = Some(name);
        self.sym_map.insert(name, sym);
    }

    fn to_name(&self, sym: Symbol) -> Option<rs::Name> {
        self.sym_vec.get(sym.usize()).and_then(|opt| *opt)
    }

    pub fn names(&self) -> &[Option<rs::Name>] {
        &self.sym_vec[..]
    }
}

impl InternalExternalNameMap {
    fn new(internal_external: Mapping, name_map: NameMap) -> Self {
        InternalExternalNameMap {
            internal_external: internal_external,
            name_map: name_map,
        }
    }

    fn to_internal(&self, sym: Symbol) -> Option<Symbol> {
        self.internal_external.to_internal[sym.usize()]
    }

    fn to_external(&self, sym: Symbol) -> Symbol {
        self.internal_external.to_external[sym.usize()]
    }

    fn name_of_external(&self, sym: Symbol) -> Option<rs::Name> {
        self.name_map.to_name(sym)
    }

    fn name_map(&self) -> &NameMap {
        &self.name_map
    }
}

#[derive(Eq, PartialEq)]
pub enum LowerLevel {
    Invoke {
        lexer: Lexer,
        embedded_strings: Vec<(rs::Spanned<Symbol>, rs::Spanned<rs::Name>, SourceOrigin)>,
    },
    CharClassifier(Vec<(ClassRange, Symbol)>),
    None
}

struct IrStmtsAndAttrs {
    stmts: ast::Stmts,
    start: rs::Name,
    attrs: Attrs<rs::Name>,
    errors: Vec<TransformationError>,
}

struct IrInitialHir {
    hir_with_names: Hir<rs::Name>,
    start: rs::Name,
    attrs: Attrs<rs::Name>,
    errors: Vec<TransformationError>,
    // Final
    lower_level: LowerLevel,
    trace_tokens: Vec<Vec<String>>,
}

struct IrFinalHir {
    hir: Hir<Symbol>,
    grammar: Grammar,
    name_map: NameMap,
    attrs: Attrs<Symbol>,
    errors: Vec<TransformationError>,
    // Final
    lower_level: LowerLevel,
    trace_tokens: Vec<Vec<String>>,
    hir_rules_with_names: Vec<hir::Rule<rs::Name>>,
}

struct IrPreparedGrammar {
    grammar: Grammar,
    ordering: HashMap<(Symbol, Symbol), Ordering>,
    // Final
    common: IrCommonPrepared,
}

struct IrBinarizedGrammar {
    bin_grammar: BinarizedGrammar,
    ordering: HashMap<(Symbol, Symbol), Ordering>,
    // Final
    common: IrCommonPrepared,
}

struct IrNonNullingBinarized {
    bin_grammar: BinarizedGrammar,
    nulling_grammar: BinarizedGrammar,
    ordering: HashMap<(Symbol, Symbol), Ordering>,
    // Warning causes
    cycles_among_nullable: Vec<u32>,
    // Final
    trivial_derivation: bool,
    common: IrCommonPrepared,
}

struct IrProperNonNullingBinarized {
    bin_grammar: BinarizedGrammar,
    nulling_grammar: BinarizedGrammar,
    ordering: HashMap<(Symbol, Symbol), Ordering>,
    // Warning causes
    cycles: Vec<u32>,
    cycles_among_nullable: Vec<u32>,
    unproductive_rules: Vec<(u32, u32)>,
    unreachable_rules: Vec<u32>,
    // Final
    trivial_derivation: bool,
    common: IrCommonPrepared,
}

pub struct IrMappedGrammar {
    bin_mapped_grammar: BinarizedGrammar,
    nulling_grammar: BinarizedGrammar,
    maps: InternalExternalNameMap,
    // Warning causes
    cycles: Vec<u32>,
    cycles_among_nullable: Vec<u32>,
    unproductive_rules: Vec<(u32, u32)>,
    unreachable_rules: Vec<u32>,
    // Final
    trivial_derivation: bool,
    common: IrCommonPrepared,
}

// Common

struct IrCommonPrepared {
    external_grammar: Vec<(Symbol, Vec<Symbol>)>,
    actions: Vec<Action>,
    attrs: Attrs<Symbol>,
    lower_level: LowerLevel,
    trace_tokens: Vec<Vec<String>>,
    trace_sources: Vec<SourceOrigin>,
    name_map: NameMap,
    hir_rules_with_names: Vec<hir::Rule<rs::Name>>,
    hir: Hir<Symbol>,
    errors: Vec<TransformationError>,
}

impl IrStmtsAndAttrs {
    fn compute(mut stmts: ast::Stmts, attrs: Attrs<rs::Name>) -> Result<Self, TransformationError> {
        // Obtain the explicit start.
        let start = if let Some(first_stmt) = stmts.stmts.get(0) {
            first_stmt.lhs.node
        } else {
            return Err(TransformationError::GrammarIsEmpty);
        };

        if let &Some(ref upper) = attrs.lexer_for_upper() {
            // Set the implicit start.
            let start = rs::gensym("_lower_start_");
            for terminal in upper.terminals() {
                stmts.stmts.push(ast::Stmt {
                    lhs: rs::dummy_spanned(start),
                    rhs: vec![(
                        ast::Rhs(vec![
                            ast::RhsElement {
                                bind: None,
                                elem: ast::RhsAst::Symbol(rs::dummy_spanned(*terminal))
                            }
                        ]),
                        ast::Action { expr: None }
                    )],
                    ty: None,
                    span: rs::DUMMY_SP,
                });
            }
        }

        Ok(IrStmtsAndAttrs {
            stmts: stmts,
            start: start,
            attrs: attrs,
            errors: vec![],
        })
    }
}

impl IrInitialHir {
    fn compute(ir: IrStmtsAndAttrs) -> Result<Self, TransformationError> {
        let mut hir_with_names = Hir::transform_stmts(&ir.stmts);
        let mut errors = ir.errors;

        if !hir_with_names.check_type_equality() {
            errors.push(TransformationError::TypeMismatch);
        }

        let trace = Trace::from_stmts(&ir.stmts);
        let trace_tokens = trace.stmts().iter().map(|stmt| {
            let mut rule_tokens = vec![];
            rule_tokens.push(stmt.lhs.as_str().to_string());
            for token in &stmt.rhs {
                rule_tokens.push(token.as_str().to_string());
            }
            rule_tokens
        }).collect();

        let lower_level = if let Some(lexer) = ir.stmts.lexer {
            LowerLevel::Invoke {
                lexer: lexer,
                embedded_strings: vec![]
            }
        } else {
            LowerLevel::None
        };

        Ok(IrInitialHir {
            hir_with_names: hir_with_names,
            start: ir.start,
            trace_tokens: trace_tokens,
            lower_level: lower_level,
            attrs: ir.attrs,
            errors: errors,
        })
    }
}

impl IrFinalHir {
    fn compute(ir: IrInitialHir) -> Self {
        let hir_rules_with_names = ir.hir_with_names.rules.clone();
        let mut grammar = Grammar::new();
        let (hir, sym_map, sym_vec) = {
            let mut fold = Folder::new(grammar.sym_source_mut());
            let hir = fold.fold_hir(ir.hir_with_names);
            (hir, fold.sym_map, fold.sym_vec)
        };
        let attrs = ir.attrs.map_symbols(|name| sym_map[&name]);
        grammar.set_start(sym_map[&ir.start]);
        IrFinalHir {
            hir: hir,
            grammar: grammar,
            name_map: NameMap {
                sym_map: sym_map,
                sym_vec: sym_vec,
            },
            attrs: attrs,
            errors: ir.errors,
            lower_level: ir.lower_level,
            trace_tokens: ir.trace_tokens,
            hir_rules_with_names: hir_rules_with_names,
        }
    }
}

impl IrPreparedGrammar {
    fn compute(mut ir: IrFinalHir) -> Self {
        let IrFinalHir { mut hir, mut grammar, .. } = ir;
        let embedded_strings = hir.embedded_strings.clone();
        let lower_level = if !hir.embedded_strings.is_empty() {
            match ir.lower_level {
                LowerLevel::Invoke { lexer, .. } => {
                    // Add strings to the lower level.
                    LowerLevel::Invoke {
                        embedded_strings: embedded_strings,
                        lexer: lexer,
                    }
                }
                LowerLevel::None => {
                    if ir.attrs.lexer_for_upper().is_some() {
                        // We are at level 1 .. N and adding strings to the current level.
                        // Embedded strings are rewritten into rules. Character ranges are
                        // collected.
                        let mut regex_rewrite = RegexTranslation::new();
                        for (symbol, string, source_origin) in embedded_strings {
                            let sym2 = regex_rewrite.rewrite_string(
                                &mut grammar,
                                &*string.node.as_str()
                            );
                            hir.rules.push(hir::Rule {
                                lhs: symbol,
                                rhs: vec![rs::dummy_spanned(sym2)],
                                tuple_binds: vec![],
                                deep_binds: vec![],
                                shallow_binds: vec![],
                                source_origin: source_origin,
                                action: ActionExpr::Auto,
                            });
                            hir.type_map.insert(
                                symbol.node,
                                Ty::Auto(AutoTy::Tuple { fields: vec![] })
                            );
                        }
                        let char_ranges = regex_rewrite.get_ranges().clone().into_iter()
                                                                            .collect::<Vec<_>>();
                        for (i, &(_, sym)) in char_ranges.iter().enumerate() {
                            let name = rs::gensym(&*format!("ChRange{}", i));
                            ir.name_map.insert_padded(sym, name);
                        }
                        LowerLevel::CharClassifier(char_ranges)
                    } else {
                        // We are at level 0 and adding strings to a newly created level 1.
                        LowerLevel::Invoke {
                            lexer: Lexer::new(rs::intern("grammar"), vec![]),
                            embedded_strings: embedded_strings,
                        }
                    }
                }
                LowerLevel::CharClassifier(_) => unreachable!()
            }
        } else {
            ir.lower_level
        };

        let mut trace_sources = vec![];
        let mut external_grammar = vec![];
        let mut actions = vec![];
        let mut ordering = HashMap::new();
        for rule in &hir.rules {
            let lhs = rule.lhs.node;
            let rhs: Vec<_> = rule.rhs.iter().map(|s| s.node).collect();
            if rhs.len() == 1 {
                let mut left = lhs;
                let mut ord = Ordering::Greater;
                let mut right = rhs[0];
                if left.usize() > right.usize() {
                    mem::swap(&mut left, &mut right);
                    ord = Ordering::Less;
                }
                ordering.insert((left, right), ord);
            }
            grammar.rule(lhs).rhs(&rhs);
            external_grammar.push((lhs, rhs));
            trace_sources.push(rule.source_origin.clone());
            let action = if rule.action.is_inline() ||
                    !rule.deep_binds.is_empty() ||
                    !rule.shallow_binds.is_empty() {
                Action::Struct {
                    deep_binds: rule.deep_binds.clone(),
                    shallow_binds: rule.shallow_binds.clone(),
                    expr: rule.action.clone(),
                }
            } else {
                Action::Tuple {
                    tuple_binds: rule.tuple_binds.clone(),
                }
            };
            actions.push(action);
        }
        //
        for seq in &hir.sequence_rules {
            grammar.sequence(seq.lhs.node).inclusive(seq.min, seq.max).rhs(seq.rhs.node);
            trace_sources.push(seq.source_origin.clone());
        }
        //
        grammar.rewrite_sequences();

        IrPreparedGrammar {
            grammar: grammar,
            ordering: ordering,
            // Final
            common: IrCommonPrepared {
                external_grammar: external_grammar,
                actions: actions,
                attrs: ir.attrs,
                errors: ir.errors,
                trace_tokens: ir.trace_tokens,
                trace_sources: trace_sources,
                lower_level: lower_level,
                name_map: ir.name_map,
                hir_rules_with_names: ir.hir_rules_with_names,
                hir: hir,
            }
        }
    }
}

impl IrBinarizedGrammar {
    fn compute(mut ir: IrPreparedGrammar) -> Result<Self, TransformationError> {
        let mut cycle_auto_type = vec![];
        let mut cycle_matrix = BitMatrix::new(ir.grammar.num_syms(), ir.grammar.num_syms());
        for (&lhs_sym, ty) in &ir.common.hir.type_map {
            match ty {
                &Ty::Auto(AutoTy::Tuple { ref fields }) => {
                    for &field_sym in fields {
                        cycle_matrix.set(lhs_sym.usize(), field_sym.usize(), true);
                    }
                }
                &Ty::Auto(AutoTy::Struct { ref members }) => {
                    for &member_sym in members.values() {
                        cycle_matrix.set(lhs_sym.usize(), member_sym.usize(), true);
                    }
                }
                _ => {}
            }
        }
        cycle_matrix.transitive_closure();
        for ((hir_rule, rule), action) in ir.common.hir_rules_with_names.iter()
                                          .zip(ir.grammar.rules())
                                          .zip(ir.common.actions.iter()) {
            if cycle_matrix[(rule.lhs().usize(), rule.lhs().usize())] {
                let mut causes = vec![];
                match action {
                    &Action::Tuple { ref tuple_binds } => {
                        for &bind_pos in tuple_binds {
                            let sym = rule.rhs()[bind_pos];
                            if cycle_matrix[(sym.usize(), sym.usize())] {
                                causes.push(hir_rule.rhs[bind_pos]);
                            }
                        }
                    }
                    &Action::Struct { ref deep_binds, ref shallow_binds, .. } => {
                        let shallow_bind_pos_iter = shallow_binds.iter().map(|t| t.0);
                        for bind_pos in deep_binds.iter().cloned().chain(shallow_bind_pos_iter) {
                            let sym = rule.rhs()[bind_pos];
                            if cycle_matrix[(sym.usize(), sym.usize())] {
                                causes.push(hir_rule.rhs[bind_pos]);
                            }
                        }
                    }
                }
                cycle_auto_type.push((hir_rule.lhs, causes));
            }
        }
        if !cycle_auto_type.is_empty() {
            ir.common.errors.push(TransformationError::RecursiveType(cycle_auto_type));
        }

        let bin_grammar = ir.grammar.binarize();

        Ok(IrBinarizedGrammar {
            bin_grammar: bin_grammar,
            ordering: ir.ordering,
            // Final
            common: ir.common,
        })
    }
}

impl IrNonNullingBinarized {
    fn compute(ir: IrBinarizedGrammar) -> Self {
        // eliminate
        let (bin_grammar, mut nulling_grammar) = ir.bin_grammar.eliminate_nulling();
        let start = bin_grammar.get_start();
        let trivial_derivation = nulling_grammar.rules().any(|rule| rule.lhs() == start);
        let mut null_cycle_matrix = BitMatrix::new(bin_grammar.num_syms(), bin_grammar.num_syms());
        for rule in nulling_grammar.rules() {
            for sym in rule.rhs() {
                null_cycle_matrix.set(rule.lhs().usize(), sym.usize(), true);
            }
        }
        null_cycle_matrix.transitive_closure();
        let mut cycles_among_nullable = vec![];
        nulling_grammar.retain(|lhs, rhs, history| {
            let any_sym_on_diagonal = rhs.iter().chain(iter::once(&lhs)).any(|sym| {
                null_cycle_matrix[(sym.usize(), sym.usize())]
            });
            if any_sym_on_diagonal {
                cycles_among_nullable.push(history.origin().expect("internal rule with a cycle"));
                false
            } else {
                true
            }
        });

        IrNonNullingBinarized {
            bin_grammar: bin_grammar,
            nulling_grammar: nulling_grammar,
            ordering: ir.ordering,
            cycles_among_nullable: cycles_among_nullable,
            // Final
            trivial_derivation: trivial_derivation,
            common: ir.common,
        }
    }
}

impl IrProperNonNullingBinarized {
    fn compute(mut ir: IrNonNullingBinarized) -> Self {
        let mut unreachable_rules = vec![];
        let mut unproductive_rules = vec![];
        // analyze reachability and productiveness
        {
            let start = ir.bin_grammar.get_start();
            let mut usefulness = Usefulness::new(&mut *ir.bin_grammar).reachable([start]);
            if !usefulness.all_useful() {
                for rule_uselessness in usefulness.useless_rules() {
                    if rule_uselessness.unreachable {
                        if let Some(origin) = rule_uselessness.rule.history().origin() {
                            unreachable_rules.push(origin);
                        }
                    } else {
                        // unproductive
                        let rule = rule_uselessness.rule;
                        for (pos, &sym) in rule.rhs().iter().enumerate() {
                            if !usefulness.productivity(sym) {
                                let dots = rule.history().dots();
                                match (dots[pos].trace(), dots[pos + 1].trace()) {
                                    (Some((origin1, pos1)), Some((origin2, pos2))) => {
                                        if origin1 == origin2 && pos1 + 1 == pos2 {
                                            unproductive_rules.push((origin1, pos1));
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                }
                usefulness.remove_useless_rules();
            }
        };
        let cycles: Vec<_>;
        {
            let mut cycle_analysis = Cycles::new(&mut *ir.bin_grammar);
            cycles = cycle_analysis.cycle_participants().map(|rule| {
                rule.history().origin().expect("internal rule participates in a cycle")
            }).collect();
            cycle_analysis.remove_cycles();
        };

        IrProperNonNullingBinarized {
            bin_grammar: ir.bin_grammar,
            nulling_grammar: ir.nulling_grammar,
            ordering: ir.ordering,
            // Warning causes
            cycles: cycles,
            cycles_among_nullable: ir.cycles_among_nullable,
            unreachable_rules: unreachable_rules,
            unproductive_rules: unproductive_rules,
            // Final
            trivial_derivation: ir.trivial_derivation,
            common: ir.common,
        }
    }
}

impl IrMappedGrammar {
    fn compute(mut ir: IrProperNonNullingBinarized) -> Result<Self, TransformationError> {
        // remap symbols
        let IrProperNonNullingBinarized { mut bin_grammar, ordering, .. } = ir;
        let maps = {
            // which one first?
            let mut remap = Remap::new(&mut *bin_grammar);
            remap.reorder_symbols(|left, right| {
                ordering.get(&(left, right)).cloned().unwrap_or(Ordering::Equal)
            });
            remap.remove_unused_symbols();
            remap.get_mapping()
        };

        if let Some(start) = maps.to_internal[bin_grammar.get_start().usize()] {
            bin_grammar.set_start(start);
        } else {
            // This is the second place where a grammar is checked for being empty.
            ir.common.errors.push(TransformationError::GrammarIsEmpty);
        };
        let maps = InternalExternalNameMap::new(maps, ir.common.name_map.clone());

        Ok(IrMappedGrammar {
            bin_mapped_grammar: bin_grammar,
            nulling_grammar: ir.nulling_grammar,
            maps: maps,
            // Warning causes
            cycles: ir.cycles,
            cycles_among_nullable: ir.cycles_among_nullable,
            unproductive_rules: ir.unproductive_rules,
            unreachable_rules: ir.unreachable_rules,
            // Final
            trivial_derivation: ir.trivial_derivation,
            common: ir.common,
        })
    }

    pub fn transform_from_stmts(stmts: ast::Stmts) -> Result<Self, TransformationError> {
        let attrs = try!(Attrs::compute(&stmts.attrs[..]));
        let ir = try!(IrStmtsAndAttrs::compute(stmts, attrs));
        let ir = try!(IrInitialHir::compute(ir));
        let ir = IrFinalHir::compute(ir);
        let ir = IrPreparedGrammar::compute(ir);
        let ir = try!(IrBinarizedGrammar::compute(ir));
        let ir = IrNonNullingBinarized::compute(ir);
        let ir = IrProperNonNullingBinarized::compute(ir);
        IrMappedGrammar::compute(ir)
    }

    pub fn report_warnings(&self, cx: &mut rs::ExtCtxt) {
        let warn = WarningCauses {
            attrs: &self.common.attrs,
            external_grammar: &self.common.external_grammar[..],
            hir_rules_with_names: &self.common.hir_rules_with_names[..],
            cycles: &self.cycles[..],
            cycles_among_nullable: &self.cycles_among_nullable[..],
            unproductive_rules: &self.unproductive_rules[..],
            unreachable_rules: &self.unreachable_rules[..],
        };
        warn.report_warnings(cx);
    }

    pub fn get_errors(&self) -> Option<&[TransformationError]> {
        if self.common.errors.is_empty() {
            None
        } else {
            Some(&self.common.errors[..])
        }
    }
}

impl From<IrMappedGrammar> for Ir {
    fn from(ir: IrMappedGrammar) -> Self {
        Ir {
            grammar: ir.bin_mapped_grammar,
            nulling_grammar: ir.nulling_grammar,
            external_grammar: ir.common.external_grammar,
            sequences: ir.common.hir.sequence_rules,
            trivial_derivation: ir.trivial_derivation,
            actions: ir.common.actions,
            type_map: ir.common.hir.type_map,
            maps: ir.maps,
            assert_type_equality: ir.common.hir.assert_type_equality.into_inner(),
            lexer_for_upper: ir.common.attrs.lexer_for_upper().clone(),
            lower_level: ir.common.lower_level,
            trace_tokens: ir.common.trace_tokens,
            trace_sources: ir.common.trace_sources,
        }
    }
}

impl Ir {
    pub fn transform(stmts: ast::Stmts) -> Result<Self, TransformationError> {
        IrMappedGrammar::transform_from_stmts(stmts).map(|ir| ir.into())
    }

    pub fn to_internal(&self, symbol: Symbol) -> Option<Symbol> {
        self.maps.to_internal(symbol)
    }

    pub fn to_external(&self, symbol: Symbol) -> Symbol {
        self.maps.to_external(symbol)
    }

    pub fn name_of_external(&self, symbol: Symbol) -> Option<rs::Name> {
        self.maps.name_of_external(symbol)
    }

    pub fn name_map(&self) -> &NameMap {
        self.maps.name_map()
    }
}
