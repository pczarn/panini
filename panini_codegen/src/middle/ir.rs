use std::collections::HashMap;
use std::cmp::Ordering;
use std::mem;
use std::iter;

use bit_matrix::BitMatrix;
use gearley::grammar::{Grammar, BinarizedGrammar, History};
use cfg::cycles::Cycles;
use cfg::remap::{Remap, Mapping};
use cfg::rule::RuleRef;
use cfg::rule::container::RuleContainer;
use cfg::usefulness::Usefulness;
use cfg::*;
use cfg_regex::{RegexTranslation, ClassRange};

use rs;
use front::ast;
use middle::{ActionExpr, Ty, Lexer, Hir, Folder, FoldHir, AutoTy, SymbolicName};
use middle::error::{TransformationError, CycleWithCauses};
use middle::trace::{Trace, SourceOrigin};
use middle::attr::{Attrs, ArgumentsFromOuterLayer};
use middle::warn::{WarningCauses, WarningsWithContext};
use middle::rule::{Rule, BasicRule};
use middle::embedded_string::EmbeddedString;

pub struct Ir {
    pub grammar: BinarizedGrammar,
    pub nulling_grammar: BinarizedGrammar,
    pub trivial_derivation: bool,
    // for actions, accessed by action ID.
    pub basic_rules: Vec<BasicRule>,
    pub type_map: HashMap<Symbol, Ty>,
    pub maps: InternalExternalNameMap,
    pub assert_type_equality: Vec<(Symbol, Ty)>,
    // what the heck is LexerForUpper?
    pub arguments_from_outer_layer: Option<ArgumentsFromOuterLayer<Symbol>>,
    pub invocation_of_inner_layer: InvocationOfInnerLayer,
    pub trace_tokens: Vec<Vec<String>>,
    pub trace_sources: Vec<SourceOrigin>,
}

#[derive(Clone)]
pub struct NameMap {
    pub sym_map: HashMap<SymbolicName, Symbol>,
    pub sym_vec: Vec<Option<SymbolicName>>,
}

pub struct InternalExternalNameMap {
    internal_external: Mapping,
    name_map: NameMap,
}

impl NameMap {
    fn insert_padded(&mut self, sym: Symbol, name: SymbolicName) {
        if self.sym_vec.len() <= sym.usize() {
            let pad_len = sym.usize() - self.sym_vec.len() + 1;
            self.sym_vec.extend(iter::repeat(None).take(pad_len));
        }
        self.sym_vec[sym.usize()] = Some(name);
        self.sym_map.insert(name, sym);
    }

    fn to_name(&self, sym: Symbol) -> Option<SymbolicName> {
        self.sym_vec.get(sym.usize()).and_then(|opt| *opt)
    }

    pub fn names(&self) -> &[Option<SymbolicName>] {
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

    pub fn internalize(&self, sym: Symbol) -> Option<Symbol> {
        self.internal_external.to_internal[sym.usize()]
    }

    pub fn externalize(&self, sym: Symbol) -> Symbol {
        self.internal_external.to_external[sym.usize()]
    }

    pub fn name_of_external(&self, sym: Symbol) -> Option<SymbolicName> {
        self.name_map.to_name(sym)
    }

    fn name_map(&self) -> &NameMap {
        &self.name_map
    }
}

/// Describes how the inner layer will be invoked. Most of the time, the inner layer
/// is either a sub-grammar described by the grammar author, or an implicit invocation of
/// a character classifier.
#[derive(Debug, Eq, PartialEq)]
pub enum InvocationOfInnerLayer {
    Invoke {
        lexer_invocation: Lexer,
        embedded_strings: Vec<EmbeddedString>,
    },
    CharClassifier(Vec<(ClassRange, Symbol)>),
    None
}

struct IrStmtsAndAttrs {
    stmts: ast::Stmts,
    start: SymbolicName,
    attrs: Attrs<SymbolicName>,
    errors: Vec<TransformationError>,
}

struct IrInitialHir {
    hir_with_names: Hir<SymbolicName>,
    start: SymbolicName,
    attrs: Attrs<SymbolicName>,
    errors: Vec<TransformationError>,
    // Final
    lower_level: InvocationOfInnerLayer,
    trace_tokens: Vec<Vec<String>>,
}

struct IrFinalHir {
    hir: Hir,
    grammar: Grammar,
    name_map: NameMap,
    attrs: Attrs<Symbol>,
    errors: Vec<TransformationError>,
    // Final
    lower_level: InvocationOfInnerLayer,
    trace_tokens: Vec<Vec<String>>,
}

struct IrPrepared {
    grammar: Grammar,
    // Final
    common: CommonPrepared,
}

struct IrBinarized {
    bin_grammar: BinarizedGrammar,
    // Final
    common: CommonPrepared,
}

struct IrNormalized {
    bin_grammar: BinarizedGrammar,
    nulling_grammar: BinarizedGrammar,
    // Warning causes
    warnings: WarningCauses,
    // Final
    trivial_derivation: bool,
    common: CommonPrepared,
}

struct IrProperNormalized {
    bin_grammar: BinarizedGrammar,
    nulling_grammar: BinarizedGrammar,
    // Warning causes
    warnings: WarningCauses,
    // Final
    trivial_derivation: bool,
    common: CommonPrepared,
}

pub struct IrMapped {
    bin_mapped_grammar: BinarizedGrammar,
    nulling_grammar: BinarizedGrammar,
    maps: InternalExternalNameMap,
    // Warning causes
    warnings: WarningCauses,
    // Final
    trivial_derivation: bool,
    common: CommonPrepared,
}

// Common

struct CommonPrepared {
    basic_rules: Vec<BasicRule>,
    attrs: Attrs<Symbol>,
    lower_level: InvocationOfInnerLayer,
    trace_tokens: Vec<Vec<String>>,
    trace_sources: Vec<SourceOrigin>,
    name_map: NameMap,
    hir: Hir,
    errors: Vec<TransformationError>,
}

impl IrStmtsAndAttrs {
    fn compute(mut stmts: ast::Stmts, attrs: Attrs<SymbolicName>) -> Result<Self, TransformationError> {
        // Obtain the explicit start.
        let start = if let Some(first_stmt) = stmts.stmts.get(0) {
            first_stmt.lhs.node
        } else {
            return Err(TransformationError::GrammarIsEmpty);
        };

        if let &Some(ref from_outer) = attrs.arguments_from_outer_layer() {
            // Set the implicit start.
            let start = rs::gensym("_lower_start_");
            for terminal in from_outer.terminals() {
                let lower_start_stmt = ast::Stmt {
                    lhs: rs::dummy_spanned(start),
                    rhs: vec![
                        vec![(
                            ast::Rhs(vec![
                                ast::RhsElement {
                                    bind: None,
                                    elem: ast::RhsAst::Symbol(rs::dummy_spanned(*terminal))
                                }
                            ]),
                            ast::Action { expr: None }
                        )]
                    ],
                    ty: None,
                    span: rs::DUMMY_SP,
                };
                stmts.stmts.push(lower_start_stmt);
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

        let lower_level = if let Some(lexer_invocation) = ir.stmts.lexer {
            InvocationOfInnerLayer::Invoke {
                lexer_invocation: lexer_invocation,
                embedded_strings: vec![]
            }
        } else {
            InvocationOfInnerLayer::None
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
        }
    }
}

impl IrPrepared {
    fn compute(mut ir: IrFinalHir) -> Self {
        let IrFinalHir { mut hir, mut grammar, .. } = ir;
        let embedded_strings = hir.embedded_strings.clone();
        let lower_level = if !hir.embedded_strings.is_empty() {
            match ir.lower_level {
                InvocationOfInnerLayer::Invoke { lexer_invocation, .. } => {
                    // Add strings to the lower level.
                    InvocationOfInnerLayer::Invoke {
                        embedded_strings: embedded_strings,
                        lexer_invocation: lexer_invocation,
                    }
                }
                InvocationOfInnerLayer::None => {
                    if ir.attrs.arguments_from_outer_layer().is_some() {
                        // We are at level 1 .. N and adding strings to the current level.
                        // Embedded strings are rewritten into rules. Character ranges are
                        // collected.
                        let mut regex_rewrite = RegexTranslation::new();
                        for embedded in embedded_strings {
                            let sym2 = regex_rewrite.rewrite_string(
                                &mut grammar,
                                &*embedded.string.node.as_str()
                            );
                            hir.rules.push(Rule::BnfRule {
                                lhs: embedded.symbol,
                                rhs: vec![rs::dummy_spanned(sym2)],
                                tuple_binds: vec![],
                                deep_binds: vec![],
                                shallow_binds: vec![],
                                source_origin: embedded.source,
                                action: ActionExpr::Auto,
                            });
                            hir.type_map.insert(
                                embedded.symbol.node,
                                Ty::Auto(AutoTy::Tuple { fields: vec![] })
                            );
                        }
                        let char_ranges = regex_rewrite.get_ranges().clone().into_iter()
                                                                            .collect::<Vec<_>>();
                        for (i, &(_, sym)) in char_ranges.iter().enumerate() {
                            let name = rs::gensym(&*format!("ChRange{}", i));
                            ir.name_map.insert_padded(sym, name);
                        }
                        InvocationOfInnerLayer::CharClassifier(char_ranges)
                    } else {
                        // We are at level 0 and adding strings to a newly created level 1.
                        InvocationOfInnerLayer::Invoke {
                            lexer_invocation: Lexer::new(rs::intern("grammar"), vec![]),
                            embedded_strings: embedded_strings,
                        }
                    }
                }
                InvocationOfInnerLayer::CharClassifier(_) => unreachable!()
            }
        } else {
            ir.lower_level
        };

        let mut trace_sources = vec![];
        let mut basic_rules = vec![];
        // Code common to all rules.
        for rule in &hir.rules {
            rule.add_to(&mut grammar);
            basic_rules.extend(rule.basic_rules().into_iter());
            trace_sources.push(rule.source_origin().clone());
        }
        // Must rewrite sequence rules. They need to be analyzed later.
        grammar.rewrite_sequences();

        IrPrepared {
            grammar: grammar,
            // Final
            common: CommonPrepared {
                basic_rules: basic_rules,
                attrs: ir.attrs,
                errors: ir.errors,
                trace_tokens: ir.trace_tokens,
                trace_sources: trace_sources,
                lower_level: lower_level,
                name_map: ir.name_map,
                hir: hir,
            }
        }
    }
}

impl IrBinarized {
    fn compute(mut ir: IrPrepared) -> Result<Self, TransformationError> {
        let mut cycle_matrix = BitMatrix::new(ir.grammar.num_syms(), ir.grammar.num_syms());
        for (&lhs_sym, ty) in &ir.common.hir.type_map {
            for ty_sym in ty.symbols() {
                cycle_matrix.set(lhs_sym.usize(), ty_sym.usize(), true);
            }
        }
        let mut cycles_among_auto_types = vec![];
        cycle_matrix.transitive_closure();
        // make sure the actions still correspond to grammar rules.
        // do not need raw hir rules?
        for rule in &ir.common.basic_rules {
            // Declare lambdas
            let to_rhs_symbol = |pos: usize| rule.rhs[pos];
            let is_in_cycle = |sym: &rs::Spanned<Symbol>| cycle_matrix[(sym.node.usize(), sym.node.usize())];
            // 
            if is_in_cycle(&rule.lhs) {
                // why does this only run for bound symbols, not all symbols? optimization??
                // add regression test for recursive type among sequences?
                let bound_symbols = rule.action.directly_bound_positions().map(to_rhs_symbol);
                let causes = bound_symbols.filter(is_in_cycle).collect();
                // can we access the symbolic name through the maps instead?
                // do we access the symbolic name correctly? equivalently to through hir_map? - No, because we need spans from the hir_map.
                // rule.history().origin()
                cycles_among_auto_types.push(CycleWithCauses {
                    lhs: rule.lhs,
                    causes: causes,
                });
            }
        }
        if cycles_among_auto_types.len() > 0 {
            // We have cycles among types. Report them.
            ir.common.errors.push(TransformationError::RecursiveType(cycles_among_auto_types));
        }

        let bin_grammar = ir.grammar.binarize();

        Ok(IrBinarized {
            bin_grammar: bin_grammar,
            // Final
            common: ir.common,
        })
    }
}

impl IrNormalized {
    fn compute(ir: IrBinarized) -> Self {
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
        let mut warnings = WarningCauses::new();
        nulling_grammar.retain(|lhs, rhs, history| {
            let is_in_cycle = |sym: &Symbol| null_cycle_matrix[(sym.usize(), sym.usize())];
            let lhs_once = [lhs];
            let mut syms = lhs_once.iter().chain(rhs.iter());
            if syms.any(is_in_cycle) {
                warnings.cycles_among_nullable.push(history.origin().expect("internal rule with a cycle"));
                false
            } else {
                true
            }
        });

        IrNormalized {
            bin_grammar: bin_grammar,
            nulling_grammar: nulling_grammar,
            warnings: warnings,
            // Final
            trivial_derivation: trivial_derivation,
            common: ir.common,
        }
    }
}

impl IrProperNormalized {
    fn compute(mut ir: IrNormalized) -> Self {
        // analyze reachability and productiveness
        {
            let start = ir.bin_grammar.get_start();
            let mut usefulness = Usefulness::new(&mut *ir.bin_grammar).reachable([start]);
            for rule_uselessness in usefulness.useless_rules() {
                if rule_uselessness.unreachable {
                    if let Some(origin) = rule_uselessness.rule.history().origin() {
                        ir.warnings.unreachable_rules.push(origin);
                    }
                } else {
                    // unproductive
                    let rule = rule_uselessness.rule;
                    for (pos, &sym) in rule.rhs().iter().enumerate() {
                        if !usefulness.productivity(sym) {
                            let dots = rule.history().dots();
                            match (dots[pos].trace(), dots[pos + 1].trace()) {
                                (Some(dot1), Some(dot2)) => {
                                    if dot1.rule == dot2.rule && dot1.pos + 1 == dot2.pos {
                                        ir.warnings.unproductive_rules.push((dot1.rule, dot1.pos));
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            // does it do anything when all rules are useful?
            usefulness.remove_useless_rules();
        };
        let to_rule_origin = |rule: RuleRef<History>| {
            rule.history().origin().expect("internal rule in a cycle")
        };
        {
            let mut cycle_analysis = Cycles::new(&mut *ir.bin_grammar);
            ir.warnings.cycles = cycle_analysis.cycle_participants().map(to_rule_origin).collect();
            cycle_analysis.remove_cycles();
        };

        IrProperNormalized {
            bin_grammar: ir.bin_grammar,
            nulling_grammar: ir.nulling_grammar,
            // Warning causes
            warnings: ir.warnings,
            // Final
            trivial_derivation: ir.trivial_derivation,
            common: ir.common,
        }
    }
}

impl IrMapped {
    fn compute(mut ir: IrProperNormalized) -> Result<Self, TransformationError> {
        // remap symbols
        let IrProperNormalized { mut bin_grammar, .. } = ir;
        // Order rules 
        let mut ordering = HashMap::new();
        for rule in bin_grammar.rules() {
            if rule.rhs().len() == 1 {
                let mut left = rule.lhs();
                let mut right = rule.rhs()[0];
                let ord;
                if left.usize() > right.usize() {
                    mem::swap(&mut left, &mut right);
                    ord = Ordering::Less;
                } else {
                    ord = Ordering::Greater;
                }
                ordering.insert((left, right), ord);
            }
        }
        let maps = {
            // which one first?
            // any way of not introducting a new scope, perhaps with an extension trait, a builder?
            let mut remap = Remap::new(&mut *bin_grammar);
            remap.reorder_symbols(|left, right| {
                // - what if left > right? does it break?
                let should_swap = left.usize() > right.usize();
                let ord = if should_swap {
                    ordering.get(&(right, left)).cloned().map(|ord| ord.reverse())
                } else {
                    ordering.get(&(left, right)).cloned()
                };
                ord.unwrap_or(Ordering::Equal)
            });
            remap.remove_unused_symbols();
            remap.get_mapping()
        };
        // Create a symbol mapping.
        let maps = InternalExternalNameMap::new(maps, ir.common.name_map.clone());
        // Internalize the start symbol.
        if let Some(start) = maps.internalize(bin_grammar.get_start()) {
            bin_grammar.set_start(start);
        } else {
            // This is the second place where a grammar is checked for being empty.
            ir.common.errors.push(TransformationError::GrammarIsEmpty);
        };
        let mut bin = BinarizedGrammar::new();
        for _ in 0 .. bin_grammar.num_syms() {
            let _: Symbol = bin.sym();
        }
        for rule in bin_grammar.rules() {
            // rhs = rule.rhs().to_owned();
            let mut history = rule.history().clone();
            history.nullable = history.nullable.map(|(sym, pos)| (maps.internalize(sym).unwrap(), pos));
            bin.rule(rule.lhs()).rhs_with_history(rule.rhs(), history);
        }
        let mut remapped_nulling_grammar = BinarizedGrammar::new();
        if let Some(start) = maps.internalize(ir.nulling_grammar.get_start()) {
            remapped_nulling_grammar.set_start(start);
        }
        for _ in 0 .. bin_grammar.num_syms() {
            let _: Symbol = remapped_nulling_grammar.sym();
        }
        bin.set_start(bin_grammar.get_start());
        for rule in ir.nulling_grammar.rules() {
            let lhs = maps.internalize(rule.lhs()).unwrap();
            let rhs: Vec<_>;
            rhs = rule.rhs().iter().map(|sym| maps.internalize(*sym).unwrap()).collect();
            let history = rule.history().clone();
            remapped_nulling_grammar.rule(lhs).rhs_with_history(&rhs, history);
        }
        Ok(IrMapped {
            bin_mapped_grammar: bin,
            nulling_grammar: remapped_nulling_grammar,
            maps: maps,
            // Warning causes
            warnings: ir.warnings,
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
        let ir = IrPrepared::compute(ir);
        let ir = try!(IrBinarized::compute(ir));
        let ir = IrNormalized::compute(ir);
        let ir = IrProperNormalized::compute(ir);
        IrMapped::compute(ir)
    }

    pub fn report_warnings(&self, cx: &mut rs::ExtCtxt) {
        let warn = WarningsWithContext {
            attrs: &self.common.attrs,
            basic_rules: &self.common.basic_rules[..],
            causes: &self.warnings,
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

impl From<IrMapped> for Ir {
    fn from(ir: IrMapped) -> Self {
        Ir {
            grammar: ir.bin_mapped_grammar,
            nulling_grammar: ir.nulling_grammar,
            trivial_derivation: ir.trivial_derivation,
            basic_rules: ir.common.basic_rules,
            type_map: ir.common.hir.type_map,
            maps: ir.maps,
            assert_type_equality: ir.common.hir.assert_type_equality.into_inner(),
            arguments_from_outer_layer: ir.common.attrs.arguments_from_outer_layer().clone(),
            invocation_of_inner_layer: ir.common.lower_level,
            trace_tokens: ir.common.trace_tokens,
            trace_sources: ir.common.trace_sources,
        }
    }
}

impl Ir {
    pub fn transform(stmts: ast::Stmts) -> Result<Self, TransformationError> {
        IrMapped::transform_from_stmts(stmts).map(|ir| ir.into())
    }

    pub fn internalize(&self, symbol: Symbol) -> Option<Symbol> {
        self.maps.internalize(symbol)
    }

    pub fn externalize(&self, symbol: Symbol) -> Symbol {
        self.maps.externalize(symbol)
    }

    pub fn name_of_external(&self, symbol: Symbol) -> Option<SymbolicName> {
        self.maps.name_of_external(symbol)
    }

    pub fn name_map(&self) -> &NameMap {
        self.maps.name_map()
    }
}
