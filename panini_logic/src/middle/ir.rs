use std::collections::{HashMap, BTreeMap, BTreeSet};
use std::cmp::Ordering;
use std::mem;
use std::iter;

use bit_matrix::BitMatrix;
use gearley::grammar::{Grammar, BinarizedGrammar, History};
use cfg::classification::cyclical::Cycles;
use cfg::remap::{Remap, Mapping};
use cfg::rule::RuleRef;
use cfg::rule::container::RuleContainer;
use cfg::classification::useful::Usefulness;
use cfg::*;
use cfg_regex::{RegexTranslation, Class};

use input::{ast, LEXER_START_FRAGMENT, LexerId, FragmentId};
use input::attr_arguments::AttrArguments;
use middle::error::{TransformationError, CycleWithCauses};
use middle::trace::Trace;
use middle::rule_rewrite::{RuleValue, Rule, Sym, RuleRewrite, LowerRuleRewriteResult, RuleRewriteResult};
use middle::flatten_stmts::{FlattenStmts, Path, Position};
use middle::type_collector::{Type, TypeCollector};
use middle::warn::{WarningCauses, WarningsWithContext};
use middle::embedded_string::EmbeddedString;
use middle::symbol_maps::SymbolMaps;

pub struct Ir {
    pub grammar: BinarizedGrammar,
    pub nulling_grammar: BinarizedGrammar,
    pub trivial_derivation: bool,
    // for actions, accessed by action ID.
    pub rules: LowerRuleRewriteResult,
    pub type_map: BTreeMap<Path, BTreeSet<Type>>,
    pub maps: SymbolMaps,
    // pub assert_type_equality: Vec<(Symbol, Type)>,
    pub attr_arguments: AttrArguments,
    pub lexer_layer: LexerLayer,
    pub trace: Trace,
    pub errors: Vec<TransformationError>,
}

/// Describes how the inner layer will be invoked. Most of the time, the inner layer
/// is either a sub-grammar described by the grammar author, or an implicit invocation of
/// a character classifier.
#[derive(Debug)]
pub enum LexerLayer {
    Invoke {
        lexer_id: LexerId,
        embedded_strings: Vec<String>,
    },
    CharClassifier(Vec<(Class, Symbol)>),
    None
}

struct IrStmts {
    stmts: ast::Stmts,
    start: FragmentId,
}

struct IrWithRules {
    rules: RuleRewriteResult,
    start: FragmentId,
    attr_arguments: AttrArguments,
    errors: Vec<TransformationError>,
    embedded_strings: Vec<String>, // TODO
    // Final
    type_collector: TypeCollector,
    lexer_layer: LexerLayer,
    trace: Trace,
}

// struct IrFinalHir {
//     rules: RuleRewriteResult,
//     grammar: Grammar,
//     sym_map: SymMap,
//     attr_arguments: AttrArguments,
//     errors: Vec<TransformationError>,
//     // Final
//     lexer_layer: LexerLayer,
//     trace: Trace,
// }

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
    // Warning causes
    warnings: WarningCauses,
    // Final
    trivial_derivation: bool,
    common: CommonPrepared,
}

// Common

struct CommonPrepared {
    rules: LowerRuleRewriteResult,
    attr_arguments: AttrArguments,
    errors: Vec<TransformationError>,
    trace: Trace,
    lexer_layer: LexerLayer,
    sym_map: SymbolMaps,
    type_map: BTreeMap<Path, BTreeSet<Type>>,
}

impl IrStmts {
    fn compute(mut stmts: ast::Stmts) -> Result<Self, TransformationError> {
        // Obtain the explicit start.
        let start = if let Some(first_stmt) = stmts.stmts.get(0) {
            first_stmt.lhs
        } else {
            return Err(TransformationError::GrammarIsEmpty);
        };

        if let Some(ref lexer_arguments) = stmts.attr_arguments.lexer_arguments {
            // Set the implicit start.
            for &terminal in &lexer_arguments.terminals {
                let lower_start_stmt = ast::Stmt {
                    lhs: LEXER_START_FRAGMENT,
                    body: vec![
                        (
                            0,
                            ast::Rhs(vec![
                                ast::RhsElement {
                                    bind: None,
                                    elem: ast::RhsAst::Fragment(terminal)
                                }
                            ]),
                            ast::Action { expr: None }
                        )
                    ],
                    ty: None,
                };
                stmts.stmts.push(lower_start_stmt);
            }
        }

        Ok(IrStmts {
            stmts,
            start,
        })
    }
}

impl IrWithRules {
    fn compute(ir: IrStmts) -> Result<Self, TransformationError> {
        let mut trace = Trace::from_stmts(&ir.stmts);
        let (rules, type_collector) = {
            let mut rule_rewrite = RuleRewrite::new(&mut trace);
            let mut flatten = FlattenStmts::new();
            flatten.flatten_stmts(&ir.stmts);
            rule_rewrite.rewrite(flatten.paths.clone());
            let mut type_collector = TypeCollector::new();
            type_collector.collect(flatten.join_stmts());
            type_collector.simplify_tuples();
            (rule_rewrite.result(), type_collector)
        };

        let mut errors = vec![];
        if let Some(inequalities) = type_collector.check_type_equality(&trace) {
            errors.extend(inequalities.into_iter().map(|spans|
                TransformationError::TypeMismatch(spans)
            ));
        }

        let lexer_layer = if let Some(lexer_id) = ir.stmts.lexer {
            LexerLayer::Invoke {
                lexer_id,
                embedded_strings: vec![]
            }
        } else {
            LexerLayer::None
        };

        Ok(IrWithRules {
            rules,
            start: ir.start,
            trace,
            lexer_layer,
            attr_arguments: ir.stmts.attr_arguments,
            embedded_strings: vec![],
            errors,
            type_collector,
        })
    }
}

// impl IrFinalHir {
//     fn compute(ir: IrInitialHir) -> Self {
//         let mut grammar = Grammar::new();
//         let (hir, sym_map, sym_vec) = {
//             let mut fold = Folder::new(grammar.sym_source_mut());
//             let hir = fold.fold_hir(ir.hir_with_names);
//             (hir, fold.sym_map, fold.sym_vec)
//         };
//         // let attrs = ir.attrs.map_symbols(|name| sym_map[&name]);
//         grammar.set_start(sym_map[&ir.start]);
//         IrFinalHir {
//             hir: hir,
//             grammar: grammar,
//             name_map: NameMap {
//                 sym_map: sym_map,
//                 sym_vec: sym_vec,
//             },
//             attrs: attrs,
//             errors: ir.errors,
//             lower_level: ir.lower_level,
//             trace_tokens: ir.trace_tokens,
//         }
//     }
// }

impl IrPrepared {
    fn compute(mut ir: IrWithRules) -> Self {
        let mut grammar = Grammar::new();
        let mut sym_map = SymbolMaps::new();
        let embedded_strings = ir.embedded_strings.clone();
        let lexer_layer = if !ir.embedded_strings.is_empty() {
            match ir.lexer_layer {
                LexerLayer::Invoke { lexer_id, .. } => {
                    // Add strings to the lower level.
                    LexerLayer::Invoke {
                        embedded_strings: embedded_strings,
                        lexer_id: lexer_id,
                    }
                }
                LexerLayer::None => {
                    if ir.attr_arguments.lexer_arguments.is_some() {
                        // // We are at level 1 .. N and adding strings to the current level.
                        // // Embedded strings are rewritten into rules. Character ranges are
                        // // collected.
                        // let mut regex_rewrite = RegexTranslation::new(&mut grammar);
                        // for embedded in embedded_strings {
                        //     let sym2 = regex_rewrite.rewrite_string(
                        //         &*embedded
                        //     );
                        //     // hir.type_map.insert(
                        //     //     embedded.symbol.node,
                        //     //     Ty::Auto(AutoTy::Tuple { fields: vec![] })
                        //     // );
                        //     unimplemented!()
                        // }
                        // let char_ranges = regex_rewrite.class_map().clone().into_iter()
                        //                                                     .collect::<Vec<_>>();
                        // for (i, &(_, sym)) in char_ranges.iter().enumerate() {
                        //     let sym_from_path = Sym::FromPath(Path {
                        //         position: vec![Position::Idx(i)]
                        //     });
                        //     sym_map.insert(sym, sym_from_path);
                        // }
                        // let class_ranges =
                        LexerLayer::CharClassifier(vec![])
                    } else {
                        // We are at level 0 and adding strings to a newly created level 1.
                        LexerLayer::Invoke {
                            lexer_id: 0,
                            embedded_strings: embedded_strings,
                        }
                    }
                }
                LexerLayer::CharClassifier(_) => unreachable!()
            }
        } else {
            ir.lexer_layer
        };
        let start_sym = Sym::Fragment(ir.start);
        let start = sym_map.intern(&mut grammar, &start_sym);
        grammar.set_start(start);
        // ir.type_collector.add_lhs(&mut grammar, &mut sym_map, &ir.rules);
        let grammar_rules = ir.rules.lower(&mut grammar, &mut sym_map);
        // Code common to all rules.
        for rule in &grammar_rules.rules {
            IrPrepared::add_rule(&mut grammar, &mut sym_map, rule);
        }
        // Must rewrite sequence rules. They need to be analyzed later.
        grammar.rewrite_sequences();

        IrPrepared {
            grammar: grammar,
            // Final
            common: CommonPrepared {
                rules: grammar_rules,
                attr_arguments: ir.attr_arguments,
                errors: ir.errors,
                trace: ir.trace,
                lexer_layer,
                sym_map,
                type_map: ir.type_collector.types,
            }
        }
    }

    fn add_rule(grammar: &mut Grammar, sym_map: &mut SymbolMaps, rule: &Rule) {
        match rule {
            &Rule { lhs, ref rhs, sequence: Some((min, max)), .. } => {
                let rhs = rhs.first().cloned().unwrap();
                grammar.sequence(lhs).inclusive(min, max).rhs(rhs);
            }
            &Rule { lhs, ref rhs, sequence: None, .. } => {
                grammar.rule(lhs).rhs(&rhs[..]);
            }
        }
    }
}

impl IrBinarized {
    fn compute(mut ir: IrPrepared) -> Result<Self, TransformationError> {
        // let mut cycle_matrix = BitMatrix::new(ir.grammar.num_syms(), ir.grammar.num_syms());
        // // for (&lhs_sym, ty) in &ir.common.type_map {
        // //     for ty_sym in ty.symbols() {
        // //         cycle_matrix.set(lhs_sym.usize(), ty_sym.usize(), true);
        // //     }
        // // }
        // let mut cycles_among_auto_types = vec![];
        // cycle_matrix.transitive_closure();
        // // make sure the actions still correspond to grammar rules.
        // // do not need raw hir rules?
        // for rule in &ir.common.rules {
        //     // Declare lambdas
        //     let to_rhs_symbol = |pos: usize| rule.rhs[pos];
        //     let is_in_cycle = |sym: &rs::Spanned<Symbol>| cycle_matrix[(sym.node.usize(), sym.node.usize())];
        //     // 
        //     if is_in_cycle(&rule.lhs) {
        //         // why does this only run for bound symbols, not all symbols? optimization??
        //         // add regression test for recursive type among sequences?
        //         let bound_symbols = rule.action.directly_bound_positions().map(to_rhs_symbol);
        //         let causes = bound_symbols.filter(is_in_cycle).collect();
        //         // can we access the symbolic name through the maps instead?
        //         // do we access the symbolic name correctly? equivalently to through hir_map? - No, because we need spans from the hir_map.
        //         // rule.history().origin()
        //         cycles_among_auto_types.push(CycleWithCauses {
        //             lhs: rule.lhs,
        //             causes: causes,
        //         });
        //     }
        // }
        // if cycles_among_auto_types.len() > 0 {
        //     // We have cycles among types. Report them.
        //     ir.common.errors.push(TransformationError::RecursiveType(cycles_among_auto_types));
        // }

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
        let _start = ir.bin_grammar.start();
        let (bin_grammar, mut nulling_grammar) = ir.bin_grammar.eliminate_nulling();
        let start = bin_grammar.start();
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
            let start = ir.bin_grammar.start();
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
                            let (dot_a, dot_b) = (rule.history().dot(pos), rule.history().dot(pos + 1));
                            match (dot_a.trace(), dot_b.trace()) {
                                (Some(dot1), Some(dot2)) => {
                                    if dot1.0 == dot2.0 && dot1.1 + 1 == dot2.1 {
                                        ir.warnings.unproductive_rules.push((dot1.0, dot1.1));
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
        {
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
            // Set a symbol mapping.
            ir.common.sym_map.internal_external = remap.get_mapping();
        };
        // Internalize the start symbol.
        if let Some(start) = ir.common.sym_map.internalize(bin_grammar.start()) {
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
            history.nullable = history.nullable.map(|(sym, pos)| (ir.common.sym_map.internalize(sym).unwrap(), pos));
            bin.rule(rule.lhs()).rhs_with_history(rule.rhs(), history);
        }
        let mut remapped_nulling_grammar = BinarizedGrammar::new();
        if let Some(start) = ir.common.sym_map.internalize(ir.nulling_grammar.start()) {
            remapped_nulling_grammar.set_start(start);
        }
        for _ in 0 .. bin_grammar.num_syms() {
            let _: Symbol = remapped_nulling_grammar.sym();
        }
        bin.set_start(bin_grammar.start());
        for rule in ir.nulling_grammar.rules() {
            let lhs = ir.common.sym_map.internalize(rule.lhs()).unwrap();
            let rhs: Vec<_>;
            rhs = rule.rhs().iter().map(|sym| ir.common.sym_map.internalize(*sym).unwrap()).collect();
            let history = rule.history().clone();
            remapped_nulling_grammar.rule(lhs).rhs_with_history(&rhs, history);
        }
        Ok(IrMapped {
            bin_mapped_grammar: bin,
            nulling_grammar: remapped_nulling_grammar,
            // Warning causes
            warnings: ir.warnings,
            // Final
            trivial_derivation: ir.trivial_derivation,
            common: ir.common,
        })
    }

    pub fn transform_from_stmts(stmts: ast::Stmts) -> Result<Self, TransformationError> {
        // let attrs = Attrs::compute(&stmts.attrs[..])?;
        let ir = IrStmts::compute(stmts)?;
        let ir = IrWithRules::compute(ir)?;
        let ir = IrPrepared::compute(ir);
        let ir = IrBinarized::compute(ir)?;
        let ir = IrNormalized::compute(ir);
        let ir = IrProperNormalized::compute(ir);
        IrMapped::compute(ir)
    }

    pub fn report_warnings(&self) {
        unimplemented!()
        // let warn = WarningsWithContext {
        //     attrs: &self.common.attrs,
        //     basic_rules: &self.common.basic_rules[..],
        //     causes: &self.warnings,
        // };
        // warn.report_warnings(cx);
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
            rules: ir.common.rules,
            type_map: ir.common.type_map,
            maps: ir.maps,
            errors: ir.common.errors,
            // assert_type_equality: ir.common.hir.assert_type_equality.into_inner(),
            // arguments_from_outer_layer: ir.common.attrs.arguments_from_outer_layer().clone(),
            // invocation_of_inner_layer: ir.common.lower_level,
            lexer_layer: ir.common.lexer_layer,
            attr_arguments: ir.common.attr_arguments,
            trace: ir.common.trace,
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

    pub fn sym_of_external(&self, symbol: Symbol) -> Option<Sym> {
        self.maps.sym_of_external(symbol)
    }

    pub fn sym_map(&self) -> &SymMap {
        self.maps.sym_map()
    }
}
