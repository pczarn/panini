use cfg::Symbol;
use middle::Ir;

// Lowered IR.

pub struct Lir {
    // The grammar
    pub grammar: BinarizedGrammar,

    // Properties of the start symbol
    pub start_variant: rs::ast::Ident,
    pub start_type: GenType,

    // For actions
    pub epsilon_actions: EpsilonActions,
    pub actions: Vec<Action>,
    pub sequence_actions: Vec<SequenceAction>,

    // For terminals
    pub terminal_names: Vec<rs::ast::Ident>,
    pub terminal_ids: Vec<usize>,

    // For passing evaluated pieces to the outer layer, when the outer layer's
    // terminals are parsed.
    pub arguments_from_outer_layer: Option<LoweredArgumentsFromOuterLayer>,

    // For declaring the inner layer
    pub inner_layer: Option<LoweredInvocationOfInnerLayer>,
    pub inner_layer_level: u32,

    // For tracing
    pub trace_rule_ids: Vec<u32>,
    pub trace_rule_pos: Vec<Vec<u32>>,
    pub trace_tokens: Vec<Vec<String>>,

    // Names of type parameters to be inferred: I0, I1, I2, etc.
    pub infer: Vec<rs::ast::Ident>,
}

// Epsilon rules

pub struct EpsilonActions {
    pub rules: Vec<EpsilonIntermediateRule>,
    pub roots: Vec<EpsilonRootAction>,
}

#[derive(Debug)]
pub struct EpsilonIntermediateRule {
    // Expr and patterns will be extracted from the action and patterns of the
    // original rule.
    pub summands: Vec<EpsilonIntermediateExpr>,
    // The name of the macro rule that will be generated.
    pub name: rs::ast::Ident,
}

#[derive(Debug)]
pub struct EpsilonRootAction {
    // This symbol must be internal
    pub sym: Symbol,
    pub num: usize,
    pub name: rs::ast::Ident,
    // redundant?
    pub variant_name: rs::ast::Ident,
}

// Rules

pub struct Action {
    // The action will the extracted later.
    pub original: usize,
    pub variant: rs::ast::Ident,
}

// Sequcene rules

pub struct SequenceAction {
    pub original: usize,
    pub elem_variant: rs::ast::Ident,
    pub variant: rs::ast::Ident,
}

// Code for an epsilon rule can be computed from this data.
struct EpsilonIntermediateExpr {
    original: usize,
    factors: Vec<Symbol>,
}

impl Lir {
    fn generate(ir: &Ir) -> Self {
        let null = Lir::generate_epsilon(ir);

        let internal_grammar = InternalGrammar::from_processed_grammar_with_maps(
            self.ir.grammar.clone(),
            Mapping::new(0),
            self.ir.nulling_grammar.clone(),
        );

        let mut processed_rule = vec![];
        let mut processed_sequences = vec![];

        let mut seen_origin = HashSet::new();
        // For generating actions.
        let rules_with_external_origin = self.ir.grammar.rules().filter_map(|rule| {
            if let Some(origin) = rule.history().origin() {
                if seen_origin.insert(origin) {
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
            let original = origin as usize;
            // Translate to external.
            let rule_lhs = rule.lhs();
            let variant = self.variant_names[&rule_lhs];

            // Detect sequence rules
            match ir.basic_rules[origin]
            if let Some((rust_expr, patterns)) = self.get_action(original) {
                processed_rule.push(Rule {
                    original: original,
                    variant: variant,
                });
            } else {
                let basic_rule = &ir.basic_rules[origin as usize];
                let variant2 = self.variant_names[&basic_rule.lhs.node];
                assert_eq!(variant, variant2);
                processed_sequences.push(Sequence {
                    original: original,
                    variant: variant,
                    elem_variant: self.variant_names[&basic_rule.rhs[0].node],
                });
            }
        }

        Lir {
            null: null,
            rules: processed_rule,
            sequences: processed_sequences,
        }
    }

    fn generate_epsilon(ir: &Ir) -> Epsilon {
        // Nulling rules
        // are these external?
        let num_nulling_syms = self.ir.grammar.num_syms();
        // Declarations
        let mut null = Null::new_with_syms(num_nulling_syms);
        // let mut null_order = iter::repeat(u32::MAX).take(num_nulling_syms).collect::<Vec<_>>();
        // Temporary variables.
        let mut null_work = vec![];
        // let mut null_num_rules = 0;
        // Here, the name must start with "_" so that we don't get "unnecessary mut"
        // warnings later on. Yes, underscore prefix works for ignoring more than just
        // "unused variable" warnings.
        let continuation_label = rs::gensym_ident("_cont");
        for rule in self.ir.nulling_grammar.rules() {
            let this_rule = null.get(rule.lhs());
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
                this_rule.originals.push(origin);
                this_rule.num_trees += 1;
                // if null_order[rule.lhs().usize()] > null_num_rules {
                //     null_order[rule.lhs().usize()] = null_num_rules;
                //     null_num_rules += 1;
                // }
            } else {
                if rule.history().origin().is_none() {
                    this_rule.intermediate = Some(rule.rhs().to_owned());
                }
                null_work.push((
                    rule.lhs(),
                    rule.rhs()[0],
                    rule.rhs().get(1).cloned(),
                    rule.history.origin()
                ));
                this_rule.num_deps += 1;
            }
        }
        // Generate code that uses macros and a continuation-passing style.
        while !null_work.is_empty() {
            null_work.retain(|work| {
                let rhs1_is_done = work.rhs1.map_or(true, |rhs1| null.get(work.rhs1).num_deps == 0);
                let rhs0_is_done = null.get(work.rhs0).num_deps == 0;
                if !rhs0_is_done || !rhs1_is_done {
                    // Process this later.
                    true
                } else {
                    // Can process this now.
                    null.process_epsilon_rule(work);
                    null.get(lhs).num_deps -= 1;
                    false
                }
            });
            // check if fixpoint is reached?
        }

        let mut null_rules = null.info.into_iter().enumerate().collect::<Vec<_>>();
        // null.sort_by(|&(left, _), &(right, _)| {
        //     // Those that are used by other symbols come first.
        //     null_order[left].cmp(&null_order[right])
        // });

        let mut intermediate_rules = vec![];
        let mut roots = vec![];
        for (i, rule) in null {
            let lhs_sym = Symbol::from(i);
            let external_lhs = self.ir.externalize(lhs_sym);
            if !blocks.is_empty() {
                let ident = self.lowercase_name(external_lhs);
                intermediate_rules.push(EpsilonIntermediateRule {
                    name: ident,
                    actions: rule.actions,
                });
            }
            if num != 0 {
                // This nulling forest is not empty.
                let ident = self.lowercase_name(external_lhs);
                roots.push(EpsilonRootAction {
                    // This symbol must be internal
                    sym: lhs_sym,
                    num: num,
                    name: ident,
                    variant_name: self.variant_names[&external_lhs],
                });
            }
        }
        Epsilon {
            rules: intermediate_rules,
            roots: roots,
        }
    }

    fn lowercase_name(&self, sym: Symbol) -> rs::ast::Ident {
        let rs_name = self.ir.name_of_external(sym).unwrap();
        let mut name = rs_name.as_str().to_string();
        write!(name, "_{}", rs_name.0).unwrap();
        rs::str_to_ident(&name[..])
    }
}

struct Null {
    info: Vec<NullInfo>
}

struct NullInfo {
    actions: Vec<EpsilonAction>,
    num_trees: u32,
    num_deps: u32,
    intermediate: Vec<Symbol>,
}

impl Null {
    pub fn new_with_syms(num_syms: usize) -> Self {
        Null {
            info: iter::repeat(NullInfo::new()).take().collect()
        }
    }

    pub fn get(&mut self, sym: Symbol) -> &mut NullInfo {
        &mut self.info[sym.usize()]
    }

    pub fn process_epsilon_rule(&mut self, work: NullWork) {
        if let Some(original) = work.action {
            // There are no sequence rules among nulling rules, so unwrapping is ok.
            let mut factors = vec![];
            let mut factor_stack = vec![];
            if let Some(rhs1) = work.rhs1 {
                factor_stack.push(work.rhs1);
            }
            factor_stack.push(work.rhs0);
            while let Some(sym) = factor_stack.pop() {
                if let &Some(ref rhs) = &null.get(sym).intermediate {
                    factor_stack.extend(rhs.iter().cloned());
                } else {
                    factors.push(sym);
                }
            }
            self.get(lhs).actions.push(EpsilonAction {
                original: original,
                factors: factors,
            });
            self.get(lhs).num_trees += factors.iter().fold(1, |acc, &factor| {
                acc * self.get(factor).num_trees
            });
            // what was this order for? Useless?
            // if null_order[lhs.usize()] > null_num_rules {
            //     null.get(lhs).order = null_num_rules;
            //     null_num_rules += 1;
            // }
        }
    }
}

impl NullInfo {
    pub fn new() -> Self {
        NullInfo {
            actions: vec![],
            num_trees: 0,
            num_deps: 0,
            intermediate: None,
        }
    }
}

// gen code

                    let (action_expr, patterns) = self.get_action(origin as usize).unwrap();
                    let mut pats = HashMap::new();
                    for arg in patterns.into_iter() {
                        pats.insert(arg.num, arg.pat);
                    }
                    let mut inner_layer = self.builder.block().stmt().expr()
                        .call().id(continuation_label).with_arg(action_expr).build().build();
                    for (i, &factor) in factors.iter().enumerate().rev() {
                        let name = self.lowercase_name(self.ir.externalize(factor));
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
