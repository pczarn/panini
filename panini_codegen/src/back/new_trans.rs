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
            builder: AstBuilder::new(),
            terminals: vec![],
            unique_names: UniqueNames::new(layer_id),
            null_rules: vec![],
        };
        this.compute_variant_map();
        this
    }

    pub fn generate(&mut self) -> Gen {
        Gen {
            start: self.start(),
            nullary_product_roots: self.nullary_product_roots()
        }
    }

    fn nullary_product_roots(&mut self) -> Vec<NullaryProduct> {
        // Nulling rules
        // are these equal?
        let num_nulling_syms = self.ir.grammar.num_syms();
        let num_all_nulling_syms = self.ir.nulling_grammar.sym_source().num_syms();
        // Declarations
        self.null_rules = (0 .. num_nulling_syms).map(|i| NullaryProducts::new(Symbol::from(i))).collect();
        // let mut null_order = iter::repeat(u32::MAX).take(num_nulling_syms).collect::<Vec<_>>();
        // These vectors may be longer than other vectors.
        let mut null_deps = iter::repeat(0).take(num_all_nulling_syms).collect::<Vec<_>>();
        self.null_intermediate = iter::repeat(None).take(num_all_nulling_syms).collect();
        // Temporary variables.
        let mut null_work = vec![];
        let mut null_num_rules = 0;
        // Here, the name must start with "_" so that we don't get "unnecessary mut"
        // warnings later on. Yes, underscore prefix works for ignoring more than just
        // "unused variable" warnings.
        let continuation_label = rs::gensym_ident("_cont");
        for rule in self.ir.nulling_grammar.rules() {
            // if rule.rhs().len() == 0 {
            //     // Can `origin` be None? In sequences? No.
            //     let origin = rule.history().origin().unwrap() as usize;
            //     // let basic_rule = self.ir.basic_rules.get(origin);
            //     // let action = basic_rule.map(|basic_rule| &basic_rule.action);
            //     // let action_expr = match action {
            //     //     Some(&Action::Tuple { .. }) => {
            //     //         unreachable!("found nulling rule that has a tuple type")
            //     //     }
            //     //     Some(&Action::Struct { expr: ActionExpr::Inline { ref expr }, .. }) => {
            //     //         expr.clone()
            //     //     }
            //     //     // A sequence rule.
            //     //     Some(&Action::Sequence) => {
            //     //         self.builder.expr().call().path().ids(&["Vec", "new"]).build().build()
            //     //     }
            //     //     _ => unreachable!("found unknown action")
            //     // };
            //     // let inner = self.builder.block()
            //     //         .stmt().expr().call().id(continuation_label).with_arg(action_expr)
            //     //         .build()
            //     //     .build();
            //     null_rules[rule.lhs().usize()].push(self.nulling_product(origin, &[]));
            //     null_num[rule.lhs().usize()] += 1;
            //     // if null_order[rule.lhs().usize()] > null_num_rules {
            //     //     null_order[rule.lhs().usize()] = null_num_rules;
            //     //     null_num_rules += 1;
            //     // }
            // } else {
                if let Some(origin) = rule.history().origin() {
                    null_work.push((
                        rule.lhs(),
                        rule.rhs().get(0).cloned(),
                        rule.rhs().get(1).cloned(),
                        origin
                    ));
                    null_deps[rule.lhs().usize()] += 1;
                } else {
                    null_intermediate[rule.lhs().usize()] = Some(rule.rhs().to_owned());
                }
            // }
        }
        // Generate code that uses macros and a continuation-passing style.
        while !null_work.is_empty() {
            null_work.retain(|&(lhs, rhs0, rhs1, origin)| {
                let rhs0_is_done = rhs0.map_or(true, |rhs0| null_deps[rhs0.usize()] == 0);
                let rhs1_is_done = rhs1.map_or(true, |rhs1| null_deps[rhs1.usize()] == 0);
                if !rhs0_is_done || !rhs1_is_done {
                    // Process this later.
                    true
                } else {
                    // There are no sequence rules among nulling rules, so unwrapping is ok.
                    // let (action_expr, patterns) = self.get_action(origin as usize).unwrap();
                    // let mut pats = HashMap::new();
                    // for arg in patterns.into_iter() {
                    //     pats.insert(arg.num, arg.pat);
                    // }
                    let product = match (rhs0, rhs1) {
                        (Some(rhs0), Some(rhs1)) => {
                            self.nullary_product(origin, &[rhs0, rhs1])
                        }
                        (Some(rhs0), _) => {
                            self.nullary_product(origin, &[rhs0])
                        }
                        (_, _) => {
                            self.nullary_product(origin, &[])
                        }
                    };
                    // if let (Some(rhs1Some(rhs1) = rhs1 {
                    //     product = self.nulling_product(&[rhs0, rhs1])
                    // } else {
                    //     product = self.nulling_product(&[rhs0])
                    // }
                    // let mut inner_layer = self.builder.block().stmt().expr()
                    //     .call().id(continuation_label).with_arg(action_expr).build().build();
                    // for (i, &factor) in factors.iter().enumerate().rev() {
                    //     let name = self.lowercase_name(self.ir.externalize(factor));
                    //     let pat = pats.get(&i).cloned()
                    //                           .unwrap_or_else(|| self.builder.pat().wild());
                    //     let fn_decl = self.builder.fn_decl()
                    //             .arg().with_pat(pat).ty().infer()
                    //             .default_return();
                    //     let closure = rs::ast::ExprKind::Closure(
                    //         rs::ast::CaptureBy::Ref, // or Value
                    //         fn_decl,
                    //         inner_layer,
                    //         rs::DUMMY_SP,
                    //     );
                    //     let closure = self.builder.expr().build_expr_kind(closure);
                    //     inner_layer =
                    //         self.builder.block().stmt().build_expr(
                    //                 MacExprBuilder::build()
                    //                     .path(self.builder.path().id(name).build())
                    //                     .expr().build(closure).build()
                    //             ).build();
                    // }
                    self.add_nullary_product(lhs, product);
                    // // what is this order for?
                    // if null_order[lhs.usize()] > null_num_rules {
                    //     null_order[lhs.usize()] = null_num_rules;
                    //     null_num_rules += 1;
                    // }
                    null_deps[lhs.usize()] -= 1;
                    false
                }
            });
            // check if fixpoint is reached?
        }

        // let mut null = null_rules.into_iter().zip(null_num).enumerate().collect::<Vec<_>>();
        // null.sort_by(|&(left, _), &(right, _)| {
        //     // Those that are used by other symbols come first.
        //     null_order[left].cmp(&null_order[right])
        // });

        // let mut rules = vec![];
        // let mut roots = vec![];
        // for (i, (products, num)) in null {
            // let lhs_sym = Symbol::from(i);
            // let external_lhs = self.ir.externalize(lhs_sym);
            // if !blocks.is_empty() {
            //     let ident = self.lowercase_name(external_lhs);
            //     rules.push(GenEpsilonIntermediateRule {
            //         name: ident,
            //         blocks: blocks
            //     });
            // }
            // if num != 0 {
            //     // This nulling forest is not empty.
            //     let ident = self.lowercase_name(external_lhs);
            //     roots.push(GenEpsilonRootAction {
            //         // This symbol must be internal
            //         sym: lhs_sym,
            //         num: num,
            //         name: ident,
            //         variant_name: self.variant_names[&external_lhs],
            //     });
            // }
            if !products.is_empty() {
        //         NullaryProduct {
        //             lhs: lhs_sym,
        //             summands: num,
        //             nullary_products: products
        //         }
        //     }
        // }
        self.null_rules
    }

    fn nullary_product(&self, origin: usize, rhs: &[Symbol]) -> NullaryProduct {
        let factors = self.nullary_product_factors(rhs);
        NullaryProduct {
            basic_rule: self.ir.basic_rules.get(origin).clone().unwrap(),
            factors: factors,
        }
    }

    fn nullary_product_factors(&self) -> Vec<Symbol> {
        let mut factors = vec![];
        let mut factor_stack = rhs.iter().rev().collect();
        while let Some(sym) = factor_stack.pop() {
            if let &Some(ref rhs) = &self.null_intermediate[sym.usize()] {
                factor_stack.extend(rhs.iter().cloned());
            } else {
                factors.push(sym);
            }
        }
        factors
    }

    fn add_nullary_product(&mut self, lhs: Symbol, product: NullaryProduct) {
        self.null_rules[lhs.usize()].summands += product.factors.iter()
            .map(|&factor| self.null_rules[factor.usize()].summands)
            .fold(1, |acc, elem| acc * elem);
        self.null_rules[lhs.usize()].nullary_products.push(product);
    }
}
