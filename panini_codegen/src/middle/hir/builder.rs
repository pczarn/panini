// how else name this?
pub type SymbolicName = rs::Name;
pub type SpannedSymbolicName = rs::Spanned<rs::Name>;

pub struct HirBuilder {
    graph: Vec<HirItem<SymbolicName>>,
    lhs_map: HashMap<S, Vec<HirHandle>>,
    results: Vec<HirHandle>,
    rhs_counter: usize,
    cur_rule_pos: usize,
}

impl HirBuilder {
    pub fn new() -> Self {
        HirBuilder {
            graph: vec![],
            results: vec![],
            rhs_counter: 0,
            cur_rule_pos: 0,
        }
    }

    pub fn transform_stmts(&mut self, stmts: &ast::Stmts) -> Self {
        for stmt in &stmts.stmts[..] {
            self.transform_stmt(stmt);
        }
    }

    pub fn transform_stmt(&mut self, stmt: &ast::Stmt) {
        let level_sums = self.transform_stmt_rhs(&stmt.rhs);
        let handle = self.sums_to_item(sums);
        self.lhs_map.entry(stmt.lhs).or_insert(vec![]).push(idx);
        self.rhs_counter += 1;
    }

    fn transform_stmt_rhs_to_sums(&mut self, rhs: &PrecedencedLevels) -> Vec<Handle> {
        let mut level_sums = vec![];
        for level in &stmt.rhs {
            let mut rules_for_level = vec![];
            for rhs_with_action in level {
                let rule = self.transform_rhs_with_action(rhs_with_action);
                rules_for_level.push(rule);
            }
            let idx = self.create_item(Sum {
                summands: rules_for_level,
            });
            level_sums.push(idx);
        }
    }

    fn sums_to_item(&mut self, level_sums: Vec<Handle>) -> Handle {
        match level_sums.len() {
            0 => {
                unreachable!()
            }
            1 => {
                levels_rules[0]
            }
            _ => {
                // Declare a precedenced rule
                self.create_item(PrecedencedSum {
                    summands: levels_rules
                })
            }
        }
    }

    fn transform_rhs_with_action(&mut self, rhs_with_action: &RhsWithAction) -> Handle {
        let &(ref rhs, ref action) = rhs_with_action;
        if let Some(ref inline_action) = action.expr {
            this.create_item(InlineAction {
                item: this.transform_rhs(rhs),
                action: inline_action.clone(),
            })
        } else {
            this.transform_rhs(rhs)
        }
    }

    fn transform_rhs(&mut self, rhs: &ast::Rhs) -> Handle {
        self.visit_rhs(rhs);
        assert_eq!(self.results.len(), 1, "only one remaining rule expected");
        self.results.pop().expect("one remaining rule expected")
    }

    fn into_hir(self, name_source: NameSource) -> Hir {
        let mut rules = self.graph.enumerate().map(|(i, item)| {
            let symbol;
            if let Some(sym) = self.lhs_map.get(&i).cloned() {
                symbol = sym;
            } else {
                symbol = name_source.name();
            }
            Node {
                item: item,
                lhs: symbol,
            }
        }).collect();
        Hir {
            graph: graph,

            // type_map:

            //     rules: vec![],
            //     type_map: HashMap::new(),
            //     type_equality: vec![],
            //     assert_type_equality: RefCell::new(vec![]),
            //     embedded_strings: vec![],
        }
    }

    fn create_item_and_result(&mut self, node: HirItem) {
        this.results.push(this.create_item(node));
    }

    fn create_item(&mut self, node: HirItem) -> HirHandle {
        self.graph.push(node);
        self.graph.len() - 1
    }
}

// Flattening the RHS AST

impl RhsAstVisitor for HirBuilder {
    // Reversed order of visitation.
    fn walk_rhs_element(&mut self, rhs_elem: &ast::RhsElement) {
        self.visit_rhs_ast(&rhs_elem.elem);
        // Binds must be visited later, because they access the item that was
        // constructed as a result of a visit to the rhs ast.
        self.visit_bind(&rhs_elem.bind);
    }

    // The following push one result

    fn visit_rhs_symbol(&mut self, symbol: Name) {
        self.create_item_and_result(Atom {
            symbol: symbol,
            source_origin: SourceOrigin {
                rule_id: self.rule_id,
                rule_pos: vec![self.cur_rule_pos],
            },
        });
        self.cur_rule_pos += 1;
    }

    fn visit_sequence(&mut self, sequence: &ast::Sequence) {
        self.walk_sequence(sequence);
        // the last result_node is either a product or an alternative etc.
        self.create_item_and_result(Sequence {
            rhs: self.pop_result(),
            min: sequence.min,
            max: sequence.max,
        });
        self.cur_rule_pos += 1;
    }

    fn visit_sum(&mut self, sum: &[ast::Rhs]) {
        for rule in sum {
            self.visit_rhs(rule);
            self.cur_rule_pos += 1;
        }
        if !sum.is_empty() {
            self.cur_rule_pos -= 1;
        }
        self.create_item_and_result(Sum {
            summands: this.pop_n_results(sum.len()),
        })
    }

    fn visit_product(&mut self, product: &ast::Rhs) {
        self.cur_rule_pos += 1;
        let prev_num_results = self.results.len();
        self.walk_product(product);
        self.cur_rule_pos += 1;
        let num_results = self.results.len() - prev_num_results;
        self.create_item_and_result(Product {
            factors: self.pop_n_results(num_results),
        });
    }

    fn visit_rhs_string(&mut self, string: Name) {
        self.create_item_and_result(Embedded {
            string: string,
            source_origin: SourceOrigin {
                rule_id: self.rule_id,
                rule_pos: vec![self.cur_rule_pos],
            },
        });
        self.cur_rule_pos += 1;
    }

    // Bind

    fn visit_bind(&mut self, pattern: &Option<rs::P<rs::Pat>>) {
        let last_result = self.results.pop;
        self.create_item_and_result(Bound {
            item: last_result,
            bind: BindType::from_pattern(pattern),
        })
    }
}
