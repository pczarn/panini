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
    pub fn transform_stmts(stmts: &ast::Stmts) -> Self {
        let mut hir_builder = HirBuilder {
            graph: vec![],
            results: vec![],
            rhs_counter: 0,
            cur_rule_pos: 0,
        };
        for stmt in &stmts.stmts[..] {
            hir_builder.transform_stmt(stmt);
        }
        hir_builder.into_hir()
    }

    pub fn transform_stmt(&mut self, stmt: &ast::Stmt) {
        let mut levels_rules = vec![];
        for level in &stmt.rhs {
            let mut rules_for_level = vec![];
            for &(ref rhs, ref action) in level {
                let rule = self.transform_rhs(rhs);
                if let Some(ref inline_action) = action.expr {
                    rule.0 = this.create(InlineAction {
                        expr: rule.0,
                        action: inline_action.clone(),
                    });
                }
                rules_for_level.push(rule);
            }
            let idx = self.create(Sum {
                summands: rules_for_level,
            });
            levels_rules.push(idx);
        }
        let idx;
        match levels_rules.len() {
            0 => {
                unreachable!()
            }
            1 => {
                idx = levels_rules[0];
            }
            _ => {
                // Declare a precedenced rule
                idx = self.create(PrecedencedSum {
                    summands: levels_rules
                });
            }
        }
        self.lhs_map.entry(stmt.lhs).or_insert(vec![]).push(idx);
        self.rhs_counter += 1;
    }

    fn transform_rhs(&mut self, rhs: &ast::Rhs) -> ResultItem {
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

    fn create_node_and_result(&mut self, node: HirItem) {
        let bind_deep = match node {
            Alternative { .. } | Rhs { .. } => {
                true
            }
            _ => {
                false
            }
        };
        let bind = BindType {
            shallow: Ignored,
            deep: bind_deep,
        }
        this.results.push(this.create_node(node));
        this.create_node(node);
    }

    fn create_node(&mut self, node: HirItem) -> HirHandle {
        self.graph.push(node);
        self.graph.len() - 1
    }
}

// Flattening the RHS AST

impl RhsAstVisitor for HirBuilder {
    // Reversed order of visitation.
    fn walk_rhs_element(&mut self, rhs_elem: &ast::RhsElement) {
        self.visit_rhs_ast(&rhs_elem.elem);
        // Binds must be visited later, because they access the rule that was
        // constructed before.
        self.visit_bind(&rhs_elem.bind);
    }

    // The following push one result

    fn visit_rhs_symbol(&mut self, symbol: Name) {
        self.push_result(Atom {
            symbol: symbol
        });
        self.cur_rule_pos += 1;
    }

    fn visit_sequence(&mut self, sequence: &ast::Sequence) {
        let mut source_origin = SourceOrigin {
            rule_id: self.rule_id,
            rule_pos: vec![self.cur_rule_pos],
        };
        self.walk_sequence(sequence);
        // the last result_node is either a product or an alternative etc.
        source_origin.rule_pos.push(self.cur_rule_pos);
        self.push_result(Sequence {
            rhs: self.pop_result(),
            min: sequence.min,
            max: sequence.max,
            source_origin: source_origin,
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
        self.push_result(Sum {
            summands: this.pop_n_results(sum.len()),
        })
    }

    fn visit_product(&mut self, product: &ast::Rhs) {
        self.cur_rule_pos += 1;
        let prev_num_results = self.results.len();
        self.walk_product(product);
        self.cur_rule_pos += 1;
        let num_results = self.results.len() - prev_num_results;
        self.push_result(Product {
            factors: self.pop_n_results(num_results),
        });
    }

    fn visit_rhs_string(&mut self, string: Name) {
        self.push_result(Embedded {
            string: string,
            // source_origin: ... vec![self.cur_rule_pos, self.cur_rule_pos + 1],
        });
        self.cur_rule_pos += 1;
    }

    // Bind

    fn visit_bind(&mut self, bind: &Option<rs::P<rs::Pat>>) {
        let current_result = self.results.last_mut().unwrap();
        let shallow_bind_type = if let &Some(ref pat) = bind {
            match &pat.node {
                &rs::PatKind::Wild => {
                    Ignored
                }
                &rs::PatKind::Ident(_mode, spanned_ident, None) => {
                    Named(spanned_ident.node)
                }
                _ => panic!("unsupported pattern"),
            }
        } else {
            Positional
        };
        current_result.1.shallow = shallow_bind_type;
    }
}
