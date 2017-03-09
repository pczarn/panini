macro_rules! memoize (
    ($name:ident = $e:expr) => {
        if let Some(x) = self.$name {
            return x;
        }
        let x = $e;
        self.$name = x;
        x
    }
)

struct IrBuilder {
    stmts: ast::Stmts,
    start: Option<SymbolicName>,
    attrs: Option<Attrs<SymbolicName>>,

}

impl IrBuilder {
    pub fn build_from_stmts(stmts: ast::Stmts) -> Result<Self, TransformationError> {

    }

    fn start(&mut self) -> Result<SymbolicName, TransformationError> {
        Ok(memoize!(start = {
            if let Some(first_stmt) = self.stmts.stmts.get(0) {
                first_stmt.lhs.node
            } else {
                return Err(TransformationError::GrammarIsEmpty);
            }
        }))
        // if let Some(start) = self.start {
        //     return start
        // }
        // // Obtain the explicit start.
        // let start = if let Some(first_stmt) = stmts.stmts.get(0) {
        //     first_stmt.lhs.node
        // } else {
        //     return Err(TransformationError::GrammarIsEmpty);
        // };
        // self.start = Some(start);
        // start
    }

    fn attrs(&mut self) -> &Attrs<SymbolicName> {
        memoize!(attrs = {
            Attrs::compute(&self.stmts.attrs[..])
        })
    }

    fn stmts_with_() {
        memoize!(stmts_with_ = {
            let stmts_with_ = self.stmts.clone();
            if let &Some(ref from_outer) = self.attrs().arguments_from_outer_layer() {
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
                    stmts_with_.stmts.push(lower_start_stmt);
                }
            }
        })
    }

    fn hir_with_names(&mut self) -> Hir {
        memoize!(hir_with_names = {
            Hir::transform_stmts(&ir.stmts)
        })
    }
}
