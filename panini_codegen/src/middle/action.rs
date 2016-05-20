use rs;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ActionExpr {
    // Builds a tuple or struct
    Auto,
    // Action
    Inline {
        expr: rs::P<rs::Expr>,
    },
}

impl ActionExpr {
    pub fn is_inline(&self) -> bool {
        *self != ActionExpr::Auto
    }
}

#[derive(Clone, Debug)]
pub enum Action {
    Tuple {
        tuple_binds: Vec<usize>,
    },
    Struct {
        deep_binds: Vec<usize>,
        shallow_binds: Vec<(usize, rs::ast::Ident)>,
        expr: ActionExpr,
    },
}
