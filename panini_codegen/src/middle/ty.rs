use std::collections::HashMap;

use rs;

//type RustTy = rs::P<rs::Ty>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Ty<S> {
    RustTy(rs::P<rs::Ty>),
    Auto(AutoTy<S>),
    SequenceVec(S),
    Infer,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AutoTy<S> {
    Tuple {
        fields: Vec<S>,
    },
    Struct {
        members: HashMap<rs::ast::Ident, S>,
    },
}
