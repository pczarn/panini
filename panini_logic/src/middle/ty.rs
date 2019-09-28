use std::collections::HashMap;
use std::collections::hash_map;
use std::slice;

use cfg::Symbol;

use input::RustTyId;

//type RustTy = rs::P<rs::Ty>;

#[derive(Clone, Debug)]
pub enum Ty<S = Symbol> {
    RustTy(RustTyId),
    RustTerminalTy,
    Auto(AutoTy<S>),
    SequenceVec(S),
    Infer,
}

#[derive(Clone, Debug)]
pub enum AutoTy<S = Symbol> {
    Tuple {
        fields: Vec<S>,
    },
    Struct {
        members: HashMap<rs::Ident, S>,
    },
}

impl<S> Ty<S> {
    pub fn symbols(&self) -> Symbols<S> {
        match self {
            &Ty::Auto(AutoTy::Tuple { ref fields }) => {
                Symbols::FieldSymbols(fields.iter())
            }
            &Ty::Auto(AutoTy::Struct { ref members }) => {
                Symbols::MemberSymbols(members.values())
            }
            _ => {
                // for sequence??
                Symbols::FieldSymbols([].iter())
            }
        }
    }

    pub fn is_terminal(&self) -> bool {
        if let &Ty::RustTerminalTy = self {
            true
        } else {
            false
        }
    }
}

/// Iterator over symbols included in a type.
pub enum Symbols<'a, S> where S: 'a {
    FieldSymbols(slice::Iter<'a, S>),
    MemberSymbols(hash_map::Values<'a, rs::Ident, S>),
}

impl<'a, S> Iterator for Symbols<'a, S>
    where S: Copy
{
    type Item = S;

    fn next(&mut self) -> Option<S> {
        match self {
            &mut Symbols::FieldSymbols(ref mut iter) => iter.next().cloned(),
            &mut Symbols::MemberSymbols(ref mut iter) => iter.next().cloned(),
        }
    }
}
