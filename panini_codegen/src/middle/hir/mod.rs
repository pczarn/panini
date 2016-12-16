use std::collections::HashMap;
use std::mem;
use std::cell::RefCell;

use cfg::*;

use rs;
use front::{ast, Name};
use front::visit::RhsAstVisitor;
use middle::{ActionExpr, Ty, AutoTy, Rule};
use middle::rule::{PrecedenceLevel, PrecedencedRuleAlternative};
use middle::trace::SourceOrigin;
use middle::embedded_string::EmbeddedString;

pub struct Hir {
    graph: Vec<Node<S>>,
    // lhs_map: HashMap<Handle, S>,
    // pub rules: Vec<Rule<S>>,
    // pub type_map: HashMap<S, Ty<S>>,
    // pub type_equality: Vec<(S, Ty<S>)>,
    // pub embedded_strings: Vec<EmbeddedString<S>>,
}

type Handle = usize;

struct Node {
    item: Item<Symbol>,
    lhs: Symbol,
}

pub enum Item<S> {
    Product {
        pub factors: Vec<Handle>,
    },
    Sum {
        pub summands: Vec<Handle>,
    },
    PrecedencedSum {
        pub summands: Vec<Handle>,
    },
    Sequence {
        pub item: Handle,
        pub min: u32,
        pub max: Option<u32>,
    },
    Bound {
        pub item: Handle,
        pub bind: BoundItem,
    },
    InlineAction {
        pub item: Handle,
        pub action: Action,
    },
    Atom {
        symbol: S,
    },
    Embedded {
        string: EmbeddedString,
    },
}

struct BoundItem {
    handle: Handle,
    bind: BindType,
}

// A rule at a given position
//
// * has either a tuple binding or a named binding
// * may have a deep binding regardless of other bindings
pub struct BindType {
    shallow: ShallowBindType,
    deep: bool,
}

pub enum ShallowBindType {
    Positional,
    Named(rs::ast::Ident),
}

// what happens for a sequence in a precedenced rule?


// type RuleNode = (RuleNode, SourceOrigin, HashMap<usize, rs::Span>);
