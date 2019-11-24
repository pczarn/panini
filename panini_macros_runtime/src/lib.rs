//! The runtime library. Bundles all upstream libraries that are required to run parsers. Must be included whenever Panini grammars are declared.

// The toolkit for context-free grammars.
extern crate cfg;
// The main engine.
extern crate gearley;

// Utilities.

// Parsers have code that uses the ref_slice function.
extern crate ref_slice;
// Parsers have code that uses arenas.
extern crate typed_arena;
// Parsers have code that deserializes grammars.
extern crate serde_cbor;

// Run-time tracing.
mod trace;

// Reexport node variants here. This location is nicer.
// TODO do not reexport like that...
pub mod node {
    pub use gearley::forest::depth_first::node::NodeInner::*;
}

//
pub use std::fmt;

// Grammar and recognizer.
pub use gearley::grammar;
pub use gearley::recognizer::item::CompletedItem;
pub use gearley::recognizer::Recognizer;
// Forest.
pub use gearley::forest::depth_first::cartesian_product::CartesianProduct;
pub use gearley::forest::depth_first::{
    ActionClosureEvaluator, ArrayEvaluator, Evaluated, NodeRef, NullOrder, TraversalBottom,
    ValueArray,
};
pub use gearley::forest::{Bocage, Forest, NullForest, Traversal};
// Engine's utilities.
pub use gearley::util::slice_builder::SliceBuilder;
// Grammar symbol type.
pub use cfg::Symbol;
// Typed arena.
pub use typed_arena::Arena;
// fn ref_slice.
pub use ref_slice::ref_slice;
// Shorthand for traversal with default order.
pub type TraversalUnordered<'g, T, V> = Traversal<'g, 'g, 'g, T, V, NullOrder<'g, 'g, T, V>>;
// Run-time tracing in this crate.
pub use self::trace::{print_trace, RuleSource, TraceInfo};

pub use serde_cbor::from_slice as serde_cbor_from_slice;

pub struct StaticInfo {
    serialized_grammar: &'static [u8],
    sym_names: &'static [&'static str],
    trace_info: TraceInfo,
}
