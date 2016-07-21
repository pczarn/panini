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
pub use gearley::recognizer::Recognizer;
pub use gearley::recognizer::item::CompletedItem;
// Forest.
pub use gearley::forest::{Forest, Bocage, Traversal, NullForest};
pub use gearley::forest::depth_first::cartesian_product::CartesianProduct;
pub use gearley::forest::depth_first::{
    NullOrder,
    ArrayEvaluator,
    ValueArray,
    ActionClosureEvaluator,
    NodeRef,
    TraversalBottom,
    Evaluated
};
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
pub use self::trace::{TraceInfo, RuleSource, print_trace};
