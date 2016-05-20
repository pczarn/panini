extern crate cfg;
extern crate gearley;
extern crate ref_slice;
extern crate typed_arena;

pub mod node {
    pub use gearley::forest::depth_first::node::NodeInner::*;
}

pub use std::fmt;

pub use gearley::item::CompletedItem;
pub use gearley::grammar;
pub use gearley::recognizer::Recognizer;

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
pub use gearley::util::slice_builder::SliceBuilder;

pub use cfg::Symbol;

pub use typed_arena::Arena;

pub use ref_slice::ref_slice;

use std::collections::BTreeMap;

pub type TraversalUnordered<'g, T, V> = Traversal<'g, 'g, 'g, T, V, NullOrder<'g, 'g, T, V>>;

#[derive(Copy, Clone)]
pub struct TraceInfo {
    pub ids: &'static [u32],
    pub map: &'static [&'static [u32]],
    pub tokens: &'static [&'static [&'static str]],
}

struct TraceItem {
    lhs: &'static str,
    print: String,
    origins: Vec<usize>,
}

pub fn print_trace<'f, F>(
    recognizer: &Recognizer<'f, 'f, F>,
    completed: &[CompletedItem<F::NodeRef>],
    trace_info: TraceInfo)
    where F: Forest<'f>
{
    // external tracing.
    let mut external = BTreeMap::new();
    let mut external_eis = vec![];
    let rightmost_dot_trace = recognizer.grammar().trace()[2];
    let trace_of_completed = completed.iter().filter_map(|completed_item| {
        rightmost_dot_trace[completed_item.dot as usize].map(|(rule_idx, rhs_idx)| {
            let origin = completed_item.origin as usize;
            (origin, rule_idx as usize, rhs_idx as usize)
        })
    });
    let trace_of_other_items = recognizer.trace().map(|((rule_idx, rhs_idx), origin)| {
        (origin, rule_idx as usize, rhs_idx as usize)
    });
    for (origin, rule_idx, rhs_idx) in trace_of_completed.chain(trace_of_other_items) {
        let original_rule_idx = trace_info.ids[rule_idx];
        let original_rhs_idx = trace_info.map[rule_idx][rhs_idx];
        external.entry(original_rule_idx).or_insert(vec![]).push((original_rhs_idx, origin));
    }
    let mut rhs = BTreeMap::new();
    let mut rhs_dedup = BTreeMap::new();
    for (&rule_idx, rhs_indices) in external.iter() {
        for &(rhs_idx, origin) in &rhs_indices[..] {
            rhs.entry(origin).or_insert(vec![]).push(rhs_idx);
        }
        for (&origin, ref mut rhs_indices) in &mut rhs {
            rhs_indices.sort();
            rhs_indices.dedup();
            rhs_dedup.entry(rhs_indices.clone()).or_insert(vec![]).push(origin);
        }
        for (rhs_indices, origins) in &rhs_dedup {
            // rhs_indices are sorted.
            let mut rhs_indices_iter = rhs_indices.iter();
            let mut print = String::new();

            let lhs_token = trace_info.tokens[rule_idx as usize][0];
            let rhs_trace_tokens = &trace_info.tokens[rule_idx as usize][1..];
            for (i, &token) in rhs_trace_tokens.iter().enumerate() {
                let i = i as u32;
                if rhs_indices_iter.as_slice().get(0) == Some(&i) {
                    print.push_str(" • ");
                    rhs_indices_iter.next();
                } else {
                    print.push_str(" ");
                }
                print.push_str(token);
            }
            let rhs_tokens_len = rhs_trace_tokens.len() as u32;
            if rhs_indices_iter.as_slice().get(0) == Some(&rhs_tokens_len) {
                print.push_str(" •");
            }

            external_eis.push(TraceItem {
                lhs: lhs_token,
                print: print,
                origins: origins.clone(),
            });
        }
        rhs.clear();
        rhs_dedup.clear();
    }
    println!("recognizer.earleme == {}", recognizer.earleme());
    println!("====================================");
    let lhs_len = external_eis.iter().map(|trace_item| trace_item.lhs.len()).max().unwrap_or(0);
    let rhs_len = external_eis.iter().map(|trace_item| trace_item.print.len()).max().unwrap_or(0);
    for item in &external_eis {
        println!("{:lhs_len$}: {:rhs_len$} ({})",
                            item.lhs,
                            &item.print,
                            item.origins.iter().map(|o| o.to_string())
                                               .collect::<Vec<_>>()
                                               .join(", "),
                            lhs_len = lhs_len,
                            rhs_len = rhs_len)
    }
}
