use std::collections::BTreeMap;

use gearley::forest::Forest;
use gearley::grammar::ExternalDot;
use gearley::recognizer::item::{CompletedItem, Origin};
use gearley::recognizer::Recognizer;

/// Trace can be printed at every step of the parse. It shows the set of
/// items, which are grouped by grammar location.
/// Items are printed to stdout.
pub fn print_trace<'f, F>(
    recognizer: &Recognizer<'f, 'f, F>,
    completed_item_set: &[CompletedItem<F::NodeRef>],
    trace_info: TraceInfo,
) where
    F: Forest<'f>,
{
    // The externalized table is keyed by rule index. It contains points for
    // the rule's rhs in the external grammar space.
    let external_set = externalize_set(recognizer, completed_item_set, trace_info);
    // Rearrange the table into trace items.
    let items = items_from_set(&external_set, trace_info);
    //
    print_header(recognizer);
    // Print trace items.
    print_items(&items[..]);
}

// Steps of making a trace.

// Externalize the recognizer's current Earley set to prepare for tracing with
// external items.
fn externalize_set<'f, F>(
    recognizer: &Recognizer<'f, 'f, F>,
    completed_item_set: &[CompletedItem<F::NodeRef>],
    trace_info: TraceInfo,
) -> EarleySetExternal
where
    F: Forest<'f>,
{
    // Collect points to external_set.
    let mut externalized_set = BTreeMap::new();
    // external tracing.
    let rightmost_edge_trace = recognizer.grammar().trace()[2];
    // Include completed items in the trace.
    let trace_of_completed = completed_item_set.iter().filter_map(|completed_item| {
        rightmost_edge_trace[completed_item.dot as usize].map(|dot| (dot, completed_item.origin))
    });
    // Include all non-completed items in the trace.
    let trace_of_other_items = recognizer.trace();
    for (dot, origin) in trace_of_completed.chain(trace_of_other_items) {
        let original = trace_info.source_dot_from_external_dot(dot);
        externalized_set
            .entry(original.rule)
            .or_insert(vec![])
            .push((original.pos, origin));
    }
    externalized_set
}

// Take an externalized set, and return a list of items ready for display.
fn items_from_set(externalized_set: &EarleySetExternal, trace_info: TraceInfo) -> Vec<TraceItem> {
    let mut items = vec![];
    // A tool for rearranging the points for each rule.
    let mut transform = TransformRhsPoints::new();
    for (&rule_idx, rhs_points) in externalized_set.iter() {
        // Put in rule index and points.
        transform.points(rhs_points);
        // We want to rearrange points into groups for better readability.
        //
        // We obtain a set of set pairs. The union of cartesian products for
        // each of these groups is equivalent to the initial point set.
        //
        // Get transformed point sets.
        for (dot_positions, origins) in transform.point_sets_by_positions() {
            // dot_positions were already sorted before.
            let rule_text = trace_info.rule_source(rule_idx);
            let rhs = rule_text.display_with_dots(&dot_positions[..]);
            items.push(TraceItem {
                lhs: rule_text.lhs(),
                print: rhs,
                origins: origins.clone(),
            });
        }
    }
    items
}

// Display a header for the current location. Write to STDOUT.
fn print_header<'f, F>(recognizer: &Recognizer<'f, 'f, F>)
where
    F: Forest<'f>,
{
    println!("recognizer.earleme == {}", recognizer.earleme());
    println!("====================================");
}

// Display a list of items. Write to STDOUT.
fn print_items(items: &[TraceItem]) {
    let lhs_width = items
        .iter()
        .map(|trace_item| trace_item.lhs.len())
        .max()
        .unwrap_or(0);
    let rhs_width = items
        .iter()
        .map(|trace_item| trace_item.print.len())
        .max()
        .unwrap_or(0);
    for item in items {
        println!(
            "{:lhs_width$}: {:rhs_width$} ({})",
            item.lhs,
            &item.print,
            item.origins
                .iter()
                .map(|o| o.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            lhs_width = lhs_width,
            rhs_width = rhs_width
        )
    }
}

// Use these for printing a trace.

pub type Rule = u32;
type RulePos = u32;
type RulePosWithOrigin = (RulePos, Origin);
type EarleySetExternal = BTreeMap<Rule, Vec<RulePosWithOrigin>>;
pub type RhsSourceMap = &'static [u32];
pub type RhsSource = &'static [&'static str];

/// Constant information provided by the parser generator.
#[derive(Copy, Clone)]
pub struct TraceInfo {
    pub ids: &'static [Rule],
    pub map: &'static [RhsSourceMap],
    pub tokens: &'static [&'static [&'static str]],
}

/// Trace consists of these items. They are the result of processing .
struct TraceItem {
    lhs: &'static str,
    print: String,
    origins: Vec<Origin>,
}

/// A place in the original source grammar.
struct SourceDot {
    rule: u32,
    pos: u32,
}

// Textual parts of a grammar rule.
#[derive(Copy, Clone)]
pub struct RuleSource {
    source: &'static [&'static str],
}

/// A tool for rearranging the points for each rule.
#[derive(Default)]
struct TransformRhsPoints {
    // Second step.
    points_by_origin: BTreeMap<Origin, Vec<Rule>>,
    // Final step.
    // This table is an inversion of the one above.
    point_sets_by_positions: BTreeMap<Vec<Rule>, Vec<Origin>>,
}

impl TraceInfo {
    /// Returns an external dot given an internal dot.
    fn source_dot_from_external_dot(&self, dot: ExternalDot) -> SourceDot {
        SourceDot {
            rule: self.ids[dot.rule as usize],
            pos: self.map[dot.rule as usize][dot.pos as usize],
        }
    }

    /// Gets the textual representation of a rule.
    fn rule_source(&self, rule_idx: Rule) -> RuleSource {
        RuleSource {
            source: self.tokens[rule_idx as usize],
        }
    }
}

impl TransformRhsPoints {
    fn new() -> Self {
        Self::default()
    }

    fn points(&mut self, points: &[RulePosWithOrigin]) {
        self.points_by_origin.clear();
        self.point_sets_by_positions.clear();
        // First step.
        for &(pos, origin) in &points[..] {
            self.points_by_origin
                .entry(origin)
                .or_insert(vec![])
                .push(pos);
        }
        // Second step.
        for (&origin, ref mut rhs_indices) in &mut self.points_by_origin {
            rhs_indices.sort();
            rhs_indices.dedup();
            self.point_sets_by_positions
                .entry(rhs_indices.clone())
                .or_insert(vec![])
                .push(origin);
        }
    }

    fn point_sets_by_positions(&self) -> &BTreeMap<Vec<RulePos>, Vec<Origin>> {
        &self.point_sets_by_positions
    }
}

impl RuleSource {
    fn display_with_dots(self, dot_positions: &[u32]) -> String {
        // rhs_positions are sorted.
        let mut rhs_positions_iter = dot_positions.iter();
        let mut text = String::new();

        for (i, &token) in self.rhs().iter().enumerate() {
            let i = i as u32;
            if rhs_positions_iter.as_slice().get(0) == Some(&i) {
                text.push_str(" • ");
                rhs_positions_iter.next();
            } else {
                text.push_str(" ");
            }
            text.push_str(token);
        }
        let rhs_tokens_len = self.rhs().len() as u32;
        if rhs_positions_iter.as_slice().get(0) == Some(&rhs_tokens_len) {
            text.push_str(" •");
        }
        text
    }

    fn lhs(self) -> &'static str {
        &self.source[0]
    }

    fn rhs(self) -> RhsSource {
        &self.source[1..]
    }
}
