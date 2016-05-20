use std::collections::HashMap;

use cfg::Symbol;

use rs;

use middle::attr::Attrs;
use middle::hir;
use middle::lint::*;

pub struct WarningCauses<'a> {
    pub attrs: &'a Attrs<Symbol>,
    pub external_grammar: &'a [(Symbol, Vec<Symbol>)],
    pub hir_rules_with_names: &'a [hir::Rule<rs::Name>],
    pub cycles: &'a [u32],
    pub cycles_among_nullable: &'a [u32],
    pub unproductive_rules: &'a [(u32, u32)],
    pub unreachable_rules: &'a [u32],
}

const UNPRODUCTIVE_RULE: &'static str = "unproductive rule.";
const UNREACHABLE_RULE:  &'static str = "unreachable rule.";
const CYCLE:             &'static str = "cycle among unit rules.";
const CYCLE_NULLABLE:    &'static str = "cycle that includes a nullable rule";
const OVERRULED_LINT:    &'static str = "conflicting lint levels.";
// const INVALID_LINT:      &'static str = "unknown lint.";
// const UNUSED_ATTR:       &'static str = "unused attribute.";


impl<'a> WarningCauses<'a> {
    pub fn report_warnings(&self, cx: &mut rs::ExtCtxt) {
        let mut unproductive_rules = HashMap::new();

        for &(origin, pos) in self.unproductive_rules {
            unproductive_rules.entry(origin).or_insert(vec![]).push(pos);
        }

        for (&origin, positions) in unproductive_rules.iter() {
            let rule = &self.hir_rules_with_names[origin as usize];
            let causes = positions.iter().map(|&pos| rule.rhs[pos as usize].span);
            let span = self.hir_rules_with_names[origin as usize].lhs.span;
            let diag_opt = match self.attrs.get_lint_level(Unproductive) {
                Allow => None,
                Warn => {
                    Some(cx.struct_span_warn(span, UNPRODUCTIVE_RULE))
                }
                Deny | Forbid => {
                    Some(cx.struct_span_err(span, UNPRODUCTIVE_RULE))
                }
            };
            if let Some(mut diag) = diag_opt {
                let multispan = rs::MultiSpan::from_spans(causes.collect());
                let msg = if multispan.primary_spans().len() == 1 {
                    "this symbol is unproductive:"
                } else {
                    "these symbols are unproductive:"
                };
                diag.span_note(multispan, msg);
                diag.emit();
            }
        }

        for &origin in self.unreachable_rules {
            let span = self.hir_rules_with_names[origin as usize].lhs.span;
            match self.attrs.get_lint_level(DeadCode) {
                Allow => {}
                Warn => {
                    cx.span_warn(span, UNREACHABLE_RULE);
                }
                Deny | Forbid => {
                    cx.span_err(span, UNREACHABLE_RULE);
                }
            }
        }

        for &origin in self.cycles {
            let span = self.hir_rules_with_names[origin as usize].lhs.span;
            match self.attrs.get_lint_level(Cycles) {
                Allow => {}
                Warn => {
                    cx.span_warn(span, CYCLE);
                }
                Deny | Forbid => {
                    cx.span_err(span, CYCLE);
                }
            }
        }

        for &origin in self.cycles_among_nullable {
            let span = self.hir_rules_with_names[origin as usize].lhs.span;
            match self.attrs.get_lint_level(Cycles) {
                Allow => {}
                Warn => {
                    cx.span_warn(span, CYCLE_NULLABLE);
                }
                Deny | Forbid => {
                    cx.span_err(span, CYCLE_NULLABLE);
                }
            }
        }

        for &span in &self.attrs.overruled_lint {
            cx.span_err(span, OVERRULED_LINT);
        }

        // Don't warn about unused attrs. Put them on generated code instead...

        // for &span in &self.attrs.invalid_lint {
        //     cx.span_err(span, INVALID_LINT);
        // }

        // for &span in &self.attrs.unused_attrs {
        //     cx.span_warn(span, UNUSED_ATTR);
        // }
    }
}
