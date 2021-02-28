#![recursion_limit = "256"]
#![feature(plugin_registrar, rustc_private)]

extern crate log;
extern crate env_logger;

extern crate bit_matrix;
extern crate cfg;
extern crate cfg_regex;
extern crate gearley;

extern crate enum_coder;

#[macro_use]
extern crate maplit;

#[macro_use]
pub mod middle;
pub mod input;
pub mod output;

use middle::ir::Ir;
use middle::error::TransformationError;
use input::InputTree;
use output::instruction::InstructionList;
use output::translator::IrTranslator;

pub fn process(input: InputTree) -> Result<InstructionList, TransformationError> {
    phase_1_lower_to_ir(input).map(phase_2_translate)
}

pub fn phase_1_lower_to_ir(input: InputTree) -> Result<Ir, TransformationError> {
    Ir::transform(input)
}    

pub fn phase_2_translate(ir: Ir) -> InstructionList {
    IrTranslator::new(ir).generate()
}

// fn report_error(ecx: &mut rs::ExtCtxt, sp: rs::Span, error: &TransformationError) {
//     match error {
//         &TransformationError::RecursiveType(ref types) => {
//             for rule in types {
//                 let mut diag = ecx.struct_span_err(rule.lhs.span, error.description());
//                 let cause_spans = rule.causes.iter().map(|c| c.span).collect();
//                 let multispan = rs::MultiSpan::from_spans(cause_spans);
//                 let msg = if multispan.primary_spans().len() == 1 {
//                     "this symbol has a recursive type:"
//                 } else {
//                     "these symbols have recursive types:"
//                 };
//                 diag.span_note(multispan, msg);
//                 diag.emit();
//             }
//         }
//         _ => {
//             ecx.span_err(sp, error.description());
//             ecx.parse_sess.span_diagnostic.abort_if_errors();
//             panic!();
//         }
//     }
// }
