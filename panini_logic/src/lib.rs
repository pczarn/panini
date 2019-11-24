#![recursion_limit = "256"]
#![feature(plugin_registrar, rustc_private)]

extern crate log;
extern crate env_logger;

extern crate rustc;
extern crate syntax;
extern crate syntax_pos;

extern crate serde_cbor;

extern crate quote;
extern crate proc_macro2;

extern crate bit_matrix;
extern crate cfg;
extern crate cfg_regex;
extern crate gearley;

extern crate enum_coder;

#[macro_use]
extern crate maplit;

pub mod middle;
pub mod input;
pub mod output;

use middle::ir::Ir;
use middle::error::TransformationError;
use input::ast::Stmts;
use output::instruction::InstructionList;
use output::translator::IrTranslator;

pub fn process(stmts: Stmts) -> Result<InstructionList, TransformationError> {
    match phase_1_lower_to_ir(stmts) {
        Ok(ir) => {
            Ok(phase_2_translate(ir))
        }
        Err(error) => {
            Err(error)
        }
    }
}

fn phase_1_lower_to_ir(stmts: Stmts) -> Result<Ir, TransformationError> {
    middle::ir::IrMapped::transform_from_stmts(stmts).map(|ir_mapped| ir_mapped.into())
}    

fn phase_2_translate(ir: Ir) -> InstructionList {
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
