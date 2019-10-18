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

// #[path = "middle/ecs.rs"]
// mod ecs;

// pub mod front;
pub mod middle;
// pub mod back;
pub mod input;
pub mod output;

// pub use front::lexer;

// use std::error::Error;

use input::ast::Stmts;
// use output::Instruction;

pub fn lower(_stmts: Stmts) {
    unimplemented!()
}

// pub fn lower(stmts: Stmts) -> Result<Vec<Instruction>, TransformationError> {
//     match phase_1_lower_to_ir(stmts) {
//         Ok(ir) => {
//             phase_2_translate(ir.into())
//         }
//         Err(error) => {
//             Err(error)
//         }
//     }
// }

// fn phase_1_lower_to_ir(stmts: Stmts) -> Result<IrMapped, TransformationError> {
//     middle::ir::IrMapped::transform_from_stmts(stmts)
// }    

// fn phase_2_translate(ir: Ir) -> Vec<Instruction> {
//     // ir.report_warnings(ecx);
//     // if let Some(errors) = ir.get_errors() {
//     //     Err(errors)
//     // }
//     back::IrTranslator::new(ir).generate()
// }

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
