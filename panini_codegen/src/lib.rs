#![recursion_limit = "256"]
#![feature(plugin_registrar, rustc_private, extern_prelude)]

#[macro_use]
extern crate log;
extern crate env_logger;

extern crate rustc;
extern crate syntax;
extern crate syntax_pos;

extern crate serde_cbor;

#[macro_use]
extern crate quote;
extern crate proc_macro2;

extern crate bit_matrix;
extern crate cfg;
extern crate cfg_regex;
extern crate enum_coder;
extern crate gearley;

extern crate panini_logic;

// #[path = "middle/ecs.rs"]
// mod ecs;

pub mod input;
pub mod middle;
pub mod output;
pub mod rs;

pub use panini_logic::input::attr_arguments;

use std::error::Error;

use panini_logic::input::ast as logic_ast;
use panini_logic::middle;
use panini_logic::middle::error::TransformationError;

use { input, middle, output };

pub fn lower(stmts: ast::Stmts) -> Result<rs::TokenStream, TransformationError> {
    let (tables, logic_stmts) = phase_1_lower_stmts(stmts);
    let ir = phase_2_lower_to_ir(stmts)?;
    let instructions = phase_3_translate(&ir);
    let lower_instructions = phase_4_lower_instructions(&ir, &tables, instructions);
    Ok(phase_5_generate(&tables, lower_instructions))
}

pub fn lower_or_report_error(stmts: ast::Stmts) -> rs::TokenStream {
    lower(stmts).unwrap_or_else(report_error)
}

pub fn phase_1_lower_stmts(stmts: ast::Stmts) -> (logic_ast::Stmts, Tables) {
    let mut 
}

pub fn phase_2_lower_to_ir(stmts: logic_ast::Stmts) -> Ir {
    match middle::ir::IrMapped::transform_from_stmts(stmts) {
        Ok(ir) => {
            // ir.report_warnings(ecx);
            if let Some(errors) = ir.get_errors() {
                // for error in errors {
                //     report_error(ecx, sp, error);
                // }
                return rs::TokenStream::new();
            }
            let result = back::IrTranslator::new(ir.into()).generate().translate();
            // Log the generated code.
            let _ = env_logger::init();
            match result {
                GenResult::Parser(expr) => {
                    info!("{}", expr);
                    // info!("{}", rs::pprust::expr_to_string(&*expr));
                    expr.parse().unwrap()
                }
                GenResult::Lexer(stmts) => {
                    info!(" ========== BEGIN LEXER OUT");
                    info!("{}", stmts);                    
                    // let mut whole_str = String::new();
                    // for stmt in &stmts {
                    //     whole_str.push_str(&rs::pprust::stmt_to_string(stmt)[..]);
                    // }
                    // info!("{}", whole_str);
                    info!(" ========== END LEXER OUT");
                    stmts.parse().unwrap()
                }
            }
        },
        Err(error) => {
            // report_error(ecx, sp, &error);
            rs::TokenStream::new()
        }
    }
}

pub fn phase_3_translate(ir: &middle::Ir) -> Vec<Instruction> {

}

pub fn phase_4_lower_instructions(ir: &Ir, tables: &Tables, instructions: Vec<Instruction>) -> Vec<LowerInstruction> {
    output::instruction::translate(ir, tables, instructions)

}

pub fn phase_5_generate(tables: &Tables, lower_instructions: Vec<LowerInstruction>) -> rs::TokenStream {

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
