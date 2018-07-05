#![recursion_limit = "256"]
#![feature(plugin_registrar, rustc_private)]

#[macro_use]
extern crate log;
extern crate env_logger;

extern crate rustc;
extern crate syntax;
extern crate syntax_pos;

#[macro_use]
extern crate quote;
extern crate proc_macro2;

extern crate bit_matrix;
extern crate cfg;
extern crate cfg_regex;
extern crate gearley;

pub mod front;
pub mod middle;
pub mod back;

pub mod rs;

pub use front::lexer;

use std::error::Error;

use front::ast::Stmts;
use middle::error::TransformationError;
use back::GenResult;

pub fn codegen<'cx>(ecx: &'cx mut rs::ExtCtxt,
                sp: rs::Span,
                stmts: Stmts) -> rs::TokenStream {
    match middle::ir::IrMapped::transform_from_stmts(stmts) {
        Ok(ir) => {
            ir.report_warnings(ecx);
            if let Some(errors) = ir.get_errors() {
                for error in errors {
                    report_error(ecx, sp, error);
                }
                return rs::TokenStream::empty();
            }
            let result = back::IrTranslator::new(ir.into()).generate().translate(ecx);
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
            report_error(ecx, sp, &error);
            rs::TokenStream::empty()
        }
    }
}

fn report_error(ecx: &mut rs::ExtCtxt, sp: rs::Span, error: &TransformationError) {
    match error {
        &TransformationError::RecursiveType(ref types) => {
            for rule in types {
                let mut diag = ecx.struct_span_err(rule.lhs.span, error.description());
                let cause_spans = rule.causes.iter().map(|c| c.span).collect();
                let multispan = rs::MultiSpan::from_spans(cause_spans);
                let msg = if multispan.primary_spans().len() == 1 {
                    "this symbol has a recursive type:"
                } else {
                    "these symbols have recursive types:"
                };
                diag.span_note(multispan, msg);
                diag.emit();
            }
        }
        _ => {
            ecx.span_err(sp, error.description());
            ecx.parse_sess.span_diagnostic.abort_if_errors();
            panic!();
        }
    }
}
