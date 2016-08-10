#![feature(plugin, plugin_registrar, rustc_private)]
#![plugin(quasi_macros)]

#[macro_use]
extern crate log;
extern crate env_logger;

extern crate rustc;
extern crate syntax;

extern crate aster;
extern crate quasi;

extern crate bit_matrix;
extern crate cfg;
extern crate cfg_regex;
extern crate gearley;

pub mod front;
pub mod middle;
pub mod back;

pub mod rs;

pub use front::lexer;

use std::env;
use std::error::Error;

use front::ast::Stmts;
use middle::error::TransformationError;
use back::GenResult;

pub fn codegen<'cx>(ecx: &'cx mut rs::ExtCtxt,
                sp: rs::Span,
                stmts: Stmts) -> Box<rs::MacResult + 'cx> {
    // Initialize logging.
    let _ = env_logger::init();
    // Codegen.
    match middle::ir::IrMapped::transform_from_stmts(stmts) {
        Ok(ir) => {
            ir.report_warnings(ecx);
            if let Some(errors) = ir.get_errors() {
                for error in errors {
                    report_error(ecx, sp, error);
                }
                return rs::DummyResult::any(rs::DUMMY_SP);
            }
            let result = back::IrTranslator::new(ir.into()).generate().translate(ecx);
            maybe_print_expansion(ecx, &result);
            match result {
                GenResult::Parser(expr) => {
                    rs::MacEager::expr(expr)
                }
                GenResult::Lexer(stmts) => {
                    rs::MacEager::stmts(rs::SmallVector::many(stmts))
                }
            }
        },
        Err(error) => {
            report_error(ecx, sp, &error);
            rs::DummyResult::any(rs::DUMMY_SP)
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

fn maybe_print_expansion(ecx: &mut rs::ExtCtxt, result: &GenResult) {
    if let Some(var_string) = env::var("PANINI_EXPANSION").ok() {
        if var_string.trim() != "0" {
            print_expansion(ecx, result);
        }
    }
}

fn print_expansion(ecx: &mut rs::ExtCtxt, result: &GenResult) {
    let invocation_mod_path = ecx.mod_path();
    let mod_name = invocation_mod_path.last().unwrap();
    // Generate invocation for the snapshot.
    println!("generate_run_parse!{{cx,{},", mod_name);
    match result {
        &GenResult::Parser(ref expr) => {
            // No need for braces around the expr. It already has braces.
            println!("{}", rs::pprust::expr_to_string(&**expr));
        }
        &GenResult::Lexer(ref stmts) => {
            println!("{{");
            let mut whole_str = String::new();
            for stmt in stmts {
                whole_str.push_str(&rs::pprust::stmt_to_string(stmt)[..]);
            }
            println!("{}", whole_str);
            println!("}}");
        }
    }
    // The invocation is still open. The enum_stream's expansion will be printed next.
}
