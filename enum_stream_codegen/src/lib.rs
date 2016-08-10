#![feature(plugin, rustc_private)]
#![plugin(quasi_macros)]

#![allow(unused_imports)]
#![allow(unused_variables)]

#[macro_use]
extern crate log;
extern crate env_logger;

extern crate rustc;
extern crate rustc_plugin;
extern crate syntax;

extern crate aster;
extern crate quasi;

extern crate panini_codegen;

pub mod front;
pub mod middle;
pub mod back;

use std::collections::HashMap;
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::env;
use std::iter::{self, Iterator};
use std::rc::Rc;
use std::mem;

use aster::expr::ExprBuilder;

pub use panini_codegen::rs;

use front::Stmts;

pub fn codegen<'cx>(ecx: &'cx mut rs::ExtCtxt,
                       sp: rs::Span,
                       stmts: Stmts) -> Box<rs::MacResult + 'cx> {
    // Initialize logging.
    let _ = env_logger::init();
    match middle::transform(stmts) {
        Ok(ir) => {
            let items = back::IrTranslator::new(ir).translate(ecx);
            maybe_print_expansion(ecx, &items[..]);
            rs::MacEager::stmts(rs::SmallVector::many(items))
        },
        Err(error) => rs::DummyResult::any(rs::DUMMY_SP),
    }
}

fn maybe_print_expansion(ecx: &mut rs::ExtCtxt, stmts: &[rs::Stmt]) {
    if let Some(var_string) = env::var("PANINI_EXPANSION").ok() {
        if var_string.trim() != "0" {
            print_expansion(ecx, stmts);
        }
    }
}

fn print_expansion(ecx: &mut rs::ExtCtxt, stmts: &[rs::Stmt]) {
    println!("{{");
    let mut whole_str = String::new();
    for stmt in stmts {
        whole_str.push_str(&rs::pprust::stmt_to_string(stmt)[..]);
    }
    println!("{}", whole_str);
    println!("}}");
    // Close the invocation of generate_run_parse!.
    println!("}}");
}
