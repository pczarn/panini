#![feature(plugin, plugin_registrar, rustc_private)]
#![plugin(quasi_macros, panini_macros)]

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

extern crate panini;
extern crate panini_codegen;

pub mod front;
pub mod middle;
pub mod back;

pub use panini_codegen::rs;

use std::collections::HashMap;
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::iter::{self, Iterator};
use std::rc::Rc;
use std::mem;

use aster::expr::ExprBuilder;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut ::rustc_plugin::Registry) {
    reg.register_macro("char_classifier", expand_char_classifier);
}

fn expand_char_classifier<'cx>(ecx: &'cx mut rs::ExtCtxt,
                       sp: rs::Span,
                       tts: &[rs::TokenTree]) -> Box<rs::MacResult + 'cx> {
    #![allow(non_snake_case)]
    let stmts = front::Parser::new().parse_from_tts(ecx, tts);
    match middle::transform(stmts) {
        Ok(ir) => {
            let items = back::IrTranslator::new(ir).translate(ecx);
            // Log the generated code.
            let _ = env_logger::init();
            for item in &items {
                info!("{}", rs::pprust::stmt_to_string(item));
            }
            rs::MacEager::stmts(rs::SmallVector::many(items))
        },
        Err(error) => rs::DummyResult::any(rs::DUMMY_SP),
    }
}
