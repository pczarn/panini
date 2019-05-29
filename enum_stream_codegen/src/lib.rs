#![feature(plugin, rustc_private)]

#![allow(unused_imports)]
#![allow(unused_variables)]

#![recursion_limit = "256"]

#[macro_use]
extern crate log;
extern crate env_logger;

extern crate rustc;
extern crate rustc_plugin;
extern crate syntax;

extern crate quote;

extern crate enum_coder;
extern crate panini_common;

pub mod front;

mod middle;
mod back;

use std::collections::HashMap;
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::iter::{self, Iterator};
use std::rc::Rc;
use std::mem;

use panini_common::rs;

use front::Stmts;

fn log(code: &rs::TokenStream) {
    let _ = env_logger::init();
    info!("{}", code);
}

pub fn codegen(stmts: Stmts) -> rs::TokenStream {
    let ir = middle::transform(stmts).unwrap();
    let instructions = back::IrTranslator::new(ir).translate();
    let result = instructions.generate();
    log(&result);
    result
}
