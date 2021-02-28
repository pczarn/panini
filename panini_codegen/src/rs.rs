#![allow(dead_code)]

use std::cell::RefCell;

// Public imports

pub use quote::ToTokens;
// pub use proc_macro2::Term;
pub use proc_macro2::Ident as Term;
pub use proc_macro2::{Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

// pub use syntax::ast;
// pub use syntax::tokenstream::{Delimited};
// pub use syntax::ext::base::{ExtCtxt, MacResult, MacEager, DummyResult};
// pub use syntax::ptr::P;
// pub use syntax::parse::lexer;
// pub use syntax::ext::tt::transcribe;
// pub use syntax::parse::{self, token};
// pub use syntax::parse::token::*;
// // pub use syntax::tokenstream::TokenStream;
// // pub use syntax_pos::symbol::Interner;
// pub use syntax_pos::symbol::{Symbol, Ident};
// pub use syntax::symbol::InternedString;
// pub use syntax::attr::{mk_attr_id};
// pub use syntax::util::small_vector::SmallVector;
// pub use syntax::print::pprust;

// use syntax::ext::base::DummyResolver;
// use syntax::ext::expand::ExpansionConfig;
// use syntax::parse::ParseSess;
// use syntax::codemap::FilePathMapping;

// pub use syntax::ast::{
//     TyKind, Ty, AngleBracketed,
//     PathSegment, Block, Pat, Name, Expr,
//     PatKind, Item, StmtKind,
//     Attribute, Stmt,
//     // SpannedIdent PathParameters AngleBracketedParameterData
// };

// pub use syntax::codemap::{
//     self,
//     MacroBang,
//     CodeMap,
//     BytePos,
//     ExpnInfo,
//     Span,
//     MultiSpan,
//     Spanned,
//     DUMMY_SP,
//     respan,
//     dummy_spanned,
//     NO_EXPANSION
//     // NameAndSpan
// };

// // Private imports

// pub fn mk_sp(lo: BytePos, hi: BytePos) -> Span {
//     Span { lo: lo, hi: hi, ctxt: NO_EXPANSION }
// }

// pub fn with_fake_extctxt<T, F>(f: F) -> T where F: Fn(&ExtCtxt) -> T {
//     let parse_sess = ParseSess::new(FilePathMapping::empty());
//     let exp_cfg = ExpansionConfig::default("panini_test".to_owned());
//     let mut resolver = DummyResolver;
//     let cx = ExtCtxt::new(&parse_sess, exp_cfg, &mut resolver);
//     f(&cx)
// }

#[derive(Clone)]
pub struct Spanned<T> {
    pub(super) span: Span,
    pub(super) elem: T,
}

pub fn respan<T>(span: Span, elem: T) -> Spanned<T> {
    Spanned { span, elem }
}

thread_local!(static COUNTER: RefCell<usize> = RefCell::new(0));

pub fn gensym(name: &str) -> Ident {
    let counter = COUNTER.with(|c| {
        *c.borrow_mut() += 1;
        *c.borrow() - 1
    });
    let gen_name = format!("{}__{}", name, counter);
    Ident::new(name, Span::call_site())
}

pub fn dummy_spanned<T>(elem: T) -> Spanned<T> {
    respan(Span::call_site(), elem)
}

// pub struct Attribute;
// pub struct ExtCtxt;

// #[derive(Hash, Eq, PartialEq)]
// pub struct Name(u32);
