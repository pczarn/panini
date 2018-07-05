#![allow(dead_code)]

// Public imports

pub use quote::Tokens;
// pub use proc_macro2::Term;
pub use quote::{Ident as Term};
pub use proc_macro2::TokenStream;

pub use syntax::ast;
pub use syntax::tokenstream::{TokenTree, Delimited};
pub use syntax::ext::base::{ExtCtxt, MacResult, MacEager, DummyResult};
pub use syntax::ptr::P;
pub use syntax::parse::lexer;
pub use syntax::ext::tt::transcribe;
pub use syntax::parse::{self, token};
pub use syntax::parse::token::*;
// pub use syntax::tokenstream::TokenStream;
// pub use syntax_pos::symbol::Interner;
pub use syntax_pos::symbol::{Symbol, Ident};
pub use syntax::symbol::InternedString;
pub use syntax::attr::{mk_attr_id};
pub use syntax::util::small_vector::SmallVector;
pub use syntax::print::pprust;

use syntax::ext::base::DummyResolver;
use syntax::ext::expand::ExpansionConfig;
use syntax::parse::ParseSess;
use syntax::codemap::FilePathMapping;

pub use syntax::ast::{
    TyKind, Ty, PathParameters, AngleBracketed, AngleBracketedParameterData,
    PathSegment, Block, Pat, Name, Expr,
    PatKind, Item, SpannedIdent, StmtKind,
    Attribute, Stmt,
};

pub use syntax::codemap::{
    self,
    MacroBang,
    CodeMap,
    BytePos,
    ExpnInfo,
    NameAndSpan,
    Span,
    MultiSpan,
    Spanned,
    DUMMY_SP,
    respan,
    dummy_spanned,
    NO_EXPANSION
};

// Private imports

pub fn mk_sp(lo: BytePos, hi: BytePos) -> Span {
    Span { lo: lo, hi: hi, ctxt: NO_EXPANSION }
}

pub fn with_fake_extctxt<T, F>(f: F) -> T where F: Fn(&ExtCtxt) -> T {
    let parse_sess = ParseSess::new(FilePathMapping::empty());
    let exp_cfg = ExpansionConfig::default("panini_test".to_owned());
    let mut resolver = DummyResolver;
    let cx = ExtCtxt::new(&parse_sess, exp_cfg, &mut resolver);
    f(&cx)
}
