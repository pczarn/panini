#![allow(dead_code)]

// Public imports

pub use quote::Tokens;

pub use syntax::ast;
pub use syntax::tokenstream::{TokenTree, Delimited};
pub use syntax::ext::base::{ExtCtxt, MacResult, MacEager, DummyResult};
pub use syntax::ptr::P;
pub use syntax::parse::lexer;
pub use syntax::ext::tt::transcribe;
pub use syntax::parse::{self, token};
pub use syntax::parse::token::*;
pub use syntax::tokenstream::TokenStream;
// pub use syntax_pos::symbol::Interner;
pub use syntax_pos::symbol::Symbol;
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
    mk_sp,
    BytePos,
    ExpnInfo,
    NameAndSpan,
    Span,
    MultiSpan,
    Spanned,
    DUMMY_SP,
    respan,
    spanned,
    dummy_spanned,
};

// Private imports

use syntax::parse::ParseSess;

pub fn with_fake_extctxt<T, F>(f: F) -> T where F: Fn(&ExtCtxt) -> T {
    let parse_sess = ParseSess::new(FilePathMapping::empty());
    let exp_cfg = ExpansionConfig::default("panini_test".to_owned());
    let mut resolver = DummyResolver;
    let cx = ExtCtxt::new(&parse_sess, exp_cfg, &mut resolver);
    f(&cx)
}
