#![allow(dead_code)]

// Public imports

pub use syntax::ast;
pub use syntax::tokenstream::{TokenTree, Delimited};
pub use syntax::ext::base::{ExtCtxt, MacResult, MacEager, DummyResult, DummyMacroLoader};
pub use syntax::ext::expand::ExpansionConfig;
pub use syntax::ptr::P;
pub use syntax::parse::lexer;
pub use syntax::ext::tt::transcribe;
pub use syntax::parse::{self, token};
pub use syntax::parse::token::*;
pub use syntax::util::interner::Interner;
pub use syntax::attr::{mk_attr_id, AttrMetaMethods};
pub use syntax::util::small_vector::SmallVector;
pub use syntax::print::pprust;

pub use syntax::ast::{
    TyKind, Ty, PathParameters, AngleBracketed, AngleBracketedParameterData,
    PathSegment, Block, Pat, Name, Expr,
    PatKind, Item, SpannedIdent, StmtKind,
    Attribute, Attribute_, Stmt,
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
    let parse_sess = ParseSess::new();
    let mut loader = DummyMacroLoader;
    let mut cx = ExtCtxt::new(
        &parse_sess,
        vec![],
        ExpansionConfig::default("panini".to_string()),
        &mut loader
    );
    cx.bt_push(ExpnInfo {
        call_site: DUMMY_SP,
        callee: NameAndSpan {
            format: MacroBang(intern("")),
            span: None,
            allow_internal_unstable: false,
        }
    });
    f(&cx)
}
