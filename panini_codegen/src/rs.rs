#![allow(dead_code)]

// Public imports

pub use syntax::ast;
pub use syntax::ast::TokenTree;
pub use syntax::ext::base::{ExtCtxt, MacResult, MacEager, DummyResult};
pub use syntax::ext::expand::ExpansionConfig;
pub use syntax::ptr::P;
pub use syntax::parse::lexer;
pub use syntax::ext::tt::transcribe;
pub use syntax::parse::{self, token};
pub use syntax::parse::token::*;
pub use syntax::util::interner::Interner;
pub use syntax::attr::{mk_attr_id, ThinAttributesExt, AttrMetaMethods};
pub use syntax::util::small_vector::SmallVector;
pub use syntax::print::pprust;

pub use syntax::ast::{
    TyKind, Ty, PathParameters, AngleBracketed, AngleBracketedParameterData,
    PathSegment, Block, Pat, Name, Expr, Delimited,
    PatKind, Item, SpannedIdent, EMPTY_CTXT, StmtKind,  DeclKind,
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

use std::rc::Rc;

use syntax::errors::emitter::Emitter;
use syntax::errors::{Level, Handler, DiagnosticBuilder};
use syntax::parse::ParseSess;

pub fn with_fake_extctxt<T, F>(f: F) -> T where F: Fn(&mut ExtCtxt) -> T {
    let mut failed = false;
    let failptr = &mut failed as *mut bool;
    let cm = Rc::new(CodeMap::new());
    let ce = Box::new(CustomEmitter::new(failptr));
    let handler = Handler::with_emitter(true, false, ce);
    let parse_sess = ParseSess::with_span_handler(handler, cm);
    let excfg = ExpansionConfig {
        crate_name: ("test").parse().unwrap(),
        features: None,
        recursion_limit: 10,
        trace_mac: true,
    };
    let mut gated_cfgs = vec![];
    let cfg = vec![];
    let mut cx = ExtCtxt::new(&parse_sess, cfg, excfg, &mut gated_cfgs);
    cx.bt_push(ExpnInfo {
        call_site: mk_sp(BytePos(0), BytePos(0)),
        callee: NameAndSpan {
            format: MacroBang(ast::Name(0u32)),
            allow_internal_unstable: true,
            span: None,
        }
    });
    f(&mut cx)
}

struct CustomEmitter {
    failed: *mut bool
}

impl CustomEmitter {
    pub fn new(fp: *mut bool) -> CustomEmitter {
        CustomEmitter {
          failed: fp,
        }
    }
}

unsafe impl Send for CustomEmitter {}

impl Emitter for CustomEmitter {
    fn emit(&mut self, _: &MultiSpan, msg: &str, _: Option<&str>, level: Level) {
        unsafe { *self.failed = true };
        println!("{} {}", level, msg);
    }

    fn emit_struct(&mut self, _db: &DiagnosticBuilder) {
        panic!();
    }
}
