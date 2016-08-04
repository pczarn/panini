use std::borrow::Cow;

use front::Name;
use front::ast::{self, Stmts, Stmt, Rhs};
use front::visit::RhsAstVisitor;
use rs;

#[derive(Debug)]
pub struct Trace {
    stmts: Vec<TraceStmt>
}

#[derive(Debug)]
pub struct TraceStmt {
    pub lhs: rs::Name,
    pub rhs: Vec<TraceToken>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceOrigin {
    pub rule_id: u32,
    pub rule_pos: Vec<u32>,
}

#[derive(Debug)]
pub enum TraceToken {
    Name(rs::Name),
    String(rs::Name),
    LParen,
    RParen,
    Star,
    Plus,
    Alternative,
}

impl TraceToken {
    pub fn as_str(&self) -> Cow<str> {
        match self {
            &TraceToken::LParen => Cow::Borrowed("("),
            &TraceToken::RParen => Cow::Borrowed(")"),
            &TraceToken::Star => Cow::Borrowed("*"),
            &TraceToken::Plus => Cow::Borrowed("+"),
            &TraceToken::Alternative => Cow::Borrowed("|"),
            &TraceToken::Name(name) => Cow::Owned(name.as_str().to_string()),
            &TraceToken::String(name) => Cow::Owned(name.as_str().to_string()),
        }
    }
}

impl Trace {
    pub fn from_stmts(stmts: &Stmts) -> Self {
        let mut trace = Trace {
            stmts: vec![]
        };
        for stmt in &stmts.stmts {
            trace.transform_stmt(stmt);
        }
        trace
    }

    fn transform_stmt(&mut self, stmt: &Stmt) {
        for level in &stmt.rhs {
            for &(ref rhs, _) in level {
                self.transform_rhs(stmt.lhs.node, rhs);
            }
        }
    }


    fn transform_rhs(&mut self, lhs: rs::Name, rhs: &Rhs) {
        let mut visitor = TraceRhs::new();
        visitor.visit_rhs(rhs);
        self.stmts.push(TraceStmt {
            lhs: lhs,
            rhs: visitor.rhs,
        });
    }

    pub fn stmts(&self) -> &[TraceStmt] {
        &self.stmts[..]
    }
}

// The behavior can be modified to show levels of precedenced rules.
// Currently, only single BNF rules appear in the trace.

struct TraceRhs {
    rhs: Vec<TraceToken>,
}

impl TraceRhs {
    pub fn new() -> Self {
        TraceRhs {
            // Rule location is equivalent to an index into `rhs`.
            rhs: vec![],
        }
    }
}

impl RhsAstVisitor for TraceRhs {
    fn visit_rhs_symbol(&mut self, symbol: Name) {
        self.rhs.push(TraceToken::Name(symbol.node));
    }

    fn visit_sequence(&mut self, sequence: &ast::Sequence) {
        self.walk_sequence(sequence);
        if sequence.min == 0 {
            self.rhs.push(TraceToken::Star);
        } else if sequence.min == 1 {
            self.rhs.push(TraceToken::Plus);
        }
    }

    fn visit_sum(&mut self, sum: &[Rhs]) {
        for rule in sum {
            self.visit_rhs(rule);
            self.rhs.push(TraceToken::Alternative);
        }
        if !sum.is_empty() {
            self.rhs.pop();
        }
    }

    fn visit_product(&mut self, product: &Rhs) {
        self.rhs.push(TraceToken::LParen);
        self.walk_product(product);
        self.rhs.push(TraceToken::RParen);
    }

    fn visit_rhs_string(&mut self, string: Name) {
        self.rhs.push(TraceToken::String(string.node));
    }
}
