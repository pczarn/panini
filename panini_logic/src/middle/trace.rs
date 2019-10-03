use std::borrow::Cow;
use std::collections::BTreeMap;

use input::ast::{Stmts, Rhs, RhsAst};
use input::FragmentId;
use middle::flatten_stmts::{Path, Position};

#[derive(Debug)]
pub struct Trace {
    tokens: BTreeMap<Path, TraceToken>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceOrigin {
    pub rule_id: u32,
    pub rule_pos: Vec<u32>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TraceToken {
    Fragment(FragmentId),
    String(String),
    LParen,
    RParen,
    Star,
    Plus,
    EmptyProduct,
    Alternative,
    PrecedencedAlternative,
}

impl TraceToken {
    pub fn as_str(&self, fragment_names: &BTreeMap<FragmentId, String>) -> Cow<str> {
        match self {
            &TraceToken::LParen => Cow::Borrowed("("),
            &TraceToken::RParen => Cow::Borrowed(")"),
            &TraceToken::Star => Cow::Borrowed("*"),
            &TraceToken::Plus => Cow::Borrowed("+"),
            &TraceToken::EmptyProduct => Cow::Borrowed("()"),
            &TraceToken::Alternative => Cow::Borrowed("|"),
            &TraceToken::PrecedencedAlternative => Cow::Borrowed("|>"),
            &TraceToken::String(ref name) => Cow::Owned(name.as_str().to_string()),

            &TraceToken::Fragment(fragment_id) => {
                Cow::Owned(fragment_names[&fragment_id].clone())
            },
        }
    }
}

impl Trace {
    pub fn from_stmts(stmts: &Stmts) -> Self {
        let mut trace = Trace {
            tokens: BTreeMap::new(),
        };
        trace.flatten_stmts(stmts);
        trace
    }

    fn flatten_stmts(&mut self, stmts: &Stmts) {
        for (stmt_idx, stmt) in stmts.stmts.iter().enumerate() {
            let mut last_level = None;
            for (alternative_idx, (level, ref rhs, ref _action)) in stmt.body.iter().enumerate() {
                let path = Path {
                    position: vec![
                        Position::IdxWithFragment {
                            idx: stmt_idx,
                            fragment: stmt.lhs,
                        },
                        Position::Alternative(alternative_idx),
                    ],
                };
                if alternative_idx != 0 {
                    let token = if last_level.is_none() || last_level == Some(level) {
                        TraceToken::Alternative
                    } else {
                        TraceToken::PrecedencedAlternative
                    };
                    self.tokens.insert(path.clone(), token);
                }
                last_level = Some(level);
                self.flatten_rhs(path, rhs);
            }
        }
    }

    fn flatten_rhs(&mut self, path: Path, rhs: &Rhs) {
        for (rhs_idx, element) in rhs.0.iter().enumerate() {
            let mut path = path.clone();
            match &element.elem {
                &RhsAst::Fragment(fragment_id) => {
                    path.position.push(Position::IdxWithFragment {
                        idx: rhs_idx,
                        fragment: fragment_id,
                    });
                    self.tokens.insert(path, TraceToken::Fragment(fragment_id));
                }
                &RhsAst::Sequence(ref sequence) => {
                    path.position.push(Position::Idx(rhs_idx));
                    path.position.push(Position::Sequence {
                        min: sequence.min,
                        max: sequence.max,
                    });
                    let mut star_or_plus_path = path.clone();
                    star_or_plus_path.position.push(Position::SequenceToken);
                    if sequence.min == 0 && sequence.max == None {
                        self.tokens.insert(star_or_plus_path, TraceToken::Star);
                    } else if sequence.min == 1 && sequence.max == None {
                        self.tokens.insert(star_or_plus_path, TraceToken::Plus);                        
                    }
                    self.flatten_rhs(path, &sequence.rhs);
                }
                &RhsAst::Sum(ref summands) => {
                    path.position.push(Position::Idx(rhs_idx));
                    if summands.is_empty() {
                        self.tokens.insert(path.clone(), TraceToken::EmptyProduct);
                    }
                    for (summand_idx, summand) in summands.iter().enumerate() {
                        let mut path = path.clone();
                        path.position.push(Position::Alternative(summand_idx));
                        if summand_idx != 0 {
                            self.tokens.insert(path.clone(), TraceToken::Alternative);
                        }
                        self.flatten_rhs(path, summand);
                    }
                }
                &RhsAst::Product(ref rhs) => {
                    path.position.push(Position::Idx(rhs_idx));
                    let mut left_paren_path = path.clone();
                    let mut right_paren_path = path.clone();
                    left_paren_path.position.push(Position::Idx(0));
                    right_paren_path.position.push(Position::Idx(1));
                    self.tokens.insert(left_paren_path, TraceToken::LParen);
                    self.tokens.insert(right_paren_path, TraceToken::RParen);
                    self.flatten_rhs(path, rhs);
                }
            }
        }
        if rhs.0.is_empty() {
            self.tokens.insert(path, TraceToken::EmptyProduct);
        }
    }

    pub fn traces_for_path(&self, path: &Path) -> (usize, usize) {
        let mut max_path = path.clone();
        max_path.position.push(Position::Max);
        (self.tokens.range(.. path).count(), self.tokens.range(.. max_path).count())
    }

    pub fn tokens(&self) -> &BTreeMap<Path, TraceToken> {
        &self.tokens
    }
}
