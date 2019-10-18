use std::borrow::Cow;
use std::collections::BTreeMap;

use input::ast::{Stmts, Rhs, RhsAst};
use input::FragmentId;
use middle::flatten_stmts::{Path, Position};

#[derive(Debug)]
pub struct Trace {
    pub tokens: BTreeMap<Path, TraceToken>,
    pub stmt_positions: Vec<StmtTokenPosition>,
    pub rule_tokens: Vec<BTreeMap<Option<usize>, usize>>,
}

#[derive(Debug)]
pub struct StmtTokenPosition {
    lhs: Option<FragmentId>,
    position: usize,
}

// #[derive(Clone, Debug, Eq, PartialEq)]
// pub struct SourceOrigin {
//     pub rule_id: u32,
//     pub rule_pos: Vec<u32>,
// }

#[derive(Debug, Eq, PartialEq)]
pub enum TraceToken {
    Fragment(FragmentId),
    Quote,
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
            &TraceToken::Quote => Cow::Borrowed("\""),
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
            stmt_positions: vec![],
            rule_tokens: vec![],
        };
        trace.flatten_stmts(stmts);
        trace
    }

    fn flatten_stmts(&mut self, stmts: &Stmts) {
        for (stmt_idx, stmt) in stmts.stmts.iter().enumerate() {
            let mut last_level = None;
            let start_path = Path {
                position: vec![
                    Position::IdxWithFragment {
                        idx: stmt_idx,
                        fragment: stmt.lhs,
                    },
                ],
            };
            self.stmt_positions.push(StmtTokenPosition {
                lhs: Some(stmt.lhs),
                position: self.tokens.len()
            });
            for (alternative_idx, (level, ref rhs, ref _action)) in stmt.body.iter().enumerate() {
                let mut path = start_path.clone();
                if stmt.body.len() > 1 {
                    path.position.push(Position::Alternative(alternative_idx));
                }
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
        self.stmt_positions.push(StmtTokenPosition {
            lhs: None,
            position: self.tokens.len(),
        });
    }

    fn flatten_rhs(&mut self, path: Path, rhs: &Rhs) {
        for (rhs_idx, element) in rhs.0.iter().enumerate() {
            let mut path = path.clone();
            // if let Some(bind_id) = element.bind {
            //     path.position.push(Position::Bind(bind_id));
            // }
            match &element.elem {
                &RhsAst::Fragment(_) => {}
                _ => {
                    if rhs.0.len() > 1 {
                        path.position.push(Position::Idx(rhs_idx));
                    }
                }
            }
            match &element.elem {
                &RhsAst::Fragment(fragment_id) => {
                    path.position.push(Position::IdxWithFragment {
                        idx: rhs_idx,
                        fragment: fragment_id,
                    });
                    self.tokens.insert(path, TraceToken::Fragment(fragment_id));
                }
                &RhsAst::String(ref _string) => {
                    // path.position.push(Position::Idx(rhs_idx));
                    // let mut left_quote_path = path.clone();
                    // let mut right_quote_path = path.clone();
                    // left_quote_path.push(Position::LeftQuote);
                    // right_quote_path.push(Position::RightQuote);
                    // self.tokens.insert(left_quote_path, TraceToken::Quote);
                    // self.tokens.insert(right_quote_path, TraceToken::Quote);
                    // let mut regex_rewrite = RegexTranslation::new();
                    unimplemented!()
                }
                &RhsAst::Sequence(ref sequence) => {
                    path.position.push(Position::Sequence {
                        min: sequence.min,
                        max: sequence.max,
                    });
                    if sequence.rhs.0.len() > 1 {
                        let left_paren_path = path.clone();
                        let mut right_paren_path = path.clone();
                        right_paren_path.position.push(Position::SequenceEnd);
                        self.tokens.insert(left_paren_path, TraceToken::LParen);
                        self.tokens.insert(right_paren_path, TraceToken::RParen);
                    }
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
                    if summands.is_empty() {
                        self.tokens.insert(path.clone(), TraceToken::EmptyProduct);
                    }
                    for (summand_idx, summand) in summands.iter().enumerate() {
                        let mut path = path.clone();
                        if summands.len() > 1 {
                            path.position.push(Position::Alternative(summand_idx));
                        }
                        if summand_idx != 0 {
                            self.tokens.insert(path.clone(), TraceToken::Alternative);
                        }
                        self.flatten_rhs(path, summand);
                    }
                }
                &RhsAst::Product(ref rhs) => {
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

    pub fn stringify_tokens(&self, fragment_names: &BTreeMap<FragmentId, String>) -> Vec<String> {
        self.tokens.iter().map(|(_, token)| {
            token.as_str(fragment_names).to_string()
        }).collect()
    }
}
