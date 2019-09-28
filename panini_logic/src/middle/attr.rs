use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;

use rs;
// use rs::AttrMetaMethods;

use middle::error::TransformationError;
use middle::lint::{Lint, Level};

pub struct Attrs<S> {
    arguments_from_outer_layer: Option<ArgumentsFromOuterLayer<S>>,
    lint_levels: HashMap<rs::Ident, Level>,
    pub overruled_lint: Vec<rs::Span>,
    pub invalid_lint: Vec<rs::Span>,
    pub unused_attrs: Vec<rs::Span>,
}

/// The outer layer invokes this layer, passing arguments. These arguments consist of
/// this layer's level number and a list of terminals.
#[derive(Clone)]
pub struct ArgumentsFromOuterLayer<S> {
    level: usize,
    terminals: Vec<S>,
}

impl Attrs<rs::Ident> {
    pub fn compute(attrs: &[rs::Attribute]) -> Result<Self, TransformationError> {
        let mut lexer_attr = None;
        let mut lint_attrs = vec![];
        let mut invalid_lint_attrs = HashSet::new();
        let mut unused_attrs = vec![];
        for attr in attrs {
            let name = &*attr.path.segments[0].identifier.name.as_str();
            if name.starts_with("lexer_") {
                if lexer_attr.is_some() || attr.path.segments.len() > 1 {
                    return Err(TransformationError::InvalidAttr(attr.span));
                }
                lexer_attr = Some((name.clone(), attr.tokens.clone()));
                continue;
            }

            if let Some(lint_level) = Level::from_str(name) {
                let cursor = attr.tokens.trees();

                while let Some(tt) = cursor.next() {
                    match cursor.next() {
                        Some(rs::TokenTree::Token::Punct(rs::Punct { op: ',', .. })) | None => {}
                        Some(tt) => {
                            lint_attrs.push(Err(tt.span()));
                            continue;
                        }
                        None => {
                            lint_attrs.push(Err(attr.span));
                            continue;
                        }
                    }
                    match tt {
                        rs::TokenTree::Ident(rs::Ident { inner, .. }) => {
                            lint_attrs.push(Ok((inner, lint_level, rs::Span::call_site())));
                        }
                        _ => {
                            lint_attrs.push(Err(tt.span()));
                        }
                    }
                }
            } else {
                unused_attrs.push(attr.span);
            }
        }
        // Map the lexer attr.
        let arguments_from_outer_layer = lexer_attr.map(|(lexer_level, tokens)| {
            let level = lexer_level["lexer_".len() ..].parse().unwrap();
            let mut terminals = vec![];
            let cursor = tokens.trees();

            while let Some(tt) = cursor.next() {
                match cursor.next() {
                    Some(rs::TokenTree::Punct(rs::Punct { op: ',', .. })) | None => {}
                    tt => unreachable!()
                }
                match tt {
                    rs::TokenTree::Ident(rs::Ident { inner, .. }) => {
                        terminals.push(inner);
                    }
                    _ => unreachable!()
                }
            }
            ArgumentsFromOuterLayer {
                level: level,
                terminals: terminals,
            }
        });

        let mut lint_levels = HashMap::new();
        let mut overruled_lint = vec![];

        for result in lint_attrs {
            match result {
                Ok((name, level, span)) => {
                    match lint_levels.entry(name) {
                        Entry::Occupied(mut occupied) => {
                            if *occupied.get() == Level::Forbid || level == Level::Forbid {
                                overruled_lint.push(span);
                            } else {
                                occupied.insert(level);
                            }
                        }
                        Entry::Vacant(vacant) => {
                            vacant.insert(level);
                        }
                    }
                }
                Err(span) => {
                    invalid_lint_attrs.insert(span);
                }
            }
        }

        Ok(Attrs {
            arguments_from_outer_layer: arguments_from_outer_layer,
            lint_levels: lint_levels,
            overruled_lint: overruled_lint,
            invalid_lint: invalid_lint_attrs.into_iter().collect(),
            unused_attrs: unused_attrs,
        })
    }
}

impl<S> Attrs<S> {
    pub fn map_symbols<F, S2>(self, f: F) -> Attrs<S2>
        where F: Fn(S) -> S2
    {
        Attrs {
            arguments_from_outer_layer: self.arguments_from_outer_layer.map(|lexer| {
                ArgumentsFromOuterLayer {
                    level: lexer.level,
                    terminals: lexer.terminals.into_iter().map(f).collect(),
                }
            }),
            lint_levels: self.lint_levels,
            overruled_lint: self.overruled_lint,
            invalid_lint: self.invalid_lint,
            unused_attrs: self.unused_attrs,
        }
    }

    pub fn arguments_from_outer_layer(&self) -> &Option<ArgumentsFromOuterLayer<S>> {
        &self.arguments_from_outer_layer
    }

    pub fn get_lint_level(&self, lint: Lint) -> Level {
        let ident = rs::Term::intern(lint.name());
        self.lint_levels.get(&ident).cloned().unwrap_or(lint.default_level())
    }
}

impl<S> ArgumentsFromOuterLayer<S> {
    pub fn terminals(&self) -> &[S] {
        &self.terminals[..]
    }

    pub fn current_level(&self) -> usize {
        self.level
    }
}
