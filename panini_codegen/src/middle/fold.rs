use std::collections::HashMap;
use std::cell::RefCell;
use std::hash::Hash;

use cfg::symbol::{Symbol, SymbolSource};

use rs;

use middle::{Ty, AutoTy, Hir};
use middle::hir::{Rule, SequenceRule};

pub trait FoldHir<S1> where S1: Eq + Hash {
    type Symbol: Eq + Hash;
    fn fold_hir(&mut self, hir: Hir<S1>) -> Hir<Self::Symbol> {
        Hir {
            rules: self.fold_rules(hir.rules),
            sequence_rules: self.fold_sequence_rules(hir.sequence_rules),
            type_map: self.fold_type_map(hir.type_map),
            type_equality: hir.type_equality.into_iter().map(|(sym, ty)| {
                (self.fold_symbol(sym), self.fold_ty(ty))
            }).collect(),
            assert_type_equality: RefCell::new(
                hir.assert_type_equality.into_inner().into_iter().map(|(sym, ty)| {
                    (self.fold_symbol(sym), self.fold_ty(ty))
                }).collect()
            ),
            embedded_strings: hir.embedded_strings.into_iter().map(|(sym, string, source_origin)| {
                (self.fold_spanned_symbol(sym), string, source_origin)
            }).collect(),
        }
    }

    fn fold_rules(&mut self, rules: Vec<Rule<S1>>) -> Vec<Rule<Self::Symbol>> {
        rules.into_iter().map(|rule| self.fold_rule(rule)).collect()
    }

    fn fold_sequence_rules(&mut self, sequence_rules: Vec<SequenceRule<S1>>)
        -> Vec<SequenceRule<Self::Symbol>>
    {
        sequence_rules.into_iter().map(|rule| self.fold_sequence_rule(rule)).collect()
    }

    fn fold_rule(&mut self, rule: Rule<S1>) -> Rule<Self::Symbol> {
        Rule {
            lhs: self.fold_spanned_symbol(rule.lhs),
            rhs: rule.rhs.into_iter().map(|sym| self.fold_spanned_symbol(sym)).collect(),
            tuple_binds: rule.tuple_binds,
            deep_binds: rule.deep_binds,
            shallow_binds: rule.shallow_binds,
            source_origin: rule.source_origin,
            action: rule.action,
        }
    }

    fn fold_sequence_rule(&mut self, sequence_rule: SequenceRule<S1>)
        -> SequenceRule<Self::Symbol>
    {
        SequenceRule {
            lhs: self.fold_spanned_symbol(sequence_rule.lhs),
            rhs: self.fold_spanned_symbol(sequence_rule.rhs),
            // sep: Option<rs::Name>,
            min: sequence_rule.min,
            max: sequence_rule.max,
            source_origin: sequence_rule.source_origin,
            action: sequence_rule.action,
        }
    }

    fn fold_type_map(&mut self, type_map: HashMap<S1, Ty<S1>>)
        -> HashMap<Self::Symbol, Ty<Self::Symbol>>
    {
        type_map.into_iter().map(|(key, val)| {
            (self.fold_symbol(key), self.fold_ty(val))
        }).collect()
    }

    fn fold_ty(&mut self, ty: Ty<S1>) -> Ty<Self::Symbol> {
        match ty {
            Ty::Auto(AutoTy::Tuple { fields }) => {
                Ty::Auto(AutoTy::Tuple {
                    fields: fields.into_iter().map(|sym| self.fold_symbol(sym)).collect()
                })
            }
            Ty::Auto(AutoTy::Struct { members }) => {
                Ty::Auto(AutoTy::Struct {
                    members: members.into_iter().map(|(name, sym)| {
                        (name, self.fold_symbol(sym))
                    }).collect()
                })
            }
            Ty::RustTy(rust_ty) => Ty::RustTy(rust_ty),
            Ty::SequenceVec(sym) => Ty::SequenceVec(self.fold_symbol(sym)),
            Ty::Infer => Ty::Infer,
        }
    }

    fn fold_spanned_symbol(&mut self, symbol: rs::Spanned<S1>) -> rs::Spanned<Self::Symbol> {
        rs::respan(symbol.span, self.fold_symbol(symbol.node))
    }

    fn fold_symbol(&mut self, symbol: S1) -> Self::Symbol;
}

pub struct Folder<'a> {
    pub sym_map: HashMap<rs::Name, Symbol>,
    pub sym_vec: Vec<Option<rs::Name>>,
    sym_source: &'a mut SymbolSource,
}

impl<'a> Folder<'a> {
    pub fn new(sym_source: &'a mut SymbolSource) -> Self {
        Folder {
            sym_map: HashMap::new(),
            sym_vec: vec![],
            sym_source: sym_source,
        }
    }
}

impl<'a> FoldHir<rs::Name> for Folder<'a> {
    type Symbol = Symbol;
    fn fold_symbol(&mut self, symbol: rs::Name) -> Symbol {
        let sym_source = &mut self.sym_source;
        let sym_vec = &mut self.sym_vec;
        *self.sym_map.entry(symbol).or_insert_with(|| {
            let sym = sym_source.sym();
            sym_vec.push(Some(symbol));
            sym
        })
    }
}
