use std::collections::HashMap;
use std::cell::RefCell;
use std::hash::Hash;

use cfg::symbol::{Symbol, SymbolSource};

use rs;

use middle::{Ty, AutoTy, Hir, Rule, FoldRule, SymbolicName};
use middle::embedded_string::EmbeddedString;

pub trait FoldHir<S1>: FoldRule<S1>
    where S1: Eq + Hash
{
    fn fold_hir(&mut self, hir: Hir<S1>) -> Hir<Self::Symbol> {
        Hir {
            rules: self.fold_rules(hir.rules),
            type_map: self.fold_type_map(hir.type_map),
            type_equality: hir.type_equality.into_iter().map(|(sym, ty)| {
                (self.fold_symbol(sym), self.fold_ty(ty))
            }).collect(),
            assert_type_equality: RefCell::new(
                hir.assert_type_equality.into_inner().into_iter().map(|(sym, ty)| {
                    (self.fold_symbol(sym), self.fold_ty(ty))
                }).collect()
            ),
            embedded_strings: hir.embedded_strings.into_iter().map(|embedded| {
                EmbeddedString {
                    symbol: self.fold_spanned_symbol(embedded.symbol),
                    string: embedded.string,
                    source: embedded.source,
                }
            }).collect(),
        }
    }

    fn fold_rules(&mut self, rules: Vec<Rule<S1>>) -> Vec<Rule<Self::Symbol>> {
        rules.into_iter().map(|rule| self.fold_rule(rule)).collect()
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
}

pub struct Folder<'a> {
    pub sym_map: HashMap<rs::Ident, Symbol>,
    pub sym_vec: Vec<Option<rs::Ident>>,
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

impl<'a> FoldRule<SymbolicName> for Folder<'a> {
    type Symbol = Symbol;

    fn fold_symbol(&mut self, symbol: SymbolicName) -> Symbol {
        let sym_source = &mut self.sym_source;
        let sym_vec = &mut self.sym_vec;
        *self.sym_map.entry(symbol).or_insert_with(|| {
            let sym = sym_source.sym();
            sym_vec.push(Some(symbol));
            sym
        })
    }
}

impl<'a> FoldHir<SymbolicName> for Folder<'a> {
}
