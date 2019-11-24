use input::FragmentId;
use middle::flatten_stmts::Path;

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum OuterSymbol {
    Fragment(FragmentId),
    FromPath(Path),
}

#[derive(Clone)]
pub struct SymbolMaps {
    sym_map: HashMap<OuterSymbol, Symbol>,
    sym_vec: Vec<Option<OuterSymbol>>,
    internal_external: Option<Mapping>,
}

impl SymbolMaps {
    fn new() -> Self {
        SymbolMaps {
            sym_map: HashMap::new(),
            sym_vec: vec![],
            internal_external: None,
        }
    }

    pub fn intern(&mut self, grammar: &mut Grammar, outer: &OuterSymbol) -> Symbol {
        if let Some(&symbol) = self.sym_map.get(outer) {
            symbol
        } else {
            let new_sym: Symbol = grammar.sym();
            self.insert(new_sym, outer.clone());
            new_sym
        }
    }

    fn insert(&mut self, sym: Symbol, outer_symbol: OuterSymbol) {
        self.insert_padding(sym);
        self.sym_vec[sym.usize()] = Some(outer_symbol.clone());
        self.sym_map.insert(outer_symbol, sym);
    }

    fn insert_padding(&mut self, outer_symbol: Symbol) {
        if self.sym_vec.len() <= outer_symbol.usize() {
            let pad_len = outer_symbol.usize() - self.sym_vec.len() + 1;
            self.sym_vec.extend(iter::repeat(None).take(pad_len));
        }
    }

    fn get(&self, external_sym: Symbol) -> Option<OuterSymbol> {
        self.sym_vec.get(external_sym.usize()).and_then(|s| s.clone())
    }

    pub fn syms(&self) -> &[Option<OuterSymbol>] {
        &self.sym_vec[..]
    }

    pub fn internalize(&self, external_sym: Symbol) -> Option<Symbol> {
        self.internal_external.unwrap().to_internal[external_sym.usize()]
    }

    pub fn externalize(&self, internal_sym: Symbol) -> Symbol {
        self.internal_external.unwrap().to_external[internal_sym.usize()]
    }

    pub fn sym_of_external(&self, sym: Symbol) -> Option<OuterSymbol> {
        self.sym_map.get(sym)
    }

    fn sym_map(&self) -> &HashMap<OuterSymbol, Symbol> {
        &self.sym_map
    }
}
