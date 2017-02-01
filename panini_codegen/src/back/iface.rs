struct Gen {
    start: Symbol,
    nullary_product_roots: Vec<NullaryProducts>,
    basic_rules: Vec<Rc<BasicRule>>,
    rules: Vec<Rule>,
    grammar_parts: InternalGrammarParts,
    // assert_type_equality: Vec<(Rc<Ty>, Rc<Ty>)>,
    assert_type_equality: Vec<(ExternalTy, ExternalTy)>,
}

struct Types {
    types: Vec<Vec<Rc<Ty>>,
}

struct Ty {
    tydef: TyDef,
    equivalent: Vec<Rc<Ty>>,
}

enum TyDef {
    External(ExternalTy),
    List(Box<TyDef>),
    Unit,
    Product(Vec<TyDef>),


}

struct Arg {
    pos: usize,
    sym: Symbol,

}

struct Rule {
    // lhs: Symbol,
    basic_rule: Rc<BasicRule>,
    origin_id: u32,

}

struct NullaryProducts {
    lhs: Symbol,
    nullary_products: Vec<NullaryProduct>,
    summands: Option<usize>,
}

struct NullaryProduct {
    basic_rule: Rc<BasicRule>,
    factors: Vec<Symbol>,
}

impl NullaryProduct {
    fn basic_rule(&self) -> &BasicRule {
        &*self.basic_rule
    }
}

struct SymbolInfo {

}
