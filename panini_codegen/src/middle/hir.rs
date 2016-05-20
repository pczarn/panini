use std::collections::HashMap;
use std::mem;
use std::cell::RefCell;

use rs;
use front::{ast, Name};
use front::visit::RhsAstVisitor;
use middle::{ActionExpr, Ty, AutoTy};
use middle::trace::SourceOrigin;

pub struct Hir<S> {
    pub rules: Vec<Rule<S>>,
    pub sequence_rules: Vec<SequenceRule<S>>,
    pub type_map: HashMap<S, Ty<S>>,
    pub type_equality: Vec<(S, Ty<S>)>,
    pub assert_type_equality: RefCell<Vec<(S, Ty<S>)>>,
    pub embedded_strings: Vec<(rs::Spanned<S>, Name, SourceOrigin)>,
}

#[derive(Clone)]
pub struct Rule<S> {
    pub lhs: rs::Spanned<S>,
    pub rhs: Vec<rs::Spanned<S>>,
    pub tuple_binds: Vec<usize>,
    pub deep_binds: Vec<usize>,
    pub shallow_binds: Vec<(usize, rs::ast::Ident)>,
    pub action: ActionExpr,
    pub source_origin: SourceOrigin,
}

#[derive(Clone)]
pub struct SequenceRule<S> {
    pub lhs: rs::Spanned<S>,
    pub rhs: rs::Spanned<S>,
    // sep: Option<rs::Name>,
    pub min: u32,
    pub max: Option<u32>,
    pub action: ActionExpr,
    pub source_origin: SourceOrigin,
}

pub type Symbol = rs::Name;

impl Hir<Symbol> {
    pub fn transform_stmts(stmts: &ast::Stmts) -> Self {
        let mut hir = Hir {
            rules: vec![],
            sequence_rules: vec![],
            type_map: HashMap::new(),
            type_equality: vec![],
            assert_type_equality: RefCell::new(vec![]),
            embedded_strings: vec![],
        };
        let mut rhs_counter = 0;
        for stmt in &stmts.stmts[..] {
            hir.transform_stmt(stmt, &mut rhs_counter);
        }
        hir
    }

    pub fn transform_stmt(&mut self, stmt: &ast::Stmt, rhs_counter: &mut u32) {
        if let Some(ref ty) = stmt.ty {
            self.type_map.insert(stmt.lhs.node, Ty::RustTy(ty.clone()));
        }

        for &(ref rhs, ref action) in &stmt.rhs {
            let rule = self.transform_rhs(stmt.lhs, rhs, *rhs_counter);
            *rhs_counter += 1;

            let (action, ty) = if let Some(ref inline_action) = action.expr {
                self.type_map.entry(rule.lhs.node).or_insert(Ty::Infer);
                let action = ActionExpr::Inline {
                    // bind: self.transform_
                    expr: inline_action.clone(),
                };
                let ty = if let Some(ref ty) = stmt.ty {
                    Ty::RustTy(ty.clone())
                } else {
                    Ty::Infer
                };
                (action, ty)
            } else {
                (ActionExpr::Auto, Ty::Auto(rule.auto_ty))
            };

            if self.type_map.contains_key(&rule.lhs.node) {
                self.type_equality.push((rule.lhs.node, ty));
            } else {
                self.type_map.insert(rule.lhs.node, ty);
            }

            self.rules.push(Rule {
                lhs: rule.lhs,
                rhs: rule.rhs,
                tuple_binds: rule.tuple_binds,
                deep_binds: rule.deep_binds,
                shallow_binds: rule.shallow_binds,
                source_origin: rule.source_origin,
                action: action,
            });
        }
    }

    fn transform_rhs(&mut self, lhs: Name, rhs: &ast::Rhs, rhs_counter: u32) -> FlatRule {
        let mut visitor = FlattenRhsAst::new(lhs, rhs_counter);
        visitor.visit_rhs(rhs);
        // The top rule `lhs ::= rhs`.
        let top_rule = visitor.rule_stack.pop().expect("one remaining rule expected");
        assert!(visitor.rule_stack.is_empty(), "only one remaining rule expected");


        for new_rule in visitor.new_rules.drain(..) {
            let new_auto_ty = Ty::Auto(new_rule.auto_ty);
            if self.type_map.contains_key(&new_rule.lhs.node) {
                self.type_equality.push((new_rule.lhs.node, new_auto_ty));
            } else {
                self.type_map.insert(new_rule.lhs.node, new_auto_ty);
            }
            self.rules.push(Rule {
                lhs: new_rule.lhs,
                rhs: new_rule.rhs,
                tuple_binds: new_rule.tuple_binds,
                shallow_binds: new_rule.shallow_binds,
                deep_binds: new_rule.deep_binds,
                source_origin: new_rule.source_origin,
                action: ActionExpr::Auto,
            });
        }

        for new_sequence in visitor.new_sequences.drain(..) {
            let new_auto_ty = Ty::SequenceVec(new_sequence.rhs.node);
            if self.type_map.contains_key(&new_sequence.lhs.node) {
                self.type_equality.push((new_sequence.lhs.node, new_auto_ty));
            } else {
                self.type_map.insert(new_sequence.lhs.node, new_auto_ty);
            }
            self.sequence_rules.push(SequenceRule {
                lhs: new_sequence.lhs,
                rhs: new_sequence.rhs,
                min: new_sequence.min,
                max: new_sequence.max,
                source_origin: new_sequence.source_origin,
                action: ActionExpr::Auto,
            })
        }

        self.embedded_strings.extend(visitor.embedded_strings.into_iter());

        top_rule
    }

    pub fn check_type_equality(&mut self) -> bool {
        let type_equality = mem::replace(&mut self.type_equality, vec![]);
        for (sym, ty) in type_equality {
            if !self.ty_equal(sym, &ty) {
                return false;
            }
        }
        true
    }

    fn sym_ty_equal(&self, left: Symbol, right: Symbol) -> bool {
        // Prevent deep comparison of same types.
        if left == right {
            return true;
        }
        match (self.type_map.get(&left), self.type_map.get(&right)) {
            (Some(_), Some(right_ty)) => {
                self.ty_equal(left, right_ty)
            }
            (None, None) => {
                // Terminals.
                true
            }
            _ => {
                false
            }
        }
    }

    fn ty_equal(&self, left: Symbol, right: &Ty<Symbol>) -> bool {
        let left_ty = &self.type_map[&left];
        match (left_ty, right) {
            (&Ty::Auto(ref l_auto), &Ty::Auto(ref r_auto)) => {
                self.auto_ty_equal(l_auto, r_auto)
            }
            (&Ty::SequenceVec(l_sym), &Ty::SequenceVec(r_sym)) => {
                return self.sym_ty_equal(l_sym, r_sym);
            }
            (&Ty::RustTy(_), ty) | (_, ty @ &Ty::RustTy(_)) => {
                self.assert_type_equality.borrow_mut().push((left, ty.clone()));
                return true;
            }
            (&Ty::Infer, _) | (_, &Ty::Infer) => {
                return true;
            }
            _ => {
                return false;
            }
        }
    }

    fn auto_ty_equal(&self, left: &AutoTy<Symbol>, right: &AutoTy<Symbol>) -> bool {
        match (left, right) {
            (&AutoTy::Tuple { fields: ref left_syms },
             &AutoTy::Tuple { fields: ref right_syms }) => {
                for (&left_sym, &right_sym) in left_syms.iter().zip(right_syms.iter()) {
                    if !self.sym_ty_equal(left_sym, right_sym) {
                        return false;
                    }
                }
                left_syms.len() == right_syms.len()
            }
            (&AutoTy::Struct { members: ref left },
             &AutoTy::Struct { members: ref right }) => {
                for ((l_pat, &l_sym), (r_pat, &r_sym)) in left.iter().zip(right.iter()) {
                    if l_pat != r_pat || !self.sym_ty_equal(l_sym, r_sym) {
                        return false;
                    }
                }
                left.len() == right.len()
            }
            _ => {
                false
            }
        }
    }
}

// Flattening the RHS AST

struct FlattenRhsAst {
    rule_stack: Vec<FlatRule>,
    new_rules: Vec<FlatRule>,
    new_sequences: Vec<FlatSequence>,
    embedded_strings: Vec<(Name, Name, SourceOrigin)>,
    rule_id: u32,
    cur_rule_pos: u32,
}

struct FlatRule {
    lhs: Name,
    rhs: Vec<Name>,
    // Indexes are RHS positions.
    tuple_binds: Vec<usize>,
    deep_binds: Vec<usize>,
    shallow_binds: Vec<(usize, rs::ast::Ident)>,
    auto_ty: AutoTy<rs::Name>,
    source_origin: SourceOrigin,
}

// The AutoTy for a Sequence is a Vec<type of rhs>.
struct FlatSequence {
    lhs: Name,
    rhs: Name,
    min: u32,
    max: Option<u32>,
    source_origin: SourceOrigin,
}

impl FlatRule {
    fn new(lhs: Name, rule_id: u32, rhs_start_pos: u32) -> Self {
        FlatRule {
            lhs: lhs,
            rhs: vec![],
            tuple_binds: vec![],
            deep_binds: vec![],
            shallow_binds: vec![],
            auto_ty: AutoTy::Tuple { fields: vec![] },
            source_origin: SourceOrigin {
                rule_id: rule_id,
                rule_pos: vec![rhs_start_pos],
            }
        }
    }
}

impl FlattenRhsAst {
    fn new(lhs: Name, rule_id: u32) -> Self {
        FlattenRhsAst {
            rule_stack: vec![FlatRule::new(lhs, rule_id, 0)],
            new_rules: vec![],
            new_sequences: vec![],
            embedded_strings: vec![],
            rule_id: rule_id,
            cur_rule_pos: 0,
        }
    }

    fn append_symbol(&mut self, symbol: rs::Spanned<rs::Name>, is_deep: bool) {
        let current_rule = self.rule_stack.last_mut().unwrap();
        current_rule.rhs.push(symbol);
        current_rule.source_origin.rule_pos.push(self.cur_rule_pos);
        if is_deep {
            let sym_pos = current_rule.rhs.len() - 1;
            current_rule.deep_binds.push(sym_pos);
        }
    }
}

impl RhsAstVisitor for FlattenRhsAst {
    // Reversed order of visitation.
    fn walk_rhs_element(&mut self, rhs_elem: &ast::RhsElement) {
        self.visit_rhs_ast(&rhs_elem.elem);
        // Binds must be visited later, because they access the rule that was
        // constructed before.
        self.visit_bind(&rhs_elem.bind);
    }

    fn visit_rhs_symbol(&mut self, symbol: Name) {
        self.cur_rule_pos += 1;
        self.append_symbol(symbol, false);
    }

    fn visit_sequence(&mut self, sequence: &ast::Sequence) {
        // TODO prevent duplication of identical sequences
        let rule_lhs = rs::dummy_spanned(rs::gensym("G"));
        let seq_lhs = rs::dummy_spanned(rs::gensym("G"));
        // The inner rule goes to `rule_stack`.
        // Walk through `sequence.rhs`, collecting the contents to `rule_lhs ::= ...`.
        let older_rule_pos = self.cur_rule_pos;
        self.rule_stack.push(FlatRule::new(rule_lhs, self.rule_id, self.cur_rule_pos));
        self.walk_sequence(sequence);
        self.new_sequences.push(FlatSequence {
            lhs: seq_lhs,
            rhs: rule_lhs,
            min: sequence.min,
            max: sequence.max,
            source_origin: SourceOrigin {
                rule_id: self.rule_id,
                rule_pos: vec![older_rule_pos, self.cur_rule_pos],
            }
        });
        self.cur_rule_pos += 1;
        // Take the inner rule.
        let new_rule = self.rule_stack.pop().unwrap();
        if new_rule.rhs.len() == 1 {
            // Optimize out a rule with a single RHS symbol.
            // TODO: deep binds in sequences with iterators
            let sym = new_rule.rhs[0];
            self.new_sequences.iter_mut().find(|seq| seq.lhs == seq_lhs).unwrap().rhs = sym;
        } else {
            // The inner rule goes to `new_rules`.
            self.new_rules.push(new_rule);
        }
        self.append_symbol(seq_lhs, false);
    }

    fn visit_sum(&mut self, sum: &[ast::Rhs]) {
        let new_lhs = rs::dummy_spanned(rs::gensym("GS"));
        for rule in sum {
            self.rule_stack.push(FlatRule::new(new_lhs, self.rule_id, self.cur_rule_pos));
            // Don't visit product, because it would add an intermediate
            // symbol that we don't need. Visiting rhs will append multiple
            // symbols directly to the pushed rule.
            self.visit_rhs(rule);
            self.new_rules.push(self.rule_stack.pop().unwrap());
            self.cur_rule_pos += 1;
        }
        if !sum.is_empty() {
            self.cur_rule_pos -= 1;
        }
        self.append_symbol(new_lhs, true);
    }

    fn visit_product(&mut self, product: &ast::Rhs) {
        let new_lhs = rs::dummy_spanned(rs::gensym("GP"));
        // The inner rule goes to `rule_stack`.
        self.rule_stack.push(FlatRule::new(new_lhs, self.rule_id, self.cur_rule_pos));
        self.cur_rule_pos += 1;
        // Go through the sub-RHS, collecting the contents to `new_lhs ::= ...`.
        self.walk_product(product);
        self.cur_rule_pos += 1;
        // The inner rule goes to `new_rules`.
        self.new_rules.push(self.rule_stack.pop().unwrap());
        self.append_symbol(new_lhs, true);
    }

    fn visit_rhs_string(&mut self, string: Name) {
        let new_sym = rs::dummy_spanned(rs::gensym("GTerm"));
        self.embedded_strings.push((
            new_sym,
            string,
            SourceOrigin {
                rule_id: self.rule_id,
                rule_pos: vec![self.cur_rule_pos, self.cur_rule_pos + 1],
            }
        ));
        self.cur_rule_pos += 1;
        self.append_symbol(new_sym, false);
    }

    fn visit_bind(&mut self, bind: &Option<rs::P<rs::Pat>>) {
        let current_rule = self.rule_stack.last_mut().unwrap();
        let num_syms = current_rule.rhs.len() - 1;
        let last_sym = *current_rule.rhs.last().unwrap();
        if let &Some(ref pat) = bind {
            match &pat.node {
                &rs::PatKind::Wild => {}
                &rs::PatKind::Ident(_mode, spanned_ident, None) => {
                    let bind_name = spanned_ident.node;
                    current_rule.shallow_binds.push((num_syms, bind_name));
                    match current_rule.auto_ty {
                        AutoTy::Tuple { .. } => {
                            let mut members = HashMap::new();
                            members.insert(bind_name, last_sym.node);
                            current_rule.auto_ty = AutoTy::Struct {
                                members: members,
                            };
                        }
                        AutoTy::Struct { ref mut members } => {
                            members.insert(bind_name, last_sym.node);
                        }
                    }
                }
                _ => panic!("unsupported pattern"),
            }
        } else {
            current_rule.tuple_binds.push(num_syms);
            match current_rule.auto_ty {
                AutoTy::Tuple { ref mut fields } => {
                    fields.push(last_sym.node);
                }
                _ => {}
            }
        }
    }
}
