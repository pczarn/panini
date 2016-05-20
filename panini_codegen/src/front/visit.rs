use super::ast::*;
use rs;
use front::Name;

pub trait RhsAstVisitor {
    // Visit

    fn visit_rhs_ast(&mut self, rhs: &RhsAst) {
        self.walk_rhs_ast(rhs);
    }

    fn visit_rhs(&mut self, rhs: &Rhs) {
        self.walk_rhs(rhs);
    }

    fn visit_rhs_element(&mut self, rhs_elem: &RhsElement) {
        self.walk_rhs_element(rhs_elem);
    }

    fn visit_bind(&mut self, _bind: &Option<rs::P<rs::Pat>>) {
        // nothing to do.
    }

    fn visit_sequence(&mut self, sequence: &Sequence) {
        self.walk_sequence(sequence);
    }

    fn visit_sum(&mut self, sum: &[Rhs]) {
        self.walk_sum(sum);
    }

    fn visit_product(&mut self, product: &Rhs) {
        self.walk_product(product);
    }

    fn visit_rhs_symbol(&mut self, _sym: Name) {
        // nothing to do.
    }

    fn visit_rhs_string(&mut self, _string: Name) {
        // nothing to do.
    }

    // Walk

    fn walk_stmts(&mut self, stmts: &Stmts) {
        for stmt in &stmts.stmts {
            self.walk_stmt(stmt);
        }
    }

    fn walk_stmt(&mut self, stmt: &Stmt) {
        for &(ref rhs, ref _action) in &stmt.rhs {
            self.visit_rhs(rhs);
        }
    }

    fn walk_rhs_ast(&mut self, rhs_ast: &RhsAst) {
        match rhs_ast {
            &RhsAst::Symbol(name) => {
                self.visit_rhs_symbol(name);
            }
            &RhsAst::Sequence(ref sequence) => {
                self.visit_sequence(sequence);
            }
            &RhsAst::Sum(ref summands) => {
                self.visit_sum(&summands[..]);
            }
            &RhsAst::Product(ref rhs) => {
                self.visit_product(rhs);
            }
            &RhsAst::String(string) => {
                self.visit_rhs_string(string);
            }
        }
    }

    fn walk_rhs(&mut self, rhs: &Rhs) {
        for rhs_elem in &rhs.0 {
            self.visit_rhs_element(rhs_elem);
        }
    }

    fn walk_rhs_element(&mut self, rhs_elem: &RhsElement) {
        self.visit_bind(&rhs_elem.bind);
        self.visit_rhs_ast(&rhs_elem.elem);
    }

    fn walk_sequence(&mut self, sequence: &Sequence) {
        self.visit_rhs(&sequence.rhs);
    }

    fn walk_sum(&mut self, sum: &[Rhs]) {
        for summand in sum {
            self.visit_rhs(summand);
        }
    }

    fn walk_product(&mut self, rhs: &Rhs) {
        self.visit_rhs(rhs);
    }
}
