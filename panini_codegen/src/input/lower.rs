use panini_logic::input as logic_input;
use panini_logic::input::ast as logic_ast;
use input::ast::*;

pub struct LowerAst {
    tables: Tables,
}

impl LowerAst {
    pub fn new() -> Self {
        LowerAst {
            tables: Tables::new(),
        }
    }

    pub fn rewrite_stmts(&mut self, stmts: Stmts) -> logic_ast::Stmts {
        logic_ast::Stmts {
            attr_arguments: logic_ast::AttrArguments {
                lexer_arguments: 
            },
            stmts: stmts.stmts.into_iter().map(|stmt|
                self.rewrite_stmt(stmt)
            ),
            lexer: 
        }
    }

    pub fn rewrite_stmt(&mut self, stmt: Stmt) -> logic_ast::Stmt {
        logic_ast::Stmt {
            lhs: self.tables.intern(stmt.lhs),
            body: ,
            ty: stmt.ty.map(|ty| self.rewrite_ty(ty)),
        }
    }

    pub fn rewrite_ty(&mut self, ty: rs::TokenStream) -> logic_input::TyId {
        self.tables.intern_ty(ty)
    }
}
