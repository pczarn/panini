extern crate cfg;
extern crate gearley;
extern crate panini_codegen;

use panini_codegen::front::ast::Stmts;
use panini_codegen::middle::ir;
use panini_codegen::middle::error::TransformationError;

#[test]
fn test_empty_grammar_ir() {
    let before = Stmts {
        attrs: vec![],
        stmts: vec![],
        lexer: None,
    };
    let result = ir::IrMapped::transform_from_stmts(before);
    // Check
    match result {
        Err(TransformationError::GrammarIsEmpty) => {}
        _ => panic!("expected transformation error")
    }
}
