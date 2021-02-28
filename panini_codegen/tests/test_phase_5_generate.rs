extern crate panini_codegen;

use panini_codegen::phase_5_generate;
use panini_codegen::rs::{Span, Term};

#[test]
fn test_simple() {
    let start = Term::new("start", Span::call_site());
    let a = Term::new("a", Span::call_site());
    let mut tables = Tables::new();
    let lower_instructions = vec![
        LowerInstruction::MakeTerminalAccessorFn { name: start, id: 0 },
        LowerInstruction::MakeTerminalAccessorFn { name: a, id: 1 },
        LowerInstruction::MakeTerminalAccessorStruct { number: 2 },
    ];
    let code = phase_5_generate(&tables, lower_instructions);
    let expected_code = quote! {
        struct TerminalAccessor;

        #[allow(non_snake_case)]
        impl TerminalAccessor {
            fn #start(&self) -> Symbol {
                Symbol::from(0 as u32)
            }

            fn #a(&self) -> Symbol {
                Symbol::from(1 as u32)
            }
        }
    };
}
