use cfg::Symbol;
use enum_coder::enum_coder;

use output::translator::IrTranslator;

#[derive(Debug, Eq, PartialEq)]
pub struct InstructionList {
    pub list: Vec<Instruction>,
}

impl InstructionList {
    pub fn new() -> Self {
        InstructionList {
            list: vec![]
        }
    }
}

enum_coder! {
    #[derive(Debug, Eq, PartialEq)]
    pub enum Instruction {
        // Concat(usize),
        // ConcatSeparated(usize, rs::TokenStream),
        // Dup,

        MakeTerminalAccessorFn {
            terminal: Symbol,
        },

        MakeTerminalAccessorStruct {
            number: usize,
            map: HashMap<FragmentId, Symbol>,
        },

        // // defines:
        // //
        // // impl EnumStreamParser<C, D> {
        // //     fn common_parse<Iter>(&'g mut self, into_iter: Iter, traced: bool) 
        // // }
        // MakeEnumStreamParserCommonParseImpl {
        //     UpperParse: rs::Ident,
        //     UpperInferTree: rs::Ident,
        // },

        // MakeValueConditionally {
        //     UpperValue: rs::Ident,
        //     terminal_id: usize,
        //     variant: rs::Ident,
        // },

        // MakeNegativePatternMatchArm {
        //     pattern: rs::Pat,
        // },

        // MakePositiveMatch {
        //     patterns: Vec<rs::Pat>,
        //     guard: Option<rs::Expr>,
        // },

        // MakeTerminalAction {
        //     terminal_id: usize,
        // },

        // MakeLayerMacroDefinition {
        //     layer_macro: rs::Ident,
        //     UpperTerminalAccessor: rs::Ident,
        // },
    }

    // #[missing_field(ir)]
    // fn make_ident(ir: &Ir, field_name: &str) -> rs::Ident {
    //     let upper_id = ir.level - 1;
    //     rs::str_to_ident(format!("{}{}", field_name, upper_id))
    // }
    // #[missing_field(ir)]
    // fn make_ident(ir: &Ir, field_name: &str) -> rs::Ident {
    //     let id = if field_name.starts_with("Lower") || field_name.starts_with("lower") {
    //         current_id + 1
    //     } else if field_name.starts_with("Upper") {
    //         if current_id == 0 { !0 } else { current_id - 1 }
    //     } else {
    //         current_id
    //     };
    //     rs::Ident::new(&*format!("{}{}", field_name, id), rs::Span::call_site())
    // }

    #[generate_list]
    pub fn translate_ir(translator: &mut IrTranslator) -> Vec<Instruction> {
        // MakeCommonDefinitions();
        for &terminal in &translator.terminals {
            MakeTerminalAccessorFn {
                terminal,
            }
        }
        let map = translator.ir.sym_map().sym_map.iter().filter_map(|key, value| {
            match key {
                &Sym::Fragment(fragment_id) => {
                    Some((fragment_id, value))
                }
                &Sym::FromPath => None
            }
        }).collect();
        MakeTerminalAccessorStruct {
            number: translator.terminals.len(),
            map,
        }

        // ConcatSeparated(ir.rules.len(), quote! { , });
        // Dup();
        // MakeEnumStreamParserCommonParseImpl();
        // for (n, rule) in ir.rules.iter().enumerate() {
        //     let capitalized = capitalize(&rule.name.to_string()[..]);
        //     let variant = rs::str_to_ident(&capitalized[..]);
        //     MakeValueConditionally {
        //         terminal_id: n,
        //         variant,
        //     };
        // }
        // Concat(ir.rules.len());
        // for (n, rule) in ir.rules.iter().enumerate() {
        //     for negative_pattern in &rule.negative {
        //         MakeNegativePatternMatchArm {
        //             pattern: negative_pattern.clone()
        //         }
        //     }
        //     Concat(rule.negative.len());
        //     MakePositiveMatch {
        //         patterns: rule.positive.clone(),
        //         guard: rule.guard.clone(),
        //     };
        //     MakeTerminalAction { terminal_id: n }
        // }
        // Concat(ir.rules.len());
        // MakeLayerMacroDefinition();
    }
}
