enum Instruction {
    Concat(usize),
    Literal(rs::TokenStream),
    LiteralList(Vec<rs::TokenStream>),

    MakeEpsilonRootMatchArm {

    }

    // args:
    // - action code
    MakeEpsilonRuleActionDefinition {
        name: rs::Term,
        continuation_label: rs::Term,
    }

    // TODO maybe arg: type? (more flexibility)
    MakeEnumVariant {
        name: rs::Term,
        ty: rs::TokenStream,
    },

    MakeValueEnum {
        Value: rs::Ident,
        Infer: rs::Ident,
    },

    MakeTerminalAccessorFn {
        id: usize,
        name: rs::Term,
    },

    // args:
    // - fn code
    MakeTerminalAccessorStruct {
        TerminalAccessor: rs::Ident,
    },

    OutermostLayer,

    // equivalent: concat 4
    LexerLayer,
}

struct InterpreterState {
  stack: Vec<rs::TokenStream>,
}

fn run_instruction(instruction: Instruction, state: &mut InterpreterState) {
    let result = match instruction {
        // terminal accessor
        MakeTerminalAccessorFn { name, id } => {
            // Id is internal symbol id.
            quote! {
                #[inline]
                fn #name(&self) -> Symbol {
                    Symbol::from(#id as u32)
                }
            }
        }
        MakeTerminalAccessorStruct { TerminalAccessor } => {
            let fns = state.pop();
            quote! {
                struct #TerminalAccessor;

                #[allow(non_snake_case)]
                impl #TerminalAccessor {
                    #fns
                }
            }
        }

        // value enum
        MakeEnumVariant { name, ty } => {
            quote! {
                #name(#ty),
            }
        }
        MakeValueEnum { Value, Infer } => {
            let variants = state.pop();
            quote! {
                #[derive(Clone)]
                #[allow(non_camel_case_types)]
                enum {{Value}}<I> where I: {{Infer}} {
                    #variants
                }
            }
        }

        // epsilon rules
        MakeEpsilonRuleActionDefinition { name, continuation_label } => {
            let actions = state.pop();
            macro_rules! #name {
                ($x:expr) => {{
                    let mut #continuation_label = $x;
                    #actions
                }}
            }
        }

        // final expression
        OutermostLayer => {
            // let common_defs = state.pop();
            // let lexer_def = state.pop();
            // let parse_builder_def = state.pop();
            // let parse_def = state.pop();
            // let parse_builder = state.pop();
            let defs = state.pop();
            quote! (
                { use ::panini::*; #defs }
            )
        }
        LexerLayer => {
            // let n = state.stack.len().checked_sub(4).expect("incorrect number of args");
            // state.stack.drain(n..).fold(vec![], |mut acc, elem| {
            //     acc.extend_from_slice(&elem[..]);
            //     acc
            // })
            let defs = state.pop();
            quote! (
                { #defs }
            )
        }
    };
    state.stack.push(result);
}

fn run(instructions: Vec<Instruction>) -> rs::TokenStream {
    let mut state = InterpreterState {
        stack: vec![],
    };
    for instruction in instructions {
        run_instruction(instruction, &mut state);
    }
    let result = state.stack.pop().expect("interpreter expected at least one result");
    assert!(state.stack.empty(), "interpreter expected exactly one result");
}

#[test]
fn test_outer_layer() {
    let code = vec![
        MakeTerminalAccessorFn { name, id },
        MakeTerminalAccessorFn { name, id },
        Concat(2),
        MakeTerminalAccessorStruct { TerminalAccessor },

        MakeEnumVariant { name, ty },
        MakeEnumVariant { name, ty },
        Concat(2),
        MakeValueEnum { Value, Infer },

        Literal(x),
        Literal(x),
        Concat(2),
        MakeEpsilonRuleActionDefinition { name, continuation_label },

        MakeSerializedGrammarDefinition { SERIALIZED_GRAMMAR, storage_str },

        LiteralVec(trace_ids),
        ConcatSeparated(trace_ids.len(), comma),
        LiteralVec(trace_map1),
        ConcatSeparated(trace_map1.len(), comma),
        MakeStaticArray,
        LiteralVec(trace_map2),
        ConcatSeparated(trace_map2.len(), comma),
        MakeStaticArray,
        ConcatSeparated(2, comma),
        LiteralVec(trace_tokens1),
        ConcatSeparated(trace_tokens1.len(), comma),
        MakeStaticArray,
        LiteralVec(trace_tokens2),
        ConcatSeparated(trace_tokens2.len(), comma),
        MakeStaticArray,
        ConcatSeparated(2, comma),
        LiteralVec(sym_names),
        ConcatSeparated(sym_names.len(), comma),
        MakeTraceInfoDefinition,

        MakeInternalGrammar {
            SERIALIZED_GRAMMAR,
            num_syms,
            num_rules,
            num_external_syms,
            num_internal_syms,
            num_nulling_intermediate,
            start_sym,
            trivial_derivation,
        }
        MakeTopLevelParseFactory {
            ParseFactory,
            TerminalAccessor,
            Parse,
            InferTree,
            Value,
        }
        MakeAssociatedType { infer_name },
        MakeAssociatedType { infer_name },
        Concat(2),
        MakeInferTrait,
        LiteralVec(infer_names),
        ConcatSeparated(infer_names.len(), comma),
        MakeAssociatedTypeSetTo { lhs, rhs },
        MakeAssociatedTypeSetTo { lhs, rhs },
        Concat(2),
        // args: - - 
        MakeValueInfer { Infer, ValueInfer },
        MakeInferTreeWithVal {
            Infer,
            InferTree,
            InferTreeVal,
            LowerInferConstraint,
        },
        MakeParseDefinition {
            Parse,
            InferTree,
            Value,
            start_type,
            start_variant,
        },
        LiteralVec(terminal_names),
        ConcatSeparated(terminal_names.len(), comma),
        Literal(str_rhs_expr1),
        MakeBnfRule { lhs },
        Literal(str_rhs_expr2),
        MakeBnfRule { lhs },
        MakeCharRange(start, end),
        MakeBnfRule { lhs },
        MakeCharRange(start, end),
        MakeBnfRule { lhs },
        Concat(4),
        Literal(lexer_tts),
        MakeLowerLayerInvocation {  
            lexer_name,
            lexer_attr,
        },

        // traversal closure

        Literal(wildcard),
        Repeat(n),
        ConcatSeparated(n, comma),

        MakeValueAccess { arg_num },
        MakeValueAccess { arg_num },
        ConcatSeparated(2, comma),
        MakeValuePat {
            Value,
            arg_variant1,
            arg_pat1,
        }
        MakeValuePat {
            Value,
            arg_variant2,
            arg_pat2,
        }
        ConcatSeparated(2, comma),
        MakeRuleActionMatchArm {
            action_id,
            Value,
            rule_variant,
            rules_expr,
        }
        Concat(n_arms),
        Concat(n_seq_arms),
        MakeEpsilonRuleMatchArm {
            Value,
            null_symbol_id,
            null_num_summands,
            null_sym_name,
            null_variant,
        }
        MakeTraversalClosure {
            traversal,
            store,
            lower_layer_macro,
        },

        MakeLowerLayerParseBuilderInvocation {
            ParseFactory,
            Parse,
            InferTreeVal,
            ValueInfer,
            lower_layer_macro,
        }

        Concat(n),
        OutermostLayer,
    ];
}

#[test]
fn test_inner_layer() {
    let code = vec![
        MakeTerminalAccessorFn { name, id },
        MakeTerminalAccessorFn { name, id },
        Concat(2),
        MakeTerminalAccessorStruct { TerminalAccessor },

        MakeEnumVariant { name, ty },
        MakeEnumVariant { name, ty },
        Concat(2),
        MakeValueEnum { Value, Infer },

        Literal(),
        Literal(),
        Concat(2),
        MakeEpsilonRuleActionDefinition { name, continuation_label },

        MakeParseFactory {

        }

        Concat(n),
        LexerLayer,
    ];
}
