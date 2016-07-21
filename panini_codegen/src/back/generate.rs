#![allow(non_snake_case)]

use std::iter;

use aster::AstBuilder;
use aster::ident::ToIdent;

use cfg::symbol::Symbol;
use gearley::grammar::InternalGrammarParts;

use rs;
use middle::LowerLevel;
use back::IrTranslator;

pub struct GenParser<'a> {
    pub trans: &'a IrTranslator,
    pub grammar_parts: InternalGrammarParts,
    pub null: GenNulling,
    pub rules: Vec<GenRule>,
    pub sequences: Vec<GenSequence>,
    pub unique_names: UniqueNames,
}

// Nulling rules

pub struct GenNulling {
    pub rules: Vec<GenNullingRule>,
    pub roots: Vec<GenNullingRoot>,
    pub continuation_label: rs::ast::Ident,
}

pub struct GenNullingRule {
    pub name: rs::ast::Ident,
    pub blocks: Vec<rs::P<rs::Block>>,
}

pub struct GenNullingRoot {
    pub sym: Symbol,
    pub num: usize,
    pub name: rs::ast::Ident,
}

// Rules

pub struct GenRule {
    pub id: u32,
    pub variant: rs::ast::Ident,
    pub action: rs::P<rs::Expr>,
    pub args: Vec<GenArg>,
}

pub struct GenArg {
    pub num: usize,
    pub variant: rs::ast::Ident,
    pub pat: rs::P<rs::Pat>,
}

// Sequcene rules

pub struct GenSequence {
    pub elem_variant: rs::ast::Ident,
    pub variant: rs::ast::Ident,
}

#[derive(Copy, Clone)]
pub struct UniqueNames {
    Value: rs::ast::Ident,
    ValueInfer: rs::ast::Ident,
    pub Infer: rs::ast::Ident,
    Layer: rs::ast::Ident,
    ParseFactory: rs::ast::Ident,
    Parse: rs::ast::Ident,
    TracedParse: rs::ast::Ident,
    LayerParam: rs::ast::Ident,
    Params: rs::ast::Ident,
    TerminalAccessor: rs::ast::Ident,
    EvalArg: rs::ast::Ident,
    SERIALIZED_GRAMMAR: rs::ast::Ident,
    UpperValue: rs::ast::Ident,
    UpperParse: rs::ast::Ident,
    UpperParseFactory: rs::ast::Ident,
    UpperLayerParam: rs::ast::Ident,
    UpperEvalArg: rs::ast::Ident,
    LowerEvalArg: rs::ast::Ident,
    UpperTerminalAccessor: rs::ast::Ident,
    UpperInfer: rs::ast::Ident,
    LowerInfer: rs::ast::Ident,
    InferTree: rs::ast::Ident,
    InferTreeVal: rs::ast::Ident,
    UpperInferTree: rs::ast::Ident,
    InferConstraint: rs::ast::Ident,
    LowerInferConstraint: rs::ast::Ident,
    layer_macro: rs::ast::Ident,
    lower_layer_macro: rs::ast::Ident,
}

impl UniqueNames {
    pub fn new(current_id: usize) -> Self {
        let upper_id = if current_id == 0 { !0 } else { current_id - 1 };
        let lower_id = current_id + 1;

        UniqueNames {
            ValueInfer: rs::str_to_ident(&*format!("ValueInfer{}", current_id)),
            Value: rs::str_to_ident(&*format!("Value{}", current_id)),
            Infer: rs::str_to_ident(&*format!("Infer{}", current_id)),
            Layer: rs::str_to_ident(&*format!("Layer{}", current_id)),
            ParseFactory: rs::str_to_ident(&*format!("ParseFactory{}", current_id)),
            Parse: rs::str_to_ident(&*format!("Parse{}", current_id)),
            TracedParse: rs::str_to_ident(&*format!("TracedParse{}", current_id)),
            LayerParam: rs::str_to_ident(&*format!("LayerParam{}", current_id)),
            Params: rs::str_to_ident(&*format!("Params{}", current_id)),
            TerminalAccessor: rs::str_to_ident(&*format!("TerminalAccessor{}", current_id)),
            EvalArg: rs::str_to_ident(&*format!("EvalArg{}", current_id)),
            SERIALIZED_GRAMMAR: rs::str_to_ident(&*format!("SERIALIZED_GRAMMAR{}", current_id)),
            UpperValue: rs::str_to_ident(&*format!("Value{}", upper_id)),
            UpperParse: rs::str_to_ident(&*format!("Parse{}", upper_id)),
            UpperParseFactory: rs::str_to_ident(&*format!("ParseFactory{}", upper_id)),
            UpperLayerParam: rs::str_to_ident(&*format!("LayerParam{}", upper_id)),
            UpperEvalArg: rs::str_to_ident(&*format!("EvalArg{}", upper_id)),
            LowerEvalArg: rs::str_to_ident(&*format!("EvalArg{}", lower_id)),
            layer_macro: rs::str_to_ident(&*format!("layer_macro{}", current_id)),
            lower_layer_macro: rs::str_to_ident(&*format!("layer_macro{}", lower_id)),
            UpperTerminalAccessor: rs::str_to_ident(&*format!("TerminalAccessor{}", upper_id)),
            UpperInfer: rs::str_to_ident(&*format!("Infer{}", upper_id)),
            LowerInfer: rs::str_to_ident(&*format!("Infer{}", lower_id)),
            InferTree: rs::str_to_ident(&*format!("InferTree{}", current_id)),
            InferTreeVal: rs::str_to_ident(&*format!("InferTreeVal{}", current_id)),
            UpperInferTree: rs::str_to_ident(&*format!("InferTree{}", upper_id)),
            InferConstraint: rs::str_to_ident(&*format!("InferConstraint{}", current_id)),
            LowerInferConstraint: rs::str_to_ident(&*format!("InferConstraint{}", lower_id)),
        }
    }
}

pub enum GenResult {
    Parser(rs::P<rs::Expr>),
    Lexer(Vec<rs::Stmt>),
}

impl<'a> GenParser<'a> {
    pub fn translate(&self, cx: &mut rs::ExtCtxt) -> GenResult {
        let common_defs = self.translate_common_defs(cx);
        let UniqueNames {
            UpperValue, Value, Layer, UpperTerminalAccessor,
            ValueInfer, InferTreeVal,
            layer_macro, lower_layer_macro, ..
        } = self.unique_names;
        let dol = rs::TokenTree::Token(rs::DUMMY_SP, rs::Token::Dollar);

        if self.trans.ir.lexer_for_upper.is_some() {
            let lexer_builder_def = self.translate_lexer_builder_def(cx);
            let lexer_lexer_def = self.translate_lexer_def(cx);
            let traversal = quote_tokens! {cx, $lower_layer_macro!(@get $dol parse).traversal};
            let store = quote_tokens! {cx, $lower_layer_macro!(@get $dol parse).store};
            let closure = self.translate_closure(cx, traversal, store);

            let lexer_for_upper = self.trans.ir.lexer_for_upper.as_ref().unwrap();
            let upper_terminal = lexer_for_upper.terminals().iter().map(|&sym|
                self.trans.ir.name_of_external(sym).unwrap().to_ident()
            );
            let variant = lexer_for_upper.terminals().iter().map(|&sym|
                self.trans.variant_names[&sym].name.as_str()
            ).collect::<Vec<_>>();
            let bare_variant = variant.clone().into_iter().map(|name|
                (&name[.. name.rfind("_").unwrap()]).to_ident()
            );
            let infer_wildcards = iter::repeat(AstBuilder::new().ty().infer())
                                  .take(self.trans.infer.len());
            let variant = variant.into_iter().map(|s| (&*s).to_ident());

            let block = quote_block!(cx, {
                // ########### QUOTED CODE
                $common_defs
                $lexer_builder_def
                $lexer_lexer_def

                macro_rules! $layer_macro {
                    (
                        @closure
                        $dol upper_builder:expr,
                        $dol parse:expr,
                        $dol node:expr;
                    ) => ({
                        // Assist the inference.
                        {
                            let _: &::std::marker::PhantomData<
                                $InferTreeVal<_, _, $ValueInfer<_, $($infer_wildcards),*>>
                            > = &$lower_layer_macro!(@get $dol parse).inference_marker;
                        };
                        let upper_builder = &mut $dol upper_builder;
                        let sym = ($dol node).terminal;
                        let root = ($dol node).value;
                        // === Deeper code
                        $closure
                        // === Result
                        let result = match root.get() {
                            Evaluated { values } => values,
                            _ => unreachable!()
                        };
                        upper_builder.reserve(result.len());
                        let upper_terminals = $UpperTerminalAccessor;
                        $(
                            if sym == upper_terminals.$upper_terminal() {
                                for value in result {
                                    let inner = if let $Value::$variant(inner) = value.clone() {
                                        inner
                                    } else {
                                        unreachable!()
                                    };
                                    upper_builder.push($UpperValue::$bare_variant(inner));
                                }
                            }
                        )else*
                        else {
                            unreachable!("wrong sym")
                        }
                    });
                    (@builder @factory [$dol factory:expr] @closure [$dol closure:expr]) => (
                        $lower_layer_macro!(@builder
                            @factory [$Layer::new().with_parse_builder($dol factory)]
                            @closure [$dol closure]
                        )
                    );
                    (@get $dol parse:expr) => (
                        $lower_layer_macro!(@get $dol parse.parse)
                    )
                }
                // ########### END QUOTED CODE
            });
            let stmts = block.unwrap().unwrap().stmts;
            GenResult::Lexer(stmts)
        } else {
            let parse_builder_def = self.translate_parse_builder_def(cx);
            let parse_def = self.translate_parse_def(cx);

            let parse_builder = self.translate_parse_builder(cx);

            let expr = quote_expr!(cx, {
                // ########### QUOTED CODE
                use ::panini::*;

                $common_defs
                $parse_builder_def
                $parse_def

                $parse_builder
                // ########### END QUOTED CODE
            });
            GenResult::Parser(expr)
        }
    }

    pub fn translate_common_defs(&self, cx: &mut rs::ExtCtxt) -> Vec<rs::TokenTree> {
        let variant_name = self.trans.variant_map.iter().map(|v| &v.0);
        let variant_type = self.trans.variant_map.iter().map(|v| &v.1);
        let item_definitions = self.trans.item_definitions.iter();
        // Macro definitions.
        let null_bind_name = self.null.rules.iter().map(|r| r.name);
        let null_actions = self.null.rules.iter().map(|r| r.blocks.iter());
        let continuation_label = self.null.continuation_label;
        
        let terminal_name = self.trans.terminals.iter().map(|&terminal|
            self.trans.ir.name_of_external(terminal).unwrap().to_ident()
        );
        let terminal_id = self.trans.terminals.iter().map(|&terminal|
            self.trans.ir.internalize(terminal).unwrap().usize()
        );

        let dol = rs::TokenTree::Token(rs::DUMMY_SP, rs::Token::Dollar);
        let dol2 = dol.clone();

        let UniqueNames { Value, Infer, TerminalAccessor, .. } = self.unique_names;

        quote_tokens! {cx,
            // ########### QUOTED CODE #########################
            #[derive(Clone)]
            #[allow(non_camel_case_types)]
            enum $Value<I> where I: $Infer {
                $($variant_name($variant_type),)*
            }

            struct $TerminalAccessor;

            #[allow(non_snake_case)]
            impl $TerminalAccessor {
                $(
                    #[inline]
                    fn $terminal_name(&self) -> Symbol {
                        // Use internal symbols.
                        Symbol::from($terminal_id as u32)
                    }
                )*
            }

            $($item_definitions)*

            $(
                macro_rules! $null_bind_name {
                    ($dol x:expr) => {{
                        let mut $continuation_label = $dol2 x;
                        $($null_actions)*
                    }}
                }
            )*
            // ########### END QUOTED CODE
        }
    }

    pub fn translate_parse_builder_def(&self, cx: &mut rs::ExtCtxt) -> Vec<rs::TokenTree> {
        // Grammar info.
        let InternalGrammarParts {
            ref storage,
            num_syms,
            num_rules,
            num_external_syms,
            num_internal_syms,
            num_nulling_intermediate,
            start_sym,
            trivial_derivation,
        } = self.grammar_parts;
        // Convert serialized data to a byte string literal.
        let storage_str = AstBuilder::new().expr().lit().byte_str(&storage[..]);
        let trace_ids = self.trans.ir.trace_sources.iter().map(|source| source.rule_id);
        let trace_map = self.trans.ir.trace_sources.iter().map(|source| source.rule_pos.iter());
        let trace_tokens = self.trans.ir.trace_tokens.iter().map(|v| {
            v.iter().map(|s| AstBuilder::new().expr().lit().str(&s[..]))
        });

        let sym_names = (0 .. num_syms).map(|sym_id| {
            let internal_sym = Symbol::from(sym_id);
            let external_sym = self.trans.ir.externalize(internal_sym);
            let sym_name = self.trans.ir.name_of_external(external_sym);
            if let Some(sym_name) = sym_name {
                AstBuilder::new().expr().lit().str(&sym_name.as_str()[..])
            } else {
                let sym_name = format!("g{}", sym_id);
                AstBuilder::new().expr().lit().str(&sym_name[..])
            }
        });
        // Convert a symbol to integer.
        let start_sym = start_sym.usize();
        // Use internal symbols.
        let infer_name = self.trans.infer.iter();
        let (i2, i3, i4, i5, i6, i7) = (infer_name.clone(), infer_name.clone(), infer_name.clone(),
            infer_name.clone(), infer_name.clone(), infer_name.clone());

        let UniqueNames {
            LowerInferConstraint,
            Value, Infer, ValueInfer, InferTree, InferTreeVal,
            ParseFactory, Parse, TerminalAccessor, ..
        } = self.unique_names;

        quote_tokens! {cx,
            static SERIALIZED_GRAMMAR: &'static [u8] = $storage_str;
            static TRACE_INFO: TraceInfo = TraceInfo {
                ids: &[
                    $($trace_ids),*
                ],
                map: &[
                    $(&[$($trace_map),*]),*
                ],
                tokens: &[
                    $(&[$($trace_tokens),*]),*
                ],
            };
            static SYM_NAMES: &'static [&'static str] = &[
                $($sym_names),*
            ];

            struct $ParseFactory {
                grammar: grammar::InternalGrammar,
            }

            impl $ParseFactory {
                fn new() -> $ParseFactory {
                    let grammar = grammar::InternalGrammar::from_parts(
                        grammar::InternalGrammarParts {
                            storage: ::std::borrow::Cow::Borrowed(SERIALIZED_GRAMMAR),
                            num_syms: $num_syms,
                            num_rules: $num_rules,
                            num_external_syms: $num_external_syms,
                            num_internal_syms: $num_internal_syms,
                            num_nulling_intermediate: $num_nulling_intermediate,
                            start_sym: Symbol::from($start_sym),
                            trivial_derivation: $trivial_derivation,
                        }
                    );
                    $ParseFactory {
                        grammar: grammar,
                    }
                }

                fn terminal_accessor(&self) -> $TerminalAccessor {
                    $TerminalAccessor
                }

                fn new_parse<'g, I>(&'g mut self) -> $Parse<'g, I>
                    where I: $InferTree<'g> + 'g,
                {
                    let bocage = Box::new(Bocage::new(&self.grammar));
                    let bocage_ref: &'g Bocage<'g, 'g, 'g, I::Node, $Value<I::Infer>>;
                    unsafe {
                        bocage_ref = &*(&*bocage as *const _);
                    }

                    let recognizer = Recognizer::new(&self.grammar, bocage_ref);
                    let traversal = Traversal::new(bocage_ref, NullOrder::new());

                    $Parse {
                        store: Arena::new(),
                        recognizer: recognizer,
                        bocage: bocage,
                        traversal: traversal,
                        finished_node: None,
                        result: [].iter(),
                    }
                }
            }

            trait $Infer {
                type T: Copy;
                $(type $infer_name;)*
            }

            #[derive(Clone, Copy)]
            struct $ValueInfer<T, $($i2),*>(
                ::std::marker::PhantomData<(T, $($i3),*)>
            );

            impl<T, $($i4),*> $Infer for $ValueInfer<T, $($i5),*>
                where T: Copy
            {
                type T = T;
                $(type $i6 = $i7;)*
            }

            trait $InferTree<'g> {
                type Node: Copy;
                type Infer: $Infer;
            }

            struct $InferTreeVal<Node, I>(::std::marker::PhantomData<(Node, I)>);

            impl<'g, Node, I> $InferTree<'g> for $InferTreeVal<Node, I>
                where I: $Infer + $LowerInferConstraint<'g, Node> + 'g, Node: Copy + 'g
            {
                type Node = Node;
                type Infer = I;
            }
        }
    }

    pub fn translate_lexer_builder_def(&self, cx: &mut rs::ExtCtxt) -> Vec<rs::TokenTree> {
        // Grammar info.
        let InternalGrammarParts {
            ref storage,
            num_syms,
            num_rules,
            num_external_syms,
            num_internal_syms,
            num_nulling_intermediate,
            start_sym,
            trivial_derivation,
        } = self.grammar_parts;
        // Convert serialized data to a byte string literal.
        let storage_str = AstBuilder::new().expr().lit().byte_str(&storage[..]);
        // Convert a symbol to integer.
        let start_sym = start_sym.usize();
        // Use internal symbols for terminals.
        let super_terminals = self.trans.ir.lexer_for_upper.as_ref().unwrap().terminals();
        let upper_terminal_name = super_terminals.iter().map(|&sym|
            self.trans.ir.name_of_external(sym).unwrap().to_ident()
        );
        let infer_name = self.trans.infer.iter();
        let (i2, i3, i4, i5, i6, i7) = (infer_name.clone(), infer_name.clone(), infer_name.clone(),
            infer_name.clone(), infer_name.clone(), infer_name.clone());

        let UniqueNames {
            InferConstraint, LowerInferConstraint,
            Value, Infer, ValueInfer, InferTree, InferTreeVal, UpperInferTree,
            TerminalAccessor, UpperTerminalAccessor,
            SERIALIZED_GRAMMAR, UpperParse, UpperParseFactory, Layer, ParseFactory, Parse, ..
        } = self.unique_names;

        quote_tokens! {cx,
            static $SERIALIZED_GRAMMAR: &'static [u8] = $storage_str;

            struct $Layer;

            struct $ParseFactory {
                grammar: grammar::InternalGrammar,
                builder: $UpperParseFactory,
            }

            impl $Layer {
                fn new() -> Self {
                    $Layer
                }

                fn with_parse_builder(self, builder: $UpperParseFactory) -> $ParseFactory {
                    let grammar = grammar::InternalGrammar::from_parts(
                        grammar::InternalGrammarParts {
                            storage: ::std::borrow::Cow::Borrowed($SERIALIZED_GRAMMAR),
                            num_syms: $num_syms,
                            num_rules: $num_rules,
                            num_external_syms: $num_external_syms,
                            num_internal_syms: $num_internal_syms,
                            num_nulling_intermediate: $num_nulling_intermediate,
                            start_sym: Symbol::from($start_sym),
                            trivial_derivation: $trivial_derivation,
                        }
                    );
                    $ParseFactory {
                        grammar: grammar,
                        builder: builder,
                    }
                }
            }

            impl $ParseFactory {
                fn terminal_accessor(&self) -> $TerminalAccessor {
                    $TerminalAccessor
                }

                fn new_parse<'g, I>(&'g mut self) -> $Parse<'g, I>
                    where I: $InferTree<'g> + 'g
                {
                    let bocage = Box::new(Bocage::new(&self.grammar));
                    let bocage_ref: &'g Bocage<'g, 'g, 'g, I::Node, $Value<I::Infer>>;
                    unsafe {
                        bocage_ref = &*(&*bocage as *const _);
                    }

                    let recognizer = Recognizer::new(&self.grammar, bocage_ref);
                    let traversal = Traversal::new(bocage_ref, NullOrder::new());
                    let parse = self.builder.new_parse();

                    $Parse {
                        parse: Box::new(parse),
                        grammar: &self.grammar,
                        exhausted: false,
                        finished_node: None,
                        store: Arena::new(),
                        recognizer: recognizer,
                        bocage: bocage,
                        traversal: traversal,
                        scanned: vec![],
                        indices_scanned: vec![0],
                        inference_marker: ::std::marker::PhantomData,
                    }
                }
            }

            struct $Parse<'g, I> where I: $InferTree<'g> + 'g {
                parse: Box<$UpperParse<'g, I::Up>>,
                grammar: &'g grammar::InternalGrammar,
                exhausted: bool,
                finished_node: Option<NodeRef<'g, 'g, <I::Infer as $Infer>::T, $Value<I::Infer>>>,
                store: Arena<$Value<I::Infer>>,
                recognizer: Recognizer<'g, 'g, Bocage<'g, 'g, 'g, I::Node, $Value<I::Infer>>>,
                bocage: Box<Bocage<'g, 'g, 'g, I::Node, $Value<I::Infer>>>,
                traversal: TraversalUnordered<'g, I::Node, $Value<I::Infer>>,
                scanned: Vec<(Symbol, I::Node)>,
                indices_scanned: Vec<usize>,
                inference_marker: ::std::marker::PhantomData<I>,
            }

            // Either `advance` or `traced_advance` may be dead code.
            #[allow(dead_code)]
            impl<'g, I> $Parse<'g, I>
                where I: $InferTree<'g> + 'g
            {
                fn begin_earleme(&mut self) {
                    if self.recognizer.is_finished() {
                        self.finished_node = Some(self.recognizer.finished_node());
                        self.scanned.clear();
                        self.indices_scanned.clear();
                        self.indices_scanned.push(0);
                        if self.exhausted {
                            self.recognizer.reset();
                        }
                    }
                    if self.exhausted {
                        // Declare tokens.
                        let upper_terminals = $UpperTerminalAccessor;
                        let tokens = &[
                            $(upper_terminals.$upper_terminal_name()),*
                        ];
                        // Parse the finished part in the upper layer.
                        self.parse.begin_earleme();
                        if let Some(finished_node) = self.finished_node {
                            for summand in finished_node.alternatives() {
                                match summand.get() {
                                    node::Product { factors, action, .. } => {
                                        let token_node = factors.left;
                                        let internal_token = match token_node.get() {
                                            node::Sum { .. } | node::Product { .. } => {
                                                Symbol::from(token_node.nonterminal(self.grammar))
                                            }
                                            node::Leaf { symbol } => {
                                                self.grammar.to_internal(symbol).unwrap()
                                            }
                                            _ => unreachable!()
                                        };
                                        let token = tokens[internal_token.usize()];                                        
                                        self.parse.scan_tok(token, token_node);
                                    }
                                    _ => unreachable!()
                                }
                            }
                        }
                        assert!(self.parse.advance(), "begin_earleme: parse error");
                        // Roll back to before the unfinished part.
                        self.recognizer.reset();
                        let mut scanned = self.scanned.iter().cloned();
                        let last_index = self.indices_scanned.len() - 1;
                        for &len in &self.indices_scanned[.. last_index] {
                            self.parse.begin_earleme();
                            for (token, value) in (&mut scanned).take(len) {
                                self.recognizer.scan(token, value);
                            }
                            self.recognizer.advance();
                        }
                    }
                }

                fn traced_begin_earleme(&mut self) {
                    self.begin_earleme();
                }

                fn scan_tok(&mut self, token: Symbol, value: I::Node) {
                    self.recognizer.scan(token, value);
                    self.scanned.push((token, value));
                    *self.indices_scanned.last_mut().unwrap() += 1;
                }

                fn advance(&mut self) -> bool {
                    self.indices_scanned.push(0);
                    self.exhausted = !self.recognizer.advance();
                    true
                }

                // TODO: trace this layer.
                fn traced_advance(&mut self) -> bool {
                    self.indices_scanned.push(0);
                    self.exhausted = !self.recognizer.advance();
                    true
                }

                fn end_of_input(&mut self) {
                    self.exhausted = true;
                    self.begin_earleme();
                    self.parse.end_of_input();
                }

                fn traced_end_of_input(&mut self) {
                    self.exhausted = true;
                    self.begin_earleme();
                    self.parse.traced_end_of_input();
                }
            }

            impl<'g, I> Iterator for $Parse<'g, I>
                where I: $InferTree<'g> + 'g
            {
                type Item = <$UpperParse<'g, I::Up> as Iterator>::Item;
                fn next(&mut self) -> Option<Self::Item> {
                    self.parse.next()
                }
            }

            trait $Infer {
                type T: Copy;
                $(type $infer_name;)*
            }

            #[derive(Clone, Copy)]
            struct $ValueInfer<T, $($i2),*>(
                ::std::marker::PhantomData<(T, $($i3),*)>
            );

            impl<T, $($i4),*> $Infer for $ValueInfer<T, $($i5),*>
                where T: Copy,
            {
                type T = T;
                $(type $i6 = $i7;)*
            }

            trait $InferTree<'g> {
                type Up: $UpperInferTree<
                    'g,
                    Node = NodeRef<
                        'g,
                        'g,
                        <Self::Infer as $Infer>::T,
                        $Value<Self::Infer>
                    >
                > + 'g;
                type Node: Copy + 'g;
                type Infer: $Infer + $LowerInferConstraint<'g, Self::Node> + 'g;
            }

            struct $InferTreeVal<Up, Node, I>(::std::marker::PhantomData<(Up, Node, I)>);

            impl<'g, Up, Node, I> $InferTree<'g> for $InferTreeVal<Up, Node, I>
                where Up: $UpperInferTree<'g, Node = NodeRef<'g, 'g,  I::T, $Value<I>>> + 'g,
                      Node: Copy + 'g,
                      I: $Infer + $LowerInferConstraint<'g, Node> + 'g
            {
                type Up = Up;
                type Node = Node;
                type Infer = I;
            }

            trait $InferConstraint<'g, Node> {}
            impl<'g, Node, T> $InferConstraint<'g, Node> for T {}
        }
    }

    pub fn translate_parse_def(&self, cx: &mut rs::ExtCtxt) -> Vec<rs::TokenTree> {
        let external_start = self.trans.ir.externalize(self.trans.ir.grammar.get_start());
        let start_type = &self.trans.type_map[&external_start]
                         .generate_qualified(self.trans.builder, self.unique_names.Infer);
        let start_variant = &self.trans.variant_names[&external_start];

        let UniqueNames {
            Parse, Value, InferTree, ..
        } = self.unique_names;

        quote_tokens! {cx,
            struct $Parse<'g, I> where I: $InferTree<'g> + 'g {
                store: Arena<$Value<I::Infer>>,
                recognizer: Recognizer<'g, 'g, Bocage<'g, 'g, 'g, I::Node, $Value<I::Infer>>>,
                finished_node: Option<NodeRef<'g, 'g, I::Node, $Value<I::Infer>>>,
                // This field is seen as unused.
                #[allow(dead_code)]
                bocage: Box<Bocage<'g, 'g, 'g, I::Node, $Value<I::Infer>>>,
                traversal: TraversalUnordered<'g, I::Node, $Value<I::Infer>>,
                result: ::std::slice::Iter<'g, $Value<I::Infer>>,
            }

            #[allow(dead_code)]
            impl<'g, I> $Parse<'g, I>
                where I: $InferTree<'g> + 'g
            {
                fn begin_earleme(&mut self) {
                    // Nothing to do
                }

                fn traced_begin_earleme(&mut self) {
                    // Nothing to do?
                    self.begin_earleme();
                }

                fn scan_tok(&mut self, token: Symbol, value: I::Node) {
                    self.recognizer.scan(token, value);
                }

                fn advance(&mut self) -> bool {
                    self.recognizer.advance()
                }

                fn traced_advance(&mut self) -> bool {
                    if self.recognizer.is_exhausted() {
                        false
                    } else {
                        let mut finished_node = None;
                        let mut completion_items = vec![];
                        {
                            let start_sym = self.recognizer.grammar().start_sym();
                            // Access completions. This must come after getting `start_sym`.
                            let mut completions = self.recognizer.completions();
                            while let Some(mut completion) = completions.next_completion() {
                                while let Some(item) = completion.next() {
                                    completion_items.push(item);
                                    completion.push(item);
                                }
                                let node = completion.complete();
                                if completion.origin() == 0 && completion.symbol() == start_sym {
                                    finished_node = Some(node);
                                }
                            }
                        };
                        self.finished_node = finished_node;
                        self.recognizer.advance_without_completion();
                        print_trace(&self.recognizer, &completion_items[..], TRACE_INFO);
                        true
                    }
                }

                fn end_of_input(&mut self) {
                    // Update the finished node field of this parse.
                    self.finished_node = Some(self.recognizer.finished_node());
                }

                fn traced_end_of_input(&mut self) {
                    // Nothing to do.
                }

                fn fmt_exhaustion(&self, fmt: &mut fmt::Formatter, input_pos: usize)
                    -> Result<(), fmt::Error>
                {
                    try!(write!(fmt, "Parse error at {}:\nexpected", input_pos));
                    let mut terminals = self.recognizer.expected_terminals();
                    let last = terminals.next();
                    for terminal in terminals {
                        try!(write!(fmt, " `{}`,", SYM_NAMES[terminal.usize()]));
                    }
                    if let Some(last) = last {
                        write!(fmt, " or `{}`.", SYM_NAMES[last.usize()])
                    } else {
                        write!(fmt, "end of input.")
                    }
                }
            }

            impl<'g, I> Iterator for $Parse<'g, I>
                where I: $InferTree<'g> + 'g,
            {
                type Item = &'g $start_type;
                fn next(&mut self) -> Option<Self::Item> {
                    match self.result.next() {
                        Some(&$Value::$start_variant(ref value)) => Some(value),
                        _ => None,
                    }
                }
            }
        }
    }

    pub fn translate_closure(
        &self,
        cx: &mut rs::ExtCtxt,
        traversal: Vec<rs::TokenTree>,
        store: Vec<rs::TokenTree>)
        -> Vec<rs::TokenTree>
    {
        let action_id = self.rules.iter().map(|r| &r.id);
        let rule_variant = self.rules.iter().map(|r| &r.variant);
        let rules_expr = self.rules.iter().map(|r| &r.action);
        let arg_num = self.rules.iter().map(|r| r.args.iter().map(|p| &p.num));
        let arg_variant = self.rules.iter().map(|r| r.args.iter().map(|p| &p.variant));
        let arg_pat = self.rules.iter().map(|r| r.args.iter().map(|p| &p.pat));

        let num_external_rules = self.trans.ir.external_grammar.len() as u32;
        let num_external_sequences = self.sequences.len() as u32;
        let seq_action_id = num_external_rules .. (num_external_rules + num_external_sequences);
        let seq_element_variant = self.sequences.iter().map(|s| &s.elem_variant);
        let seq_variant = self.sequences.iter().map(|s| &s.variant);

        let null_symbol_id = self.null.roots.iter().map(|root| root.sym.usize());
        // All lengths are non-zero
        let null_num_summands = self.null.roots.iter().map(|root| root.num);
        let null_sym_name = self.null.roots.iter().map(|root| root.name);
        let null_variant = self.null.roots.iter().map(|root| {
            self.trans.variant_names[&root.sym]
        });

        let UniqueNames { Value, lower_layer_macro, .. } = self.unique_names;
        let Value_ = Value;

        quote_tokens! {cx,
            // ########### QUOTED CODE #########################
            let mut cartesian_product = CartesianProduct::new();
            $traversal.traverse(root);
            loop {
                if let Some(deps) = $traversal.traverse_deps() {
                    for node in deps {
                        match node {
                            TraversalBottom::Leaf(node) => {
                                let mut builder = SliceBuilder::new(&$store, 0);
                                $lower_layer_macro!(
                                    @closure
                                    builder,
                                    parse,
                                    node;
                                );
                                node.result(builder.into_slice());
                            }
                            TraversalBottom::Null(nulling) => {
                                // The builder may be unused.
                                let mut _builder = SliceBuilder::new(&$store, 0);
                                // Use external symbols.
                                match nulling.symbol.usize() {
                                    $(
                                        $null_symbol_id => {
                                            _builder.reserve($null_num_summands);
                                            $null_sym_name!(|result| {
                                                _builder.push($Value::$null_variant(result));
                                            });
                                        }
                                    )*
                                    id => unreachable!("nulling id {}", id)
                                }
                                nulling.result(_builder.into_slice());
                            }
                        }
                    }
                } else {
                    break;
                }
                for node in $traversal.traverse_sum() {
                    let count = node.iter().map(|alt| alt.len()).fold(0, |acc, elem| acc + elem);
                    let mut slice_builder = SliceBuilder::new(&$store, count);
                    // ... eval:
                    for alt in node.iter() {
                        cartesian_product.from_production(&alt);
                        // let mut finished = false;
                        loop {
                            let result = {
                                let args = cartesian_product.as_slice();
                                match alt.action() {
                                    $(
                                        $action_id => {
                                            // `true` is to avoid irrefutable patterns
                                            let val = (true, $(args[$arg_num].clone(),)*);
                                            if let (true,
                                                    $($Value::$arg_variant($arg_pat),)*) = val {
                                                $Value_::$rule_variant($rules_expr)
                                            } else {
                                                unreachable!()
                                            }
                                        }
                                    )*
                                    $(
                                        $seq_action_id => {
                                            let seq_vec = args.iter().map(|arg| {
                                                let val = (true, (*arg).clone());
                                                if let (true,
                                                        $Value::$seq_element_variant(elem)) = val {
                                                    elem
                                                } else {
                                                    unreachable!()
                                                }
                                            }).collect::<Vec<_>>();
                                            $Value_::$seq_variant(seq_vec)
                                        }
                                    )*
                                    _ => unreachable!("rule id {}", alt.action())
                                }
                            };
                            // placement new?
                            slice_builder.push(result);
                            if cartesian_product.next().is_none() {
                                break;
                            }
                        }
                    }
                    node.result(slice_builder.advance_slice());
                }
            }
            // ########### END QUOTED CODE
        }
    }

    pub fn translate_parse_builder(&self, cx: &mut rs::ExtCtxt) -> Vec<rs::TokenTree> {
        let UniqueNames {
            ValueInfer, InferTreeVal,
            Parse, ParseFactory, lower_layer_macro, ..
        } = self.unique_names;

        let lexer_def = self.translate_lexer_def(cx);
        let traversal = quote_tokens! {cx, traversal};
        let store = quote_tokens! {cx, store};
        let closure = self.translate_closure(cx, traversal, store);
        let infer_wildcards = iter::repeat(AstBuilder::new().ty().infer())
                              .take(self.trans.infer.len());

        quote_tokens! {cx,
            // ########### QUOTED CODE #########################
            $lexer_def

            $lower_layer_macro!(@builder
                @factory [$ParseFactory::new()]
                @closure [|mut parse| {
                    {
                        // Partially guide the inference of the argument's type.
                        let _: &$Parse<
                            $InferTreeVal<_, $ValueInfer<_, $($infer_wildcards),*>>
                        > = &*$lower_layer_macro!(@get parse);
                    };
                    let &mut $Parse {
                        ref mut traversal,
                        ref store,
                        finished_node,
                        ref mut result,
                        ..
                    } = &mut *$lower_layer_macro!(@get parse);
                    let root = finished_node.unwrap();
                    // ===
                    $closure
                    // ===
                    *result = match root.get() {
                        Evaluated { values } => values.iter(),
                        _ => unreachable!()
                    };
                }])
            // ########### END QUOTED CODE
        }
    }

    pub fn translate_lexer_def(&self, cx: &mut rs::ExtCtxt) -> Vec<rs::TokenTree> {
        // Conditionally compile
        let item = if self.trans.ir.lower_level == LowerLevel::None {
            self.translate_identity(cx)
        } else {
            self.translate_lexer_invocation(cx)
        };
        item
    }

    fn translate_identity(&self, cx: &mut rs::ExtCtxt) -> Vec<rs::TokenTree> {
        let UniqueNames { layer_macro, .. } = self.unique_names;
        let dol = rs::TokenTree::Token(rs::DUMMY_SP, rs::Token::Dollar);
        // What to do with this?
        quote_tokens! {cx,
            // ########### QUOTED CODE #########################
            macro_rules! $layer_macro {
                () => (unreachable!());
                (@builder @factory [$dol factory:expr] @closure [$dol closure:expr]) => (Identity);
                (@get $dol parse:expr) => ($dol parse);
            }

            pub struct Identity;
            impl Identity {
                pub fn with_parse_builder<P>(self, parse_builder: P) -> P { parse_builder }
            }
            // ########### END QUOTED CODE
        }
    }

    fn translate_lexer_invocation(&self, cx: &mut rs::ExtCtxt) -> Vec<rs::TokenTree> {
        // Prepend an attribute.
        // Pass this parser's terminal names to this parser's lexer.
        let terminal_name = self.trans.terminals.iter().map(|&terminal|
            self.trans.ir.name_of_external(terminal).unwrap().to_ident()
        );
        let super_layer = self.trans.ir.lexer_for_upper.as_ref();
        let lexer_level = super_layer.map_or(0, |lexer| lexer.level()) + 1;
        let lexer_attr = format!("lexer_{}", lexer_level).to_ident();
        // Quote
        match &self.trans.ir.lower_level {
            &LowerLevel::Invoke { ref lexer, ref embedded_strings } => {
                let lexer_name = lexer.name().to_ident();
                let lexer_tts = lexer.tts();
                let mut tokens = quote_tokens!(cx, #![$lexer_attr($($terminal_name),*)]);
                for string in embedded_strings {
                    let lhs = self.trans.ir.name_of_external(string.0.node).unwrap().to_ident();
                    let rhs = self.trans.builder.expr().str(string.1.node);
                    let stmt_tokens = quote_tokens! {cx,
                        $lhs ::= $rhs;
                    };
                    tokens.extend(stmt_tokens);
                }
                tokens.extend(lexer_tts.into_iter());
                // The lexer invocation.
                quote_tokens! {cx,
                    // ########### QUOTED CODE #########################
                    $lexer_name! { $tokens }
                    // ########################### END QUOTED CODE
                }
            }
            &LowerLevel::CharClassifier(ref char_ranges) => {
                let lhs = char_ranges.iter().map(|range| {
                    self.trans.ir.name_of_external(range.1).unwrap().to_ident()
                });
                let start = char_ranges.iter().map(|range| {
                    rs::TokenTree::Token(
                        rs::DUMMY_SP,
                        rs::Token::Literal(
                            rs::token::Char(rs::intern(&*range.0.start.to_string())),
                            None
                        )
                    )
                });
                let end = char_ranges.iter().map(|range| {
                    rs::TokenTree::Token(
                        rs::DUMMY_SP,
                        rs::Token::Literal(
                            rs::token::Char(rs::intern(&*range.0.end.to_string())),
                            None
                        )
                    )
                });
                quote_tokens! {cx,
                    // ########### QUOTED CODE
                    char_classifier! {
                        #![$lexer_attr($($terminal_name),*)]
                        $(
                            $lhs ::= $start ... $end;
                        )*
                    }
                    // END QUOTED CODE
                }
            }
            _ => unreachable!()
        }
    }
}
