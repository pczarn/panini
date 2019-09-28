#![allow(non_snake_case)]

use std::iter;

use cfg::symbol::Symbol;
use cfg_regex::ClassRange;
use gearley::grammar::InternalGrammarParts;

use rs;
use quote::ByteStr;
use quote::ToTokens;

// Info for generation.

pub struct GenParser {
    // Serialized grammar
    pub grammar_parts: InternalGrammarParts,

    // Properties of the start symbol
    pub start_variant: rs::Term,
    pub start_type: GenType,

    // For actions
    pub epsilon_actions: GenEpsilonActions,
    pub rules: Vec<GenRule>,
    pub sequences: Vec<GenSequence>,

    // Forest node variants with their inner types.
    pub variant_map: Vec<(rs::Term, rs::TokenStream)>,

    // For terminals
    pub terminal_names: Vec<rs::Term>,
    pub terminal_ids: Vec<usize>,

    // For passing evaluated pieces to the outer layer, when the outer layer's
    // terminals are parsed.
    pub arguments_from_outer_layer: Option<GenArgumentsFromOuterLayer>,

    // For declaring the inner layer
    pub inner_layer: Option<GenInvocationOfInnerLayer>,
    pub inner_layer_level: u32,

    // For tracing
    pub trace_rule_ids: Vec<u32>,
    pub trace_rule_pos: Vec<Vec<u32>>,
    pub trace_tokens: Vec<Vec<String>>,

    // Names of all internal symbols
    pub sym_names: Vec<String>,

    // For automatic AST
    pub item_definitions: Vec<rs::TokenStream>,

    // Names of type parameters to be inferred: I0, I1, I2, etc.
    pub infer: Vec<rs::Term>,

    // This layer's item names
    pub unique_names: UniqueNames,
}

pub struct GenArgumentsFromOuterLayer {
    pub terminal_names: Vec<rs::Term>,
    pub terminal_variants: Vec<rs::InternedString>,
    pub terminal_bare_variants: Vec<rs::Term>,
}

// #[derive(Eq, PartialEq)]
pub struct GenInvocationOfInnerLayer {
    pub lexer_name: rs::Term,
    pub lexer_tts: Vec<rs::TokenTree>,
    pub str_lhs: Vec<rs::Term>,
    pub str_rhs: Vec<rs::Name>,
    pub char_range_lhs: Vec<rs::Term>,
    pub char_ranges: Vec<ClassRange>,
}

// Nulling rules

pub struct GenEpsilonActions {
    pub rules: Vec<GenEpsilonIntermediateRule>,
    pub roots: Vec<GenEpsilonRootAction>,
    pub continuation_label: rs::Term,
}

#[derive(Debug)]
pub struct GenEpsilonIntermediateRule {
    pub name: rs::Term,
    pub blocks: Vec<rs::TokenTree>,
}

#[derive(Debug)]
pub struct GenEpsilonRootAction {
    // This symbol must be internal
    pub sym: Symbol,
    pub num: usize,
    pub name: rs::Term,
    // redundant?
    pub variant_name: rs::Term,
}

// Rules

pub struct GenRule {
    pub id: u32,
    pub variant: rs::Term,
    pub action: rs::TokenTree,
    pub args: Vec<GenArg>,
}

pub struct GenArg {
    pub num: usize,
    pub variant: rs::Term,
    pub pat: rs::TokenTree,
}

// Sequcene rules

pub struct GenSequence {
    pub id: u32,
    pub elem_variant: rs::Term,
    pub variant: rs::Term,
}

// Type

#[derive(Clone)]
pub enum GenType {
    RustTy(rs::Tokens),
    Vec(Box<GenType>),
    Unit,
    Tuple(Vec<GenType>),
    Identifier(rs::Term),
    Item(rs::Term),
    Infer(rs::Term),
    Terminal,
}

//------------------------------
// Names of generated items. The number unique to this layer is put on every name.

#[derive(Copy, Clone)]
pub struct UniqueNames {
    Value: rs::Term,
    ValueInfer: rs::Term,
    pub Infer: rs::Term,
    Layer: rs::Term,
    ParseFactory: rs::Term,
    Parse: rs::Term,
    TracedParse: rs::Term,
    LayerParam: rs::Term,
    Params: rs::Term,
    TerminalAccessor: rs::Term,
    EvalArg: rs::Term,
    SERIALIZED_GRAMMAR: rs::Term,
    UpperValue: rs::Term,
    UpperParse: rs::Term,
    UpperParseFactory: rs::Term,
    UpperLayerParam: rs::Term,
    UpperEvalArg: rs::Term,
    LowerEvalArg: rs::Term,
    UpperTerminalAccessor: rs::Term,
    UpperInfer: rs::Term,
    LowerInfer: rs::Term,
    InferTree: rs::Term,
    InferTreeVal: rs::Term,
    UpperInferTree: rs::Term,
    InferConstraint: rs::Term,
    LowerInferConstraint: rs::Term,
    layer_macro: rs::Term,
    lower_layer_macro: rs::Term,
}

impl UniqueNames {
    pub fn new(current_id: usize) -> Self {
        let upper_id = if current_id == 0 { !0 } else { current_id - 1 };
        let lower_id = current_id + 1;

        UniqueNames {
            ValueInfer: rs::Term::intern(&*format!("ValueInfer{}", current_id)),
            Value: rs::Term::intern(&*format!("Value{}", current_id)),
            Infer: rs::Term::intern(&*format!("Infer{}", current_id)),
            Layer: rs::Term::intern(&*format!("Layer{}", current_id)),
            ParseFactory: rs::Term::intern(&*format!("ParseFactory{}", current_id)),
            Parse: rs::Term::intern(&*format!("Parse{}", current_id)),
            TracedParse: rs::Term::intern(&*format!("TracedParse{}", current_id)),
            LayerParam: rs::Term::intern(&*format!("LayerParam{}", current_id)),
            Params: rs::Term::intern(&*format!("Params{}", current_id)),
            TerminalAccessor: rs::Term::intern(&*format!("TerminalAccessor{}", current_id)),
            EvalArg: rs::Term::intern(&*format!("EvalArg{}", current_id)),
            SERIALIZED_GRAMMAR: rs::Term::intern(&*format!("SERIALIZED_GRAMMAR{}", current_id)),
            UpperValue: rs::Term::intern(&*format!("Value{}", upper_id)),
            UpperParse: rs::Term::intern(&*format!("Parse{}", upper_id)),
            UpperParseFactory: rs::Term::intern(&*format!("ParseFactory{}", upper_id)),
            UpperLayerParam: rs::Term::intern(&*format!("LayerParam{}", upper_id)),
            UpperEvalArg: rs::Term::intern(&*format!("EvalArg{}", upper_id)),
            LowerEvalArg: rs::Term::intern(&*format!("EvalArg{}", lower_id)),
            layer_macro: rs::Term::intern(&*format!("layer_macro{}", current_id)),
            lower_layer_macro: rs::Term::intern(&*format!("layer_macro{}", lower_id)),
            UpperTerminalAccessor: rs::Term::intern(&*format!("TerminalAccessor{}", upper_id)),
            UpperInfer: rs::Term::intern(&*format!("Infer{}", upper_id)),
            LowerInfer: rs::Term::intern(&*format!("Infer{}", lower_id)),
            InferTree: rs::Term::intern(&*format!("InferTree{}", current_id)),
            InferTreeVal: rs::Term::intern(&*format!("InferTreeVal{}", current_id)),
            UpperInferTree: rs::Term::intern(&*format!("InferTree{}", upper_id)),
            InferConstraint: rs::Term::intern(&*format!("InferConstraint{}", current_id)),
            LowerInferConstraint: rs::Term::intern(&*format!("InferConstraint{}", lower_id)),
        }
    }
}

pub enum GenResult {
    Parser(String),
    Lexer(String),
}

impl GenParser {
    pub fn translate(&self, cx: &mut rs::ExtCtxt) -> GenResult {
        let common_defs = self.translate_common_defs(cx);
        let lexer_def = self.translate_lexer_def(cx);

        if let &Some(ref arguments_from_outer_layer) = &self.arguments_from_outer_layer {
            let lexer_builder_def = self.translate_lexer_builder_def(cx);
            let layer_macro_def = self.translate_layer_macro_def(cx, arguments_from_outer_layer);

            let block = quote!({
                // ########### QUOTED CODE
                #common_defs
                #lexer_builder_def
                #lexer_def
                #layer_macro_def
                // ########### END QUOTED CODE
            });
            // let stmts = block.unwrap().unwrap().stmts;
            GenResult::Lexer(block.to_string())
        } else {
            let parse_builder_def = self.translate_parse_builder_def(cx);
            let parse_def = self.translate_parse_def(cx);
            let parse_builder = self.translate_parse_builder(cx);

            let expr = quote!({
                // ########### QUOTED CODE
                use ::panini::*;

                #common_defs
                #parse_builder_def
                #parse_def
                #lexer_def
                #parse_builder
                // ########### END QUOTED CODE
            });
            GenResult::Parser(expr.to_string())
        }
    }

    pub fn translate_common_defs(&self, cx: &mut rs::ExtCtxt) -> rs::Tokens {
        let variant_name = self.variant_map.iter().map(|v| &v.0);
        let variant_type = self.variant_map.iter().map(|v| &v.1);
        let item_definitions = self.item_definitions.iter();
        // Macro definitions.
        let null_bind_name = self.epsilon_actions.rules.iter().map(|r| r.name);
        let null_actions = self.epsilon_actions.rules.iter().map(|r| r.blocks.iter());
        let continuation_label = iter::repeat(self.epsilon_actions.continuation_label);
        
        let terminal_name = self.terminal_names.iter();
        let terminal_id = self.terminal_ids.iter();

        let dol = rs::TokenTree::Token(rs::DUMMY_SP, rs::Token::Dollar);
        let dol2 = dol.clone();

        let UniqueNames { Value, Infer, TerminalAccessor, .. } = self.unique_names;

        rust! {
            // ########### QUOTED CODE #########################
            #[derive(Clone)]
            #[allow(non_camel_case_types)]
            enum {{Value}}<I> where I: {{Infer}} {
                {% for (name, type) in variants %}
                    {{name}}({{type}}),
                {% endfor %}
            }

            struct {{TerminalAccessor}};

            #[allow(non_snake_case)]
            impl {{TerminalAccessor}} {
                #(
                    #[inline]
                    fn #terminal_name(&self) -> Symbol {
                        // Use internal symbols.
                        Symbol::from(#terminal_id as u32)
                    }
                )*
            }

            #(#item_definitions)*

            #(
                macro_rules! #null_bind_name {
                    ($x:expr) => {{
                        let mut #continuation_label = $x;
                        #(#null_actions)*
                    }}
                }
            )*
            // ########### END QUOTED CODE
        }
    }

    pub fn translate_parse_builder_def(&self, cx: &mut rs::ExtCtxt) -> rs::Tokens {
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
        // FIXME [u8]=>str. let storage_str = ByteStr(&storage[..]);
        let storage_str = ByteStr("");
        let trace_ids = self.trace_rule_ids.iter();
        let trace_map = self.trace_rule_pos.iter().map(|v| v.iter());
        let trace_tokens = self.trace_tokens.iter().map(|rule_tokens| {
            rule_tokens.iter().map(|tok| &tok[..])
        });

        let sym_names = self.sym_names.iter().map(|name| {
            &name[..]
        });
        // Convert a symbol to integer.
        let start_sym = start_sym.usize();
        // Use internal symbols.
        let infer_name = self.infer.iter();
        let (i2, i3, i4, i5, i6, i7) = (infer_name.clone(), infer_name.clone(), infer_name.clone(),
            infer_name.clone(), infer_name.clone(), infer_name.clone());

        let UniqueNames {
            LowerInferConstraint,
            Value, Infer, ValueInfer, InferTree, InferTreeVal,
            ParseFactory, Parse, TerminalAccessor, ..
        } = self.unique_names;

        quote! {
            static SERIALIZED_GRAMMAR: &'static [u8] = #storage_str;
            static TRACE_INFO: TraceInfo = TraceInfo {
                ids: &[
                    #(#trace_ids),*
                ],
                map: &[
                    #(&[#(#trace_map),*]),*
                ],
                tokens: &[
                    #(&[#(#trace_tokens),*]),*
                ],
            };
            static SYM_NAMES: &'static [&'static str] = &[
                #(#sym_names),*
            ];

            struct #ParseFactory {
                grammar: grammar::InternalGrammar,
            }

            impl #ParseFactory {
                fn new() -> #ParseFactory {
                    let grammar = grammar::InternalGrammar::from_parts(
                        grammar::InternalGrammarParts {
                            storage: ::std::borrow::Cow::Borrowed(SERIALIZED_GRAMMAR),
                            num_syms: #num_syms,
                            num_rules: #num_rules,
                            num_external_syms: #num_external_syms,
                            num_internal_syms: #num_internal_syms,
                            num_nulling_intermediate: #num_nulling_intermediate,
                            start_sym: Symbol::from(#start_sym),
                            trivial_derivation: #trivial_derivation,
                        }
                    );
                    #ParseFactory {
                        grammar: grammar,
                    }
                }

                fn terminal_accessor(&self) -> #TerminalAccessor {
                    #TerminalAccessor
                }

                fn new_parse<'g, I>(&'g mut self) -> #Parse<'g, I>
                    where I: #InferTree<'g> + 'g,
                {
                    let bocage = Box::new(Bocage::new(&self.grammar));
                    let bocage_ref: &'g Bocage<'g, 'g, 'g, I::Node, #Value<I::Infer>>;
                    unsafe {
                        bocage_ref = &*(&*bocage as *const _);
                    }

                    let recognizer = Recognizer::new(&self.grammar, bocage_ref);
                    let traversal = Traversal::new(bocage_ref, NullOrder::new());

                    #Parse {
                        store: Arena::new(),
                        recognizer: recognizer,
                        bocage: bocage,
                        traversal: traversal,
                        finished_node: None,
                        result: [].iter(),
                    }
                }
            }

            trait #Infer {
                type T: Copy;
                #(type #infer_name;)*
            }

            #[derive(Clone, Copy)]
            struct #ValueInfer<T, #(#i2),*>(
                ::std::marker::PhantomData<(T, #(#i3),*)>
            );

            impl<T, #(#i4),*> #Infer for #ValueInfer<T, #(#i5),*>
                where T: Copy
            {
                type T = T;
                #(type #i6 = #i7;)*
            }

            trait #InferTree<'g> {
                type Node: Copy;
                type Infer: #Infer;
            }

            struct #InferTreeVal<Node, I>(::std::marker::PhantomData<(Node, I)>);

            impl<'g, Node, I> #InferTree<'g> for #InferTreeVal<Node, I>
                where I: #Infer + #LowerInferConstraint<'g, Node> + 'g, Node: Copy + 'g
            {
                type Node = Node;
                type Infer = I;
            }
        }
    }

    pub fn translate_lexer_builder_def(&self, cx: &mut rs::ExtCtxt) -> rs::Tokens {
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
        let storage_str = ByteStr(&storage[..]);
        // Convert a symbol to integer.
        let start_sym = start_sym.usize();
        // Use internal symbols for terminals.
        let arguments_from_outer_layer = self.arguments_from_outer_layer.as_ref().unwrap();
        let outer_terminal_name = arguments_from_outer_layer.terminal_names.iter();
        let infer_name = self.infer.iter();
        let (i2, i3, i4, i5, i6, i7) = (infer_name.clone(), infer_name.clone(), infer_name.clone(),
            infer_name.clone(), infer_name.clone(), infer_name.clone());

        let UniqueNames {
            InferConstraint, LowerInferConstraint,
            Value, Infer, ValueInfer, InferTree, InferTreeVal, UpperInferTree,
            TerminalAccessor, UpperTerminalAccessor,
            SERIALIZED_GRAMMAR, UpperParse, UpperParseFactory, Layer, ParseFactory, Parse, ..
        } = self.unique_names;

        quote! {
            static #SERIALIZED_GRAMMAR: &'static [u8] = #storage_str;

            struct #Layer;

            struct #ParseFactory {
                grammar: grammar::InternalGrammar,
                builder: #UpperParseFactory,
            }

            impl #Layer {
                fn new() -> Self {
                    #Layer
                }

                fn with_parse_builder(self, builder: #UpperParseFactory) -> #ParseFactory {
                    let grammar = grammar::InternalGrammar::from_parts(
                        grammar::InternalGrammarParts {
                            storage: ::std::borrow::Cow::Borrowed(#SERIALIZED_GRAMMAR),
                            num_syms: #num_syms,
                            num_rules: #num_rules,
                            num_external_syms: #num_external_syms,
                            num_internal_syms: #num_internal_syms,
                            num_nulling_intermediate: #num_nulling_intermediate,
                            start_sym: Symbol::from(#start_sym),
                            trivial_derivation: #trivial_derivation,
                        }
                    );
                    #ParseFactory {
                        grammar: grammar,
                        builder: builder,
                    }
                }
            }

            impl #ParseFactory {
                fn terminal_accessor(&self) -> #TerminalAccessor {
                    #TerminalAccessor
                }

                fn new_parse<'g, I>(&'g mut self) -> #Parse<'g, I>
                    where I: #InferTree<'g> + 'g
                {
                    let bocage = Box::new(Bocage::new(&self.grammar));
                    let bocage_ref: &'g Bocage<'g, 'g, 'g, I::Node, #Value<I::Infer>>;
                    unsafe {
                        bocage_ref = &*(&*bocage as *const _);
                    }

                    let recognizer = Recognizer::new(&self.grammar, bocage_ref);
                    let traversal = Traversal::new(bocage_ref, NullOrder::new());
                    let parse = self.builder.new_parse();

                    #Parse {
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

            struct #Parse<'g, I> where I: #InferTree<'g> + 'g {
                parse: Box<#UpperParse<'g, I::Up>>,
                grammar: &'g grammar::InternalGrammar,
                exhausted: bool,
                finished_node: Option<NodeRef<'g, 'g, <I::Infer as #Infer>::T, #Value<I::Infer>>>,
                store: Arena<#Value<I::Infer>>,
                recognizer: Recognizer<'g, 'g, Bocage<'g, 'g, 'g, I::Node, #Value<I::Infer>>>,
                bocage: Box<Bocage<'g, 'g, 'g, I::Node, #Value<I::Infer>>>,
                traversal: TraversalUnordered<'g, I::Node, #Value<I::Infer>>,
                scanned: Vec<(Symbol, I::Node)>,
                indices_scanned: Vec<usize>,
                inference_marker: ::std::marker::PhantomData<I>,
            }

            // Either `advance` or `traced_advance` may be dead code.
            #[allow(dead_code)]
            impl<'g, I> #Parse<'g, I>
                where I: #InferTree<'g> + 'g
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
                        let upper_terminals = #UpperTerminalAccessor;
                        let tokens = &[
                            #(upper_terminals.#outer_terminal_name()),*
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

            impl<'g, I> Iterator for #Parse<'g, I>
                where I: #InferTree<'g> + 'g
            {
                type Item = <#UpperParse<'g, I::Up> as Iterator>::Item;
                fn next(&mut self) -> Option<Self::Item> {
                    self.parse.next()
                }
            }

            trait #Infer {
                type T: Copy;
                #(type #infer_name;)*
            }

            #[derive(Clone, Copy)]
            struct #ValueInfer<T, #(#i2),*>(
                ::std::marker::PhantomData<(T, #(#i3),*)>
            );

            impl<T, #(#i4),*> #Infer for #ValueInfer<T, #(#i5),*>
                where T: Copy,
            {
                type T = T;
                #(type #i6 = #i7;)*
            }

            trait #InferTree<'g> {
                type Up: #UpperInferTree<
                    'g,
                    Node = NodeRef<
                        'g,
                        'g,
                        <Self::Infer as #Infer>::T,
                        #Value<Self::Infer>
                    >
                > + 'g;
                type Node: Copy + 'g;
                type Infer: #Infer + #LowerInferConstraint<'g, Self::Node> + 'g;
            }

            struct #InferTreeVal<Up, Node, I>(::std::marker::PhantomData<(Up, Node, I)>);

            impl<'g, Up, Node, I> #InferTree<'g> for #InferTreeVal<Up, Node, I>
                where Up: #UpperInferTree<'g, Node = NodeRef<'g, 'g,  I::T, #Value<I>>> + 'g,
                      Node: Copy + 'g,
                      I: #Infer + #LowerInferConstraint<'g, Node> + 'g
            {
                type Up = Up;
                type Node = Node;
                type Infer = I;
            }

            trait #InferConstraint<'g, Node> {}
            impl<'g, Node, T> #InferConstraint<'g, Node> for T {}
        }
    }

    pub fn translate_parse_def(&self, cx: &mut rs::ExtCtxt) -> rs::Tokens {
        // let external_start = self.trans.ir.externalize(self.trans.ir.grammar.get_start());

        let start_type = self.start_type
                         .generate_qualified(self.unique_names.Infer);
        let start_variant = self.start_variant;

        let UniqueNames {
            Parse, Value, InferTree, ..
        } = self.unique_names;

        quote! {
            struct #Parse<'g, I> where I: #InferTree<'g> + 'g {
                store: Arena<#Value<I::Infer>>,
                recognizer: Recognizer<'g, 'g, Bocage<'g, 'g, 'g, I::Node, #Value<I::Infer>>>,
                finished_node: Option<NodeRef<'g, 'g, I::Node, #Value<I::Infer>>>,
                // This field is seen as unused.
                #[allow(dead_code)]
                bocage: Box<Bocage<'g, 'g, 'g, I::Node, #Value<I::Infer>>>,
                traversal: TraversalUnordered<'g, I::Node, #Value<I::Infer>>,
                result: ::std::slice::Iter<'g, #Value<I::Infer>>,
            }

            #[allow(dead_code)]
            impl<'g, I> #Parse<'g, I>
                where I: #InferTree<'g> + 'g
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
                            let start_sym = Symbol::from(#start_sym);
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

            impl<'g, I> Iterator for #Parse<'g, I>
                where I: #InferTree<'g> + 'g,
            {
                type Item = &'g #start_type;
                fn next(&mut self) -> Option<Self::Item> {
                    match self.result.next() {
                        Some(&#Value::#start_variant(ref value)) => Some(value),
                        _ => None,
                    }
                }
            }
        }
    }

    pub fn translate_parse_builder(&self, cx: &mut rs::ExtCtxt) -> rs::Tokens {
        let UniqueNames {
            ValueInfer, InferTreeVal, Parse, ParseFactory, lower_layer_macro, ..
        } = self.unique_names;

        let traversal = quote! { traversal};
        let store = quote! { store};
        let closure = self.translate_closure(cx, traversal, store);
        let infer_wildcards = iter::repeat(quote! { _ })
                              .take(self.infer.len());

        quote! {
            // ########### QUOTED CODE #########################
            let closure = |mut parse| {
                {
                    // Partially guide the inference of the argument's type.
                    let _: &#Parse<
                        #InferTreeVal<_, #ValueInfer<_, #(#infer_wildcards),*>>
                    > = &*#lower_layer_macro!(@get parse);
                };
                let &mut #Parse {
                    ref mut traversal,
                    ref store,
                    finished_node,
                    ref mut result,
                    ..
                } = &mut *#lower_layer_macro!(@get parse);
                let root = finished_node.unwrap();
                // ===
                #closure
                // ===
                *result = match root.values().unwrap();
            };
            #lower_layer_macro!(@builder
                @factory [#ParseFactory::new()]
                @closure [closure]
            )
            // ########### END QUOTED CODE
        }
    }

    pub fn translate_layer_macro_def(&self, cx: &mut rs::ExtCtxt, arguments_from_outer_layer: &GenArgumentsFromOuterLayer) -> rs::Tokens {
        let UniqueNames {
            UpperValue, Value, Layer, UpperTerminalAccessor, ValueInfer, InferTreeVal,
            layer_macro, lower_layer_macro, ..
        } = self.unique_names;
        let dol = rs::TokenTree::Token(rs::DUMMY_SP, rs::Token::Dollar);
        // cannot put these in variables, because that would cause a compilation error.
        let traversal = quote! { #lower_layer_macro!(@get #dol parse).traversal};
        let store = quote! { #lower_layer_macro!(@get #dol parse).store};
        let closure = self.translate_closure(cx, traversal, store);
        // Terminals and their variants
        let &GenArgumentsFromOuterLayer {
            ref terminal_names,
            ref terminal_variants,
            ref terminal_bare_variants
        } = arguments_from_outer_layer;
        let terminal_names = terminal_names.iter();
        let terminal_variants = terminal_variants.iter();
        let terminal_bare_variants = terminal_bare_variants.iter();
        // Wildcards
        let infer_wildcards = iter::repeat(quote! { _ })
                              .take(self.infer.len());
        quote! {
            macro_rules! #layer_macro {
                (
                    @closure
                    #dol upper_builder:expr,
                    #dol parse:expr,
                    #dol node:expr;
                ) => ({
                    // Assist the inference.
                    {
                        let _: &::std::marker::PhantomData<
                            #InferTreeVal<_, _, #ValueInfer<_, #(#infer_wildcards),*>>
                        > = &#lower_layer_macro!(@get #dol parse).inference_marker;
                    };
                    let upper_builder = &mut #dol upper_builder;
                    let sym = (#dol node).terminal;
                    let root = (#dol node).value;
                    // === Deeper code
                    #closure
                    // === Result
                    let result = match root.get() {
                        Evaluated { values } => values,
                        _ => unreachable!()
                    };
                    upper_builder.reserve(result.len());
                    let upper_terminals = #UpperTerminalAccessor;
                    #(
                        if sym == upper_terminals.#terminal_names() {
                            for value in result {
                                let inner =
                                if let #Value::#terminal_variants(inner) = value.clone() {
                                    inner
                                } else {
                                    unreachable!()
                                };
                                upper_builder.push(#UpperValue::#terminal_bare_variants(inner));
                            }
                        }
                    )else*
                    else {
                        unreachable!("wrong sym")
                    }
                });
                (@builder @factory [#dol factory:expr] @closure [#dol closure:expr]) => (
                    #lower_layer_macro!(@builder
                        @factory [#Layer::new().with_parse_builder(#dol factory)]
                        @closure [#dol closure]
                    )
                );
                (@get #dol parse:expr) => (
                    #lower_layer_macro!(@get #dol parse.parse)
                )
            }
        }
    }

    pub fn translate_closure(
        &self,
        cx: &mut rs::ExtCtxt,
        traversal: Vec<rs::TokenTree>,
        store: Vec<rs::TokenTree>)
        -> rs::Tokens
    {
        let action_id = self.rules.iter().map(|r| &r.id);
        let rule_variant = self.rules.iter().map(|r| &r.variant);
        let rules_expr = self.rules.iter().map(|r| &r.action);
        let arg_num = self.rules.iter().map(|r| r.args.iter().map(|p| &p.num));
        let arg_variant = self.rules.iter().map(|r| r.args.iter().map(|p| &p.variant));
        let arg_pat = self.rules.iter().map(|r| r.args.iter().map(|p| &p.pat));

        let seq_action_id = self.sequences.iter().map(|s| &s.id);
        let seq_element_variant = self.sequences.iter().map(|s| &s.elem_variant);
        let seq_variant = self.sequences.iter().map(|s| &s.variant);

        let null_symbol_id = self.epsilon_actions.roots.iter().map(|root| root.sym.usize());
        // All lengths are non-zero
        let null_num_summands = self.epsilon_actions.roots.iter().map(|root| root.num);
        let null_sym_name = self.epsilon_actions.roots.iter().map(|root| root.name);
        let null_variant = self.epsilon_actions.roots.iter().map(|root| root.variant_name);

        let UniqueNames { Value, lower_layer_macro, .. } = self.unique_names;
        let Value_ = Value;

        quote! {
            // ########### QUOTED CODE #########################
            let mut cartesian_product = CartesianProduct::new();
            #traversal.traverse(root);
            loop {
                if let Some(deps) = #traversal.traverse_deps() {
                    for node in deps {
                        match node {
                            TraversalBottom::Leaf(node) => {
                                let mut builder = SliceBuilder::new(&#store, 0);
                                #lower_layer_macro!(
                                    @closure
                                    builder,
                                    parse,
                                    node;
                                );
                                node.result(builder.into_slice());
                            }
                            TraversalBottom::Null(nulling) => {
                                // The builder may be unused.
                                let mut _builder = SliceBuilder::new(&#store, 0);
                                // Use external symbols.
                                match nulling.symbol.usize() {
                                    #(
                                        #null_symbol_id => {
                                            _builder.reserve(#null_num_summands);
                                            #null_sym_name!(|result| {
                                                _builder.push(#Value::#null_variant(result));
                                            });
                                        }
                                    )*
                                    id => unreachable!("nulling id {}", id)
                                }
                                let slice = _builder.into_slice();
                                assert!(!slice.is_empty(), "built slice is empty");
                                nulling.result(slice);
                            }
                        }
                    }
                } else {
                    break;
                }
                for node in #traversal.traverse_sum() {
                    let count = node.iter().map(|alt| alt.len()).fold(0, |acc, elem| acc + elem);
                    let mut slice_builder = SliceBuilder::new(&#store, count);
                    // ... eval:
                    for alt in node.iter() {
                        cartesian_product.from_production(&alt);
                        // let mut finished = false;
                        loop {
                            let result = {
                                let args = cartesian_product.as_slice();
                                match alt.action() {
                                    #(
                                        #action_id => {
                                            // `true` is to avoid irrefutable patterns
                                            let val = (true, #(args[#arg_num].clone(),)*);
                                            if let (true,
                                                    #(#Value::#arg_variant(#arg_pat),)*) = val {
                                                #Value_::#rule_variant(#rules_expr)
                                            } else {
                                                unreachable!()
                                            }
                                        }
                                    )*
                                    #(
                                        #seq_action_id => {
                                            let seq_vec = args.iter().map(|arg| {
                                                let val = (true, (*arg).clone());
                                                if let (true,
                                                        #Value::#seq_element_variant(elem)) = val {
                                                    elem
                                                } else {
                                                    unreachable!()
                                                }
                                            }).collect::<Vec<_>>();
                                            #Value_::#seq_variant(seq_vec)
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

    pub fn translate_lexer_def(&self, cx: &mut rs::ExtCtxt) -> rs::Tokens {
        // Conditionally compile
        let item = if self.inner_layer.is_none() {
            self.translate_identity(cx)
        } else {
            self.translate_lexer_invocation(cx)
        };
        item
    }

    fn translate_identity(&self, cx: &mut rs::ExtCtxt) -> rs::Tokens {
        let UniqueNames { lower_layer_macro, .. } = self.unique_names;
        let dol = rs::TokenTree::Token(rs::DUMMY_SP, rs::Token::Dollar);
        // The inner layer is missing. Put placeholding definitions in the inner layer.
        // - How will the parse builder work with an identity layer?
        // - I think it won't. The main parse builder will be Identity.
        quote! {
            // ########### QUOTED CODE #########################
            macro_rules! #lower_layer_macro {
                (@builder @factory [#dol factory:expr] @closure [#dol closure:expr]) => (Identity);
                (@get #dol parse:expr) => (#dol parse);
            }

            pub struct Identity;
            impl Identity {
                pub fn with_parse_builder<P>(self, parse_builder: P) -> P { parse_builder }
            }
            // ########### END QUOTED CODE
        }
    }

    fn translate_lexer_invocation(&self, cx: &mut rs::ExtCtxt) -> rs::Tokens {
        // Creates a literal as a Rust token from a char value.
        fn to_char_literal(num: char) -> rs::TokenTree {
            rs::TokenTree::Token(
                rs::DUMMY_SP,
                rs::Token::Literal(
                    rs::token::Char(rs::Symbol::gensym(&*num.to_string())),
                    None
                )
            )
        }
        // Prepend an attribute.
        // Pass this parser's terminal names to this parser's lexer.
        let lexer_attr = format!("lexer_{}", self.inner_layer_level).to_ident();
        let terminal_names = self.terminal_names.iter();
        // Get the invocation.
        let invoc = self.inner_layer.as_ref().unwrap();
        let &GenInvocationOfInnerLayer { lexer_name, ref lexer_tts, .. } = invoc;
        // Turn implicit string rules into iterators.
        let str_lhs = invoc.str_lhs.iter();
        let str_rhs_exprs = invoc.str_rhs.iter().map(|rhs| self.builder.expr().str(rhs));
        // Char ranges. Only present with implicit char_classifier.
        let char_range_lhs = invoc.char_range_lhs.iter();
        let start = invoc.char_ranges.iter().map(|range| range.start).map(to_char_literal);
        let end = invoc.char_ranges.iter().map(|range| range.end).map(to_char_literal);
        // The lexer invocation.
        quote! {
            // ########### QUOTED CODE #########################
            // Inner layer.
            #lexer_name! {
                // Arguments for the inner layer.
                #![#lexer_attr(#(#terminal_names),*)]
                // Implicit string rules.
                #(
                    #str_lhs ::= #str_rhs_exprs;
                )*
                // Implicit char ranges. Only present with an implicit char_classifier.
                #(
                    #char_range_lhs ::= #start ... #end;
                )*
                // Explicit code.
                #lexer_tts
            }
            // ########### END QUOTED CODE
        }
    }
}

impl GenType {
    pub fn generate(&self) -> rs::TokenStream {
        match self {
            &GenType::RustTy(ref ty) => ty.clone(),
            &GenType::Unit => {
                quote! { () }   
            }
            &GenType::Identifier(name) => {
                quote! { #name }
            }
            &GenType::Tuple(ref fields) => {
                let types = fields.iter().map(|field|
                    field.generate()
                ).collect::<Vec<_>>();
                quote! { ( #(#types),* ) }
            }
            &GenType::Item(name) => {
                quote! { #name<I> }
            }
            &GenType::Vec(ref elem_ty) => {
                let gen_elem_ty = elem_ty.generate();
                quote! { Vec<#gen_elem_ty> }
            }
            &GenType::Infer(name) => {
                quote! { I::#name }
            }
            &GenType::Terminal => {
                quote! { I::T }
            }
        }
    }

    pub fn generate_qualified(&self, infer_trait: rs::Term) -> rs::TokenStream
    {
        match self {
            &GenType::Tuple(ref fields) => {
                let types = fields.iter().map(|field|
                    field.generate_qualified(infer_trait)
                ).collect::<Vec<_>>();
                quote! { ( #(#types),* ) }
            }
            &GenType::Item(name) => {
                quote! { #name<I::Infer> }
            }
            &GenType::Vec(ref elem_ty) => {
                let gen_elem_ty = elem_ty.generate_qualified(infer_trait);
                quote! { Vec<#gen_elem_ty> }
            }
            &GenType::Infer(name) => {
                quote! { <I::Infer as #infer_trait>::#name }
            }
            &GenType::Terminal => {
                quote! { <I::Infer as #infer_trait>::T }
            }
            _ => self.generate()
        }
    }
}
