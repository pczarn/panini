#![recursion_limit = "256"]
#![feature(rustc_private)]
// plugin_registrar, 

extern crate log;
extern crate env_logger;

extern crate bit_matrix;
extern crate cfg;
extern crate cfg_regex;
extern crate gearley;

extern crate enum_coder;

#[macro_use]
extern crate maplit;

pub mod tokens;

use middle::ir::Ir;
use middle::error::TransformationError;
use input::PathwayGraph;
use output::instruction::InstructionList;
use output::translator::IrTranslator;

const STAGES_PROCESS: &'static [fn(tokens::Tokens) -> tokens::Tokens] = [
    stage0::process,
    stage1::process,
];

const NUM_STAGES: usize = 2;

enum Stage {
    Begin(Vec<tokens::Tokens>),
    Check(Vec<tokens::Tokens>),
    None,
}

struct StageList {
    list: Vec<Stage>,
}

pub fn process(input: tokens::Tokens) -> stage10::Output {
    let stage_list = parse_stage_list(input);
    STAGES_PROCESS.iter().zip(stage_list.list.iter().chain(iter::once))
    stage0::process(input)
}

// fn parse_stage_list(input: tokens::Tokens) -> StageList {
//     let mut current_stage = 0;
//     let mut stage_tokens: HashMap<usize, Vec<tokens::Tokens>>;
//     let mut tokens_iter = input.tokens.into_iter();
//     let append = |token| { stage_tokens.entry(current_stage).or_insert(vec![]).push(token); };
//     while let Some(token) = tokens_iter {
//         match token {
//             Token::Pound => {
//                 match (tokens_iter.as_slice().get(0), tokens_iter.as_slice().get(1), tokens_iter.as_slice().get(2)) {
//                     (
//                         Some(&Token::OpenDelim(DelimToken::Bracket)),
//                         Some(&Token::Ident(ref attr_name)),
//                         Some(&Token::CloseDelim(DelimToken::Bracket)),
//                     ) if attr_name.starts_with("stage") => {
//                         let stage_num: Option<u32> = attr_name["stage".len() ..].parse();
//                         match stage_num {
//                             Some(num) => {
//                                 current_stage = num;
//                                 true
//                             }
//                             None => {
//                                 Some(Token::Pound)
//                             }
//                         }
//                     }
//                     _ => {
//                         Some(Token::Pound)
//                     }
//                 }
//             }
//             Token::OpenDelim(DelimToken::Brace) => {

//                 Some(other)
//             }
//             other => {
//                 Some(other)
//             }
//         }
//     }
// }
