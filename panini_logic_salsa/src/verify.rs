

use proc_macro2::{Delimiter, TokenTree};

#[derive(Clone, Copy, Debug)]
enum VerifyState {
    ExpectName,
    ExpectEq,
    ExpectContent,
}

#[derive(Clone, Debug)]
struct VerifyGroup {
    name: String,
    content: TokenStream,
}

impl VerifyGroup {
    fn new() -> Self {
        VerifyGroup {
            name: String::new(),
            content: TokenStream::new(),
        }
    }
}

fn verify(input: Input, tokens: TokenStream) {
    let mut groups = vec![];
    let mut what_to_expect = vec![
        VerifyState::ExpectName,
        VerifyState::ExpectEq,
        VerifyState::ExpectEq,
        VerifyState::ExpectContent,
    ]
    .into_iter()
    .cycle();
    let mut state = VerifyGroup::new();
    for (token_tree, step) in tokens.into_iter().zip(what_to_expect) {
        match (token_tree, step) {
            (TokenTree::Ident(ident), VerifyState::ExpectName) => {
                state.name = ident.to_string();
            }
            (TokenTree::Punct(punct), VerifyState::ExpectEq) => {
                assert_eq!(punct.as_char(), '=');
            }
            (TokenTree::Group(group), VerifyState::ExpectContent) => {
                assert_eq!(group.delimiter(), Delimiter::Brace);
                state.content = group.stream();
                groups.push(mem::replace(&mut state, VerifyGroup::new()));
            }
            (token_tree, step) => {
                panic!(
                    "unexpected token tree {:?}, current state {:?}",
                    token_tree, step
                );
            }
        }
    }

    // println!("{:?}", input);

    let mut db = DatabaseStruct::default();
    db.set_input(Rc::new(input));

    for group in groups {
        match &group.name[..] {
            "input" => {
                assert_eq!(db.pretty_input(), group.content.to_string());
            }
            "flattened" => {
                unimplemented!();
            }
            "trace" => {
                unimplemented!();
            }
            "rewritten" => {
                unimplemented!();
            }
            "types" => {
                unimplemented!();
            }
            _ => {}
        }
    }
}