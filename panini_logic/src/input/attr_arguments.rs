use input::FragmentId;

#[derive(Clone, Debug)]
pub struct AttrArguments {
    pub lexer_arguments: Option<LexerArguments>,
}

/// The outer layer invokes this layer, passing arguments. These arguments consist of
/// this layer's level number and a list of terminals.
#[derive(Clone, Debug)]
pub struct LexerArguments {
    pub level: usize,
    pub terminals: Vec<FragmentId>,
}
