use input::FragmentId;

/// Attribute parameters.
#[derive(Clone, Debug)]
pub struct Parameters {
    pub lexer_parameters: Option<LexerParameters>,
}

/// The outer layer invokes this layer, passing arguments. These arguments consist of
/// this layer's level number and a list of terminals.
#[derive(Clone, Debug)]
pub struct LexerParameters {
    pub level: usize,
    pub terminals: Vec<FragmentId>,
}
