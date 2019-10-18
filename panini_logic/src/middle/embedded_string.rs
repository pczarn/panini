use cfg::Symbol;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EmbeddedString {
    pub symbol: Symbol,
    pub string: String,
    pub source: Vec<usize>,
}
