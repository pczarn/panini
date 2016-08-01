use cfg::Symbol;

use rs;
use middle::trace::SourceOrigin;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EmbeddedString<S = Symbol> {
    pub symbol: rs::Spanned<S>,
    pub string: rs::Spanned<rs::Name>,
    pub source: SourceOrigin,
}
