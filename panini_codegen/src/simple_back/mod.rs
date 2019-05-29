pub mod generate;
pub mod trans;

// External and internal symbols are different.
// The backend mostly uses internal symbols.

pub use self::trans::IrTranslator;
pub use self::generate::GenResult;
