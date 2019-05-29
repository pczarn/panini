#![allow(dead_code)]

// Public imports

pub use proc_macro2::{Span, Ident, TokenStream, Literal, Punct, Spacing, TokenTree};

pub use quote::ToTokens;

pub use syn::{
  Pat, Expr, Meta, Attribute,
};

pub use syn::Token;

pub use syn::token::{Token, Dollar};

pub fn str_to_ident<S: AsRef<str>>(name: S) -> Ident {
    // FIXME change to Span::def_site()
    Ident::new(name.as_ref(), Span::call_site())
}
