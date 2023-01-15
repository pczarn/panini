use std::fmt::{Debug, Display};
use std::hash::Hash;

use proc_macro2::TokenStream;

macro_rules! value {
    (
        $Value:ident
    ) => (
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        pub struct $Value(salsa::InternId);
        
        impl salsa::InternKey for $Value {
            fn from_intern_id(id: salsa::InternId) -> Self {
                Self(id)
            }
        
            fn as_intern_id(&self) -> salsa::InternId {
                self.0
            }
        }
    )
}

macro_rules! impls {
    (trait $Data:ident: Debug + Display + ToTokenStream + trait $ToData:ident) => {
        pub trait $ToData {
            fn to_data(&self) -> Box<dyn $Data>;
        }

        pub trait $Data: Debug + Display + ToTokenStream + $ToData {}

        impl Hash for Box<dyn $Data> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.to_string().hash(state)
            }
        }
        
        impl Eq for Box<dyn $Data> {
            fn assert_receiver_is_total_eq(&self) {}
        }
        
        impl PartialEq for Box<dyn $Data> {
            fn eq(&self, other: &Self) -> bool {
                self.to_string().eq(&other.to_string())
            }
        }

        impl Clone for Box<dyn $Data> {
            fn clone(&self) -> Self {
                self.to_data()
            }
        }
    };
}

pub trait ToTokenStream {
    fn to_token_stream(&self) -> TokenStream;
}

impls!(trait SymbolData: Debug + Display + ToTokenStream + trait ToSymbolData);

value!(Symbol);

impls!(trait LabelData: Debug + Display + ToTokenStream + trait ToLabelData);

value!(Label);

impls!(trait TypeData: Debug + Display + ToTokenStream + trait ToTypeData);

value!(Type);
