macro_rules! enum_stream {
    (
        [-> $Terminal:ty, $RetTy:ident]
        {
            $(
                $lhs:ident
                =
                $rhs:tt
                ;
            )*
        }
    ) => (
        {
            impl $RetTy {
                $(
                    fn $lhs(self) -> $Terminal {
                        if let $RetTy::__Terminal(val) = self {
                            val
                        } else {
                            unreachable!()
                        }
                    }
                )*
            }
            let mut grammar = crate::EnumStreamGrammar::new();
            $(
                grammar.pattern(
                    stringify!($lhs),
                    enum_stream_pat!($rhs),
                );
            )*
            grammar
        }
    )
}

macro_rules! enum_stream_pat {
    (( !$pat:tt )) => {
        enum_stream_pat!($pat).negate()
    };
    ($pat:pat) => {
        crate::Pattern::pattern(|token| {
            #[allow(unreachable_patterns)]
            match token {
                $pat => true,
                _ => false,
            }
        })
    };
    ($elem0:tt $(&& $elemN:tt)+) => {
        enum_stream_pat!($elem0) $(.and(enum_stream_pat!($elemN)))+
    };
}
