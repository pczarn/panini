macro_rules! runtime_grammar {
    (
        sub $sub_name:ident -> ($Terminal:ty) $sub_content:tt
        $(
            $lhs:ident
            ->
            ($rule_ty:ty)
            ::=
            $(
                $rule:tt
                =>
                {$($action_tts:tt)*}
            )|+
            ;
        )*
    ) => (
        {
            #[allow(non_camel_case_types)]
            enum __RetTy {
                $(
                    $lhs($rule_ty),
                    __Terminal($Terminal),
                )*
            }
            impl __RetTy {
                $(
                    fn $lhs(self) -> $rule_ty {
                        if let __RetTy::$lhs(val) = self {
                            val
                        } else {
                            unreachable!()
                        }
                    }
                )*
            }
            let mut grammar: crate::Grammar<__RetTy, $Terminal>;
            grammar = crate::Grammar::new($sub_name!([-> $Terminal, __RetTy] $sub_content));
            $(
                grammar.rule(
                    stringify!($lhs),
                    rule!($($rule [__RetTy::$lhs]=> {{$($action_tts)*}})|+),
                );
            )*
            grammar.parser()
        }
    )
}

macro_rules! rule {
    ($rhs:ident) => {
        crate::Rule::call(stringify!($rhs))
    };
    (( $name:ident : $rhs:ident )) => {
        rule!($rhs).bind(stringify!($name))
    };
    (( $rhs:tt * )) => {
        rule!($rhs).repeat()
    };
    (( $rhs0:tt $($rhsN:tt)* )) => {
        rule!($rhs0) $(.then(rule!($rhsN)))*
    };
    ($rhs:tt [$ret:ident :: $variant:ident]=> $action:expr) => {
        rule!($rhs).action(|mut _input: Vec<$ret>| {
            declare_binds!([_input] $rhs);
            let result = $action;
            $ret::$variant(result)
        })
    };
    (
        /**/ $rhs0:tt [$ret0:ident :: $variant0:expr]=> {$action0:expr}
        $(|  $rhsN:tt [$retN:ident :: $variantN:expr]=> {$actionN:expr})+
    ) => {
        /***/ rule!($rhs0 [$ret0::$variant0]=> $action0)
        $(.or(rule!($rhsN [$retN::$variantN]=> $actionN)))+
    };
}

macro_rules! declare_binds {
    ([$input:ident] $rhs:ident) => ();

    ([$input:ident] ($name:ident : $rhs:ident)) => (
        let $name = $input.pop().unwrap().$rhs();
    );

    ([$input:ident] ($($rhs:tt)*)) => {
        $(declare_binds!([$input] $rhs);)*
    };
}
