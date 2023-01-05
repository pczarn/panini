#[macro_export]
macro_rules! input {
    (
        $(
            $lhs:ident
            ::=
            $(
                $rule:tt
            )|+
            ;
        )*
    ) => (
        {
            use $crate::input::FrozenInput;
            use $crate::graph::{Step, IdxKind, GroupKind};
            let mut input = FrozenInput::new();
            $(
                let lhs_str = stringify!($lhs);
                let args = vec![
                    $(
                        (
                            input.next_stmt_idx(lhs_str),
                            rule!(input: input, rhs: $rule),
                        ),
                    )*
                ];
                let children = args.into_iter().map(
                    |(step, child_node_id)| {
                        let node = input.graph_mut().node(Step::Idx(step), vec![child_node_id]);
                        input.graph_mut().node(Step::Group(GroupKind::Stmt), vec![node]);
                    }
                ).collect();
                let step = Step::StmtFragment(input.intern_fragment(lhs_str));
                input.graph_mut().node(
                    step,
                    children,
                );
            )*
            input.thaw()
        }
    )
}

#[macro_export]
macro_rules! rule {
    (input: $input:expr, rhs: $rhs:ident) => {
        {
            node! {
                (Step::Fragment)
                --
                (Step::Symbol { symbol: Box::new(stringify!($rhs)) })
            }
        }
    };
    (input: $input:expr, rhs: ( $label:ident : $rhs:tt )) => {
        {
            node! {
                (Step::Bind { label: Box::new(stringify!($label)) })
                --
                (rule!(input: $input, $rhs))
            }
        }
    };
    (input: $input:expr, rhs: ( $rhs:tt * )) => {
        {
            let step = Step::Sequence { min: 0, max: None };
            let child = rule!(input: $input, $rhs);
            $input.graph_mut().node(step, vec![child])
        }
    };
    (input: $input:expr, rhs: ( $($rhs:tt)* )) => {
        {
            let children = vec![
                $(
                    rule!(input: $input, $rhs),
                )*
            ];
            let children: Vec<_> = children
                .into_iter()
                .enumerate()
                .map(|(i, child)| {
                    $input.graph_mut().node(Step::Idx(i), vec![child])
                })
                .collect();
            $input.graph_mut().node(Step::Group(GroupKind::Product), children)
        }
    };
}

#[macro_export]
macro_rules! node {
    (($parent:expr) -- ($child:expr)) => {
        {
            PATHWAY_GRAPH.node($parent, vec![node!{ $child }])
        }
    }
}