#[derive(Clone)]
pub struct Input {
    graph: PathwayGraph,
    lhs_set: IndexSet<String>,
    bind_set: IndexSet<String>,
    rule_indices: BTreeMap<String, usize>,
}

pub struct FrozenInput {
    graph: PathwayGraph,
    lhs_set: FrozenIndexSet<String>,
    bind_set: FrozenIndexSet<String>,
    rule_indices: BTreeMap<String, usize>,
}

#[derive(Debug)]
pub struct InputDebug<'a> {
    graph: &'a PathwayGraph,
    lhs_set: Vec<String>,
    bind_set: Vec<String>,
    rule_indices: &'a BTreeMap<String, usize>,
}

impl ::std::fmt::Debug for Input {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        fn interner_to_vec(set: &IndexSet<String>) -> Vec<String> {
            let mut result = vec![];
            for i in 0usize.. {
                if let Some(elem) = set.get_index(i) {
                    result.push(elem.to_string());
                } else {
                    break;
                }
            }
            result
        }
        InputDebug {
            graph: &self.graph,
            lhs_set: interner_to_vec(&self.lhs_set),
            bind_set: interner_to_vec(&self.bind_set),
            rule_indices: &self.rule_indices,
        }
        .fmt(fmt)
    }
}

impl FrozenInput {
    fn new() -> Self {
        FrozenInput {
            graph: PathwayGraph::new(),
            lhs_set: FrozenIndexSet::new(),
            bind_set: FrozenIndexSet::new(),
            rule_indices: BTreeMap::new(),
        }
    }

    fn intern_fragment(&mut self, ident: &str) -> FragmentId {
        self.lhs_set.insert_full(ident.to_string()).0 as FragmentId
    }

    fn intern_bind(&mut self, ident: &str) -> FragmentId {
        self.bind_set.insert_full(ident.to_string()).0 as FragmentId
    }

    fn next_stmt_idx(&mut self, lhs_str: &str) -> usize {
        let entry = self.rule_indices.entry(lhs_str.to_string()).or_insert(0);
        let result = *entry;
        *entry += 1;
        result
    }

    fn thaw(self) -> Input {
        Input {
            graph: self.graph,
            lhs_set: self.lhs_set.into_set(),
            bind_set: self.bind_set.into_set(),
            rule_indices: self.rule_indices,
        }
    }
}
