use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, BinaryHeap};
use std::mem;

use cfg::Symbol;

use input::FragmentId;
use middle::flatten_stmts::{Path, Position};
use middle::trace::Trace;

pub struct TypeCollector {
    queue: BinaryHeap<PathWithType>,
    pub types: BTreeMap<Path, BTreeSet<Type>>,
    pub conflicts: BTreeMap<Path, TypeConflicts>,
    pub final_types: BTreeMap<Path, Type>,
}

struct TypeMerger {
    tuple: BTreeMap<usize, Type>,
    tuple_keys: BTreeSet<usize>,
    sequence: Option<Type>,
    struct_: BTreeMap<Path, Type>,
    conflicts: TypeConflicts,
}

pub struct TypeConflicts {
    tuple: BTreeSet<usize>,
    struct_: bool,
    sequence: BTreeSet<Type>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PathWithType {
    path: Path,
    ty: Type,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Type {
    Tuple { fields: BTreeMap<usize, Type> },
    Sequence { ty: Box<Type> },
    Struct { fields: BTreeMap<Path, Type> },
    TypeOfFragment { fragment: FragmentId },
    // TypeOfSymbol {
    //     symbol: Symbol,
    // },
    Bottom,
}

// #[ast]
// mod type {
//     #[ast_branch]
//     enum Branch {
//         TupleField(usize),
//         Sequence,
//         StructField(Path),
//         Fragment(FragmentId),
//         Bottom,
//     }
// }

impl Ord for PathWithType {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.path.position.len(), &self.path, &self.ty).cmp(&(
            other.path.position.len(),
            &other.path,
            &other.ty,
        ))
    }
}

impl PartialOrd for PathWithType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl TypeMerger {
    fn new() -> TypeMerger {
        TypeMerger {
            tuple: BTreeMap::new(),
            tuple_keys: BTreeSet::new(),
            sequence: None,
            struct_: BTreeMap::new(),
            conflicts: TypeConflicts::new(),
        }
    }

    fn merge(&mut self, ty: Type) {
        match ty {
            Type::Sequence { ty } => {
                if let Some(target) = self.sequence.as_mut() {
                    if &*target != &*ty {
                        self.conflicts.sequence.insert(target.clone());
                        self.conflicts.sequence.insert(*ty);
                    }
                } else {
                    self.sequence = Some(*ty);
                }
            }
            Type::Tuple { fields } => {
                let keys: BTreeSet<usize> = fields.keys().cloned().collect();
                self.conflicts
                    .tuple
                    .extend(self.tuple_keys.intersection(&keys));
                self.tuple.extend(fields.into_iter());
                self.tuple_keys.extend(keys.into_iter());
            }
            // a:foo | b:foo
            Type::Struct { fields } => {
                if !self.struct_.is_empty() && fields != self.struct_ {
                    self.conflicts.struct_ = true;
                } else {
                    self.struct_.extend(fields.into_iter());
                }
            }
            Type::Bottom | Type::TypeOfFragment { .. } => unreachable!(),
        }
    }

    fn finish_merge(&mut self) {
        if let &Some(ref ty) = &self.sequence {
            if !self.tuple.is_empty() && !self.struct_.is_empty() {
                self.conflicts.sequence.insert(ty.clone());
            }
        }
    }

    fn merged_type(self) -> Type {
        if let Some(ty) = self.sequence {
            Type::Sequence { ty: Box::new(ty) }
        } else {
            if !self.struct_.is_empty() {
                Type::Struct {
                    fields: self.struct_,
                }
            } else {
                Type::Tuple { fields: self.tuple }
            }
        }
    }
}

impl TypeConflicts {
    fn new() -> TypeConflicts {
        TypeConflicts {
            tuple: BTreeSet::new(),
            struct_: false,
            sequence: BTreeSet::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.tuple.is_empty() && !self.struct_ && self.sequence.is_empty()
    }
}

impl TypeCollector {
    pub fn new() -> Self {
        TypeCollector {
            queue: BinaryHeap::new(),
            types: BTreeMap::new(),
            conflicts: BTreeMap::new(),
            final_types: BTreeMap::new(),
        }
    }

    pub fn collect(&mut self, input_tree: InputTree) {
        self.decompose_types(input_tree);
        self.merge_types();
    }

    fn decompose_types(&mut self, input_tree: InputTree) {
        // let not_start_position = |(backward_way, _forward_ways)| !backward_way.is_empty();
        // let map = |(backward_way, forward_ways)| {
        //     let forward_types = forward_ways.map(|way| {
        //         way.filter_map(|step| {
        //             match step {
        //                 Step::Fragment(fragment) => Some(Type::Fragment(fragment)),
        //                 Step::Idx(idx) => Some(Type::TupleField(idx)),
        //                 Step::Sequence { min, max } => {
        //                     if let Some(&Step::Bind(..)) = backward_way.last() {
        //                         Type::Sequence {
        //                             ty: Box::new(Type::Struct { fields })
        //                         }
        //                     } else {
        //                         Type::struct_with_position(fields, Position::Sequence { min, max })
        //                     }
        //                 }

        //                 Step::StmtIdx(..)
        //                     | Step::Alternative(_) =>
        //                         None

        //                 Step::SequenceEnd
        //                     | Step::SequenceToken
        //                     | Step::Max
        //                     | Step::StmtFragment(..) =>
        //                         unreachable!()
        //             }
        //         })
        //     }).collect();
        //     (backward_way.collect(), forward_types)
        // };

        // let types = input_tree.positions()
        //                       .filter(not_start_position)
        //                       .map(map).collect();
        // self.types = types;

        for path in &input_tree.paths {
            path.prefixes()
                .rev()
                .filter(|prefix| prefix.position.len() >= 2)
                .fold(Type::Bottom, |acc, elem| {
                    let (last_position, prefix) = elem.split_last();
                    let ty = match (last_position, acc) {
                        (Position::Fragment(fragment), Type::Bottom) => {
                            Type::TypeOfFragment { fragment }
                        }
                        (Position::Fragment(..), _) => unreachable!(),
                        (Position::Idx(idx), ty) => Type::Tuple {
                            fields: btreemap! {
                                idx => ty
                            },
                        },
                        (Position::StmtIdx(..), ty) => ty,
                        (Position::Alternative(_), ty) => ty,
                        (Position::Sequence { min, max }, Type::Struct { fields }) => {
                            if let Some(&Position::Bind(..)) = prefix.position.last() {
                                Type::Sequence {
                                    ty: Box::new(Type::Struct { fields }),
                                }
                            } else {
                                Type::struct_with_position(fields, Position::Sequence { min, max })
                            }
                        }
                        (Position::Sequence { .. }, ty) => Type::Sequence { ty: Box::new(ty) },
                        (bind @ Position::Bind { .. }, ty) => Type::Struct {
                            fields: btreemap! {
                                path![bind] => ty
                            },
                        },
                        (Position::SequenceEnd, _)
                        | (Position::SequenceToken, _)
                        | (Position::Max, _)
                        | (Position::StmtFragment(..), _) => unreachable!(),
                    };
                    self.types
                        .entry(prefix)
                        .or_insert(BTreeSet::new())
                        .insert(ty.clone());
                    ty
                });
        }
    }

    fn merge_types(&mut self) {
        for (path, ty_set) in self.types.iter_mut() {
            let mut merger = TypeMerger::new();
            for mut ty in ty_set.iter().cloned() {
                ty.simplify_tuples();
                merger.merge(ty);
            }
            merger.finish_merge();
            if !merger.conflicts.is_empty() {
                self.conflicts.insert(
                    path.clone(),
                    mem::replace(&mut merger.conflicts, TypeConflicts::new()),
                );
            }
            self.final_types.insert(path.clone(), merger.merged_type());
        }
    }

    pub fn check_type_equality(&self, trace: &Trace) -> Option<Vec<Vec<(usize, usize)>>> {
        let mut all_spans = vec![];
        for (path, ty_set) in &self.types {
            if ty_set.len() > 1 {
                let mut spans = vec![];
                for ty in ty_set {
                    let mut paths = vec![path.clone()];
                    let path_range = path.clone().range();
                    for (path, ty_set) in self.types.range(path_range) {
                        if ty_set.contains(ty) {
                            paths.push(path.clone());
                        }
                    }
                    let mut longest_path = paths
                        .into_iter()
                        .max_by_key(|path| path.position.len())
                        .unwrap();
                    let mut longest_path_range = longest_path.clone().range();
                    longest_path_range
                        .start
                        .position
                        .push(Position::Alternative(!0));
                    longest_path.position.push(Position::Alternative(!0));
                    let start = trace.tokens.range(..longest_path).count();
                    let end = start + trace.tokens.range(longest_path_range.clone()).count();
                    spans.push((start, end));
                }
                all_spans.push(spans);
            }
        }
        if all_spans.is_empty() {
            None
        } else {
            Some(all_spans)
        }
    }

    // pub fn add_lhs(&mut self, grammar: &mut Grammar, sym_map: &mut SymMap, rules: &RuleRewriteResult) {
    //     for (ref path, ref rule_value) in rules.rules {
    //         let value = self.types.entry(path.clone).or_insert(BTreeSet::new());
    //         let symbol = sym_map.intern(grammar, &rule_value.lhs);
    //         value.insert(Type::TypeOfSymbol { symbol });
    //     }
    // }

    // fn rewrite_rules(&mut self) {
    //     for (path, ty_set) in &self.types {
    //         if ty_set.len() != 1 {
    //             continue;
    //         }
    //         let ty = ty_set.iter().next();
    //         match (path.last().cloned().unwrap(), ty) {
    //             (Position::IdxWithFragment { .. }, &Type::Sequence { .. }) => {}
    //             (Position::Sequence { min, max }, Type::TypeOfFragment { fragment }) => {
    //                 self.rules.push(RuleValue {

    //                 })
    //             }
    //         }
    //     }
    // }
}

impl Type {
    fn simplify_tuples(&mut self) {
        let replace = match self {
            &mut Type::Tuple { ref mut fields } => {
                for ty in fields.values_mut() {
                    ty.simplify_tuples();
                }
                if fields.len() == 1 && fields.contains_key(&0) {
                    Some(fields[&0].clone())
                } else {
                    None
                }
            }
            &mut Type::Struct { ref mut fields } => {
                for ty in fields.values_mut() {
                    ty.simplify_tuples();
                }
                None
            }
            &mut Type::Sequence { ref mut ty } => {
                ty.simplify_tuples();
                None
            }
            _ => None,
        };
        if let Some(ty) = replace {
            *self = ty;
        }
    }

    fn struct_with_position(fields: BTreeMap<Path, Type>, position: Position) -> Type {
        let mut fields: Vec<_> = fields.into_iter().collect();
        for &mut (ref mut path, ref mut ty) in &mut fields {
            match &position {
                &Position::Sequence { .. } => {
                    let ty_inner = Box::new(ty.clone());
                    *ty = Type::Sequence { ty: ty_inner };
                }
                _ => unreachable!(),
            }
            path.position.insert(0, position);
        }
        Type::Struct {
            fields: fields.into_iter().collect(),
        }
    }
}
