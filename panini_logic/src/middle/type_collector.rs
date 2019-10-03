use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, BinaryHeap};

use input::FragmentId;
use middle::flatten_stmts::{Path, Position};

pub struct TypeCollector {
    queue: BinaryHeap<PathWithType>,
    pub types: BTreeMap<Path, BTreeSet<Type>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PathWithType {
    path: Path,
    ty: Type,
}

impl Ord for PathWithType {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.path.position.len(), &self.path, &self.ty).cmp(&(other.path.position.len(), &other.path, &other.ty))
    }
}

impl PartialOrd for PathWithType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl TypeCollector {
    pub fn new() -> Self {
        TypeCollector {
            queue: BinaryHeap::new(),
            types: BTreeMap::new(),
        }
    }

    pub fn collect(&mut self, paths: Vec<Path>) {
        self.queue.clear();
        self.queue.extend(paths.into_iter().map(|path| PathWithType { path, ty: Type::Bottom }));

        while let Some(path_with_type) = self.queue.pop() {
            let (&last_position, prefix_path_a) = path_with_type.path.split_last();
            let prefix_path = prefix_path_a.clone();
            println!("collect prefix => {:#?}, last => {:#?}, ty => {:#?}", prefix_path, last_position, path_with_type.ty);
            let ty = match (last_position, path_with_type.ty) {
                (Position::IdxWithFragment { idx, fragment }, Type::Bottom) => {
                    let fields = btreemap! { idx => Type::TypeOfFragment { fragment } };
                    let ty = Type::Tuple { fields };
                    let entry = self.types.entry(prefix_path).or_insert(BTreeSet::new());
                    Type::merge(entry, ty)
                }
                (Position::IdxWithFragment { .. }, ty) => {
                    // let entry = self.types.entry(prefix_path).or_insert(BTreeSet::new());
                    // Type::merge(entry, ty)
                    ty
                }
                (Position::Alternative(_), ty) => {
                    // let ty = Type::Tuple { fields };
                    println!("alternative {:#?}", self.types.get(&prefix_path));
                    let entry = self.types.entry(prefix_path).or_insert(BTreeSet::new());
                    let ty = Type::merge(entry, ty);
                    println!("alternative {:#?} {:#?}", entry, ty);
                    ty
                }
                (Position::Sequence { min, max }, Type::Struct { fields }) => {
                    let ty = Type::struct_with_position(fields, Position::Sequence { min, max });
                    let entry = self.types.entry(prefix_path).or_insert(BTreeSet::new());
                    Type::merge(entry, ty)
                }
                (Position::Sequence { .. }, type_b) => {
                    let ty = Type::Sequence { ty: Box::new(type_b) };
                    let entry = self.types.entry(prefix_path).or_insert(BTreeSet::new());
                    Type::merge(entry, ty)
                }
                (Position::Bind(bind_id), type_b) => {
                    let ty = Type::Struct {
                        fields: btreemap! {
                            Path {
                                position: vec![Position::Bind(bind_id)]
                            } => type_b
                        }
                    };
                    let entry = self.types.entry(prefix_path).or_insert(BTreeSet::new());
                    Type::merge(entry, ty)
                }
                (Position::Idx(idx), type_b) => {
                    let ty = Type::Tuple {
                        fields: btreemap! {
                            idx => type_b
                        }
                    };
                    let entry = self.types.entry(prefix_path).or_insert(BTreeSet::new());
                    Type::merge(entry, ty)
                }
                (Position::SequenceToken, _) | (Position::Max, _) => unreachable!()
            };
            if prefix_path_a.position.len() > 0 {
                if self.queue.iter().all(|elem| {
                    let (_, prefix) = elem.path.position.split_last().unwrap();
                    let prefix_path_b = Path { position: prefix.to_vec() };
                    prefix_path_b != prefix_path_a
                }) {
                    println!("queue => {:#?}; new prefix => {:#?}, ty => {:#?}", self.queue, prefix_path_a, ty);
                    self.queue.push(PathWithType {
                        path: prefix_path_a.clone(),
                        ty: ty,
                    });
                }
            }
        }
    }

    pub fn simplify_tuples(&mut self) {
        for (_, ty_set) in &mut self.types {
            let mut tys: Vec<_> = ty_set.iter().cloned().collect();
            for ty in &mut tys {
                ty.simplify_tuples();
            }
            *ty_set = tys.into_iter().collect();
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Type {
    Tuple {
        fields: BTreeMap<usize, Type>,
    },
    Struct {
        fields: BTreeMap<Path, Type>,
    },
    Sequence {
        ty: Box<Type>,
    },
    TypeOfFragment {
        fragment: FragmentId,
    },
    Bottom,
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
            _ => None
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
                _ => unreachable!()
            }
            path.position.insert(0, position);
        }
        Type::Struct { fields: fields.into_iter().collect() }
    }

    fn merge(types: &mut BTreeSet<Type>, ty: Type) -> Type {
        let mut queue: Vec<_> = types.iter().cloned().collect();
        queue.push(ty);
        let mut result = BTreeSet::new();
        loop {
            let mut take_2 = [queue.pop(), queue.pop()];
            take_2.sort();
            match take_2 {
                [Some(type_a), Some(type_b)] => match [type_a, type_b] {
                    [Type::Tuple { fields: mut fields_a }, Type::Tuple { fields: fields_b }] => {
                        let field_indices_a: BTreeSet<_> = fields_a.keys().collect();
                        let field_indices_b: BTreeSet<_> = fields_b.keys().collect();
                        if field_indices_a.is_disjoint(&field_indices_b) {
                            fields_a.extend(fields_b.into_iter());
                            let ty = Type::Tuple { fields: fields_a };
                            result.insert(ty.clone());
                            queue.push(ty);
                        } else {
                            result.insert(Type::Tuple { fields: fields_a.clone() });
                            result.insert(Type::Tuple { fields: fields_b });
                            queue.push(Type::Tuple { fields: fields_a });
                        }
                    }
                    [Type::Tuple { .. }, type_b] | [type_b, Type::Tuple { .. }] => {
                        result.insert(type_b.clone());
                        queue.push(type_b);
                    }
                    [Type::Struct { fields: mut fields_a }, Type::Struct { fields: fields_b }] => {
                        for (key, value) in fields_b {
                            fields_a.insert(key, value);
                        }
                        let ty = Type::Struct { fields: fields_a };
                        result.insert(ty.clone());
                        queue.push(ty);
                    }
                    [Type::Struct { fields }, Type::Sequence { ty }] => {
                        result.insert(Type::Struct { fields: fields.clone() });
                        result.insert(Type::Sequence { ty });
                        queue.push(Type::Struct { fields });
                    }
                    // [Type::Tuple { fields }, Type::Sequence { ty }] => {
                    //     match ty {
                    //         &mut Type::Tuple { ref mut fields } => {
                    //             fields.insert(idx, Type::TypeOfFragment { fragment })
                    //         }
                    //         _ => unreachable!()
                    //     }
                    // }
                    // [Type::Struct {}]
                    [Type::Sequence { ty }, type_b] => {
                        result.insert(Type::Sequence { ty });
                        result.insert(type_b.clone());
                        queue.push(type_b);
                    }
                    [Type::TypeOfFragment { .. }, _]
                    | [_, Type::TypeOfFragment { .. }]
                    | [Type::Bottom, _]
                    | [_, Type::Bottom] => unreachable!(),
                },
                [Some(ty), None] | [None, Some(ty)] => {
                    result.insert(ty.clone());
                    *types = result;
                    return ty;
                }
                [None, None] => panic!("{:#?}", queue)
            }
        }
    }
}
