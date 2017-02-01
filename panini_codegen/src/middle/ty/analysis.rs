// pub type_map: HashMap<S, Ty<S>>,
// pub type_equality: Vec<(S, Ty<S>)>,
// pub embedded_strings: Vec<EmbeddedString<S>>,

struct TypeInfo {
    types: Vec<>,
}

impl {
    pub fn analyze(hir: &mut Hir) {
        hir.graph.iter().map(|node| {
            let ty = match &node.item {
                &Product { ref factors } => {
                    let mut named = HashMap::new();
                    let mut unnamed = vec![];
                    for &factor in factors {
                        match hir.graph[factor] {
                            Bounded { item, bind } => {
                                named.insert(bind, item);
                            }
                            _ => {
                                unnamed.push(factor);
                            }
                        }
                    }
                    if named.is_empty() {
                        Auto(Tuple {
                            factors: unnamed,
                        })
                    } else {
                        Auto(Struct {
                            factors: named,
                        })
                    }
                    Auto(Struct {
                        factors:
                    })
                    Tuple {
                        factors: factors.clone(),
                    }
                }
                &Sum { ref summands } => {
                }
                &
            }
            (node.lhs, )
        })
        Analysis {
            type_map:
        }
    }

    pub fn check_type_equality(&mut self) -> bool {
        let type_equality = mem::replace(&mut self.type_equality, vec![]);
        for (sym, ty) in type_equality {
            if !self.ty_equal(sym, &ty) {
                return false;
            }
        }
        true
    }

    fn sym_ty_equal(&self, left: SymbolicName, right: SymbolicName) -> bool {
        // Prevent deep comparison of same types.
        if left == right {
            return true;
        }
        match (self.type_map.get(&left), self.type_map.get(&right)) {
            (Some(_), Some(right_ty)) => {
                self.ty_equal(left, right_ty)
            }
            (None, None) => {
                // Terminals.
                true
            }
            _ => {
                false
            }
        }
    }

    fn ty_equal(&self, left: SymbolicName, right: &Ty<SymbolicName>) -> bool {
        let left_ty = &self.type_map[&left];
        match (left_ty, right) {
            (&Ty::Auto(ref l_auto), &Ty::Auto(ref r_auto)) => {
                self.auto_ty_equal(l_auto, r_auto)
            }
            (&Ty::SequenceVec(l_sym), &Ty::SequenceVec(r_sym)) => {
                return self.sym_ty_equal(l_sym, r_sym);
            }
            (&Ty::RustTy(_), ty) | (_, ty @ &Ty::RustTy(_)) => {
                self.assert_type_equality.borrow_mut().push((left, ty.clone()));
                return true;
            }
            (&Ty::Infer, _) | (_, &Ty::Infer) => {
                return true;
            }
            _ => {
                return false;
            }
        }
    }

    fn auto_ty_equal(&self, left: &AutoTy<SymbolicName>, right: &AutoTy<SymbolicName>) -> bool {
        match (left, right) {
            (&AutoTy::Tuple { fields: ref left_syms },
             &AutoTy::Tuple { fields: ref right_syms }) => {
                for (&left_sym, &right_sym) in left_syms.iter().zip(right_syms.iter()) {
                    if !self.sym_ty_equal(left_sym, right_sym) {
                        return false;
                    }
                }
                left_syms.len() == right_syms.len()
            }
            (&AutoTy::Struct { members: ref left },
             &AutoTy::Struct { members: ref right }) => {
                for ((l_pat, &l_sym), (r_pat, &r_sym)) in left.iter().zip(right.iter()) {
                    if l_pat != r_pat || !self.sym_ty_equal(l_sym, r_sym) {
                        return false;
                    }
                }
                left.len() == right.len()
            }
            _ => {
                false
            }
        }
    }
