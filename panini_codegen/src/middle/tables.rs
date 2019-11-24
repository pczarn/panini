use indexmap::IndexSet;
use panini_logic::input::{BindId, FragmentId, TyId};
use rs;

pub struct Tables {
    name_set: IndexSet<rs::Term>,
    ty_set: IndexSet<rs::Ty>,
    bind_set: IndexSet<rs::Bind>,
}

impl Tables {
    fn intern_fragment(&mut self, term: rs::Term) -> FragmentId {
        self.name_set.insert_full(term).0 as FragmentId
    }

    fn intern_ty(&mut self, ty: rs::Ty) -> TyId {
        self.ty_set.insert_full(ty).0 as TyId
    }

    fn intern_bind(&mut self, bind: rs::Bind) -> BindId {
        self.bind_set.insert_full(bind).0 as BindId
    }
}
