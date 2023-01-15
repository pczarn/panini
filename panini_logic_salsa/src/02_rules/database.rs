use crate::rules::query_group::RulesStorage;

#[salsa::database(RulesStorage)]
#[derive(Default)]
pub struct DatabaseStruct {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DatabaseStruct {}
