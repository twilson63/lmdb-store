use lmdb::Database;

pub struct DatabaseResource {
    pub db: Database,
    pub name: String,
}

impl DatabaseResource {
    pub fn new(db: Database, name: String) -> Self {
        DatabaseResource { db, name }
    }
}