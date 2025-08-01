use lmdb::{Environment, EnvironmentFlags, Transaction};
use parking_lot::Mutex;
use rustler::Env as RustlerEnv;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use crate::error::StoreResult;

pub struct LmdbEnvironment {
    pub env: Arc<Environment>,
    pub path: PathBuf,
    pub databases: Arc<Mutex<HashMap<String, lmdb::Database>>>,
}

impl LmdbEnvironment {
    pub fn new(path: &Path, map_size: usize, max_dbs: u32, max_readers: u32) -> StoreResult<Self> {
        std::fs::create_dir_all(path)?;
        
        let env = Environment::new()
            .set_flags(EnvironmentFlags::WRITE_MAP | EnvironmentFlags::MAP_ASYNC)
            .set_map_size(map_size)
            .set_max_dbs(max_dbs)
            .set_max_readers(max_readers)
            .open(path)?;
        
        Ok(LmdbEnvironment {
            env: Arc::new(env),
            path: path.to_path_buf(),
            databases: Arc::new(Mutex::new(HashMap::new())),
        })
    }
    
    pub fn get_or_create_db(&self, name: Option<&str>) -> StoreResult<lmdb::Database> {
        let mut dbs = self.databases.lock();
        let db_name = name.unwrap_or("__default__");
        
        if let Some(db) = dbs.get(db_name) {
            return Ok(*db);
        }
        
        let db = self.env.create_db(name, lmdb::DatabaseFlags::empty())?;
        dbs.insert(db_name.to_string(), db);
        Ok(db)
    }
    
    pub fn reset(&self) -> StoreResult<()> {
        let dbs = self.databases.lock();
        
        // Clear all databases
        for (_name, db) in dbs.iter() {
            let mut txn = self.env.begin_rw_txn()?;
            txn.clear_db(*db)?;
            txn.commit()?;
        }
        
        Ok(())
    }
}

// Rustler resource type
pub struct EnvironmentResource(pub Arc<LmdbEnvironment>);

pub fn on_load(env: RustlerEnv) -> bool {
    rustler::resource!(EnvironmentResource, env);
    true
}

// Direct environment API functions removed - use the store API instead
// The store module provides start/stop/read/write which handle all LMDB operations