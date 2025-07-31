use lmdb::{Environment, EnvironmentFlags, Transaction};
use parking_lot::Mutex;
use rustler::{Env as RustlerEnv, NifResult, Term, Encoder};
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
            .set_flags(EnvironmentFlags::WRITE_MAP | EnvironmentFlags::NO_SYNC)
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

pub fn env_open<'a>(env: RustlerEnv<'a>, path: Term<'a>, opts: Term<'a>) -> NifResult<Term<'a>> {
    use rustler::{Binary, Error, MapIterator, ResourceArc};
    use crate::atoms;
    
    // Parse path
    let path_str: String = if let Ok(bin) = path.decode::<Binary>() {
        std::str::from_utf8(bin.as_slice())
            .map_err(|_| Error::BadArg)?
            .to_string()
    } else {
        path.decode::<String>()?
    };
    
    // Parse options
    let mut map_size: usize = 10 * 1024 * 1024 * 1024; // 10GB default
    let mut max_dbs: u32 = 128;
    let mut max_readers: u32 = 126;
    
    if let Ok(iter) = opts.decode::<MapIterator>() {
        for (key, value) in iter {
            if let Ok(key_str) = key.decode::<&str>() {
                match key_str {
                    "map_size" => {
                        if let Ok(size) = value.decode::<i64>() {
                            map_size = size as usize;
                        }
                    }
                    "max_dbs" => {
                        if let Ok(dbs) = value.decode::<i64>() {
                            max_dbs = dbs as u32;
                        }
                    }
                    "max_readers" => {
                        if let Ok(readers) = value.decode::<i64>() {
                            max_readers = readers as u32;
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    
    // Create environment
    match LmdbEnvironment::new(Path::new(&path_str), map_size, max_dbs, max_readers) {
        Ok(lmdb_env) => {
            let resource = ResourceArc::new(EnvironmentResource(Arc::new(lmdb_env)));
            Ok((atoms::ok(), resource).encode(env))
        }
        Err(e) => {
            Ok((atoms::error(), e.to_string()).encode(env))
        }
    }
}

pub fn env_close<'a>(env: RustlerEnv<'a>, env_resource: Term<'a>) -> NifResult<Term<'a>> {
    use rustler::{Error, ResourceArc};
    use crate::atoms;
    
    // Decode the resource
    let _resource: ResourceArc<EnvironmentResource> = env_resource.decode()
        .map_err(|_| Error::BadArg)?;
    
    // The resource will be automatically dropped when it goes out of scope
    // This will decrease the reference count, and if it reaches 0,
    // the LMDB environment will be closed
    
    Ok(atoms::ok().encode(env))
}

pub fn env_get<'a>(env: RustlerEnv<'a>, env_resource: Term<'a>, db_name: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    use rustler::{Binary, Error, ResourceArc};
    use lmdb::Transaction;
    use crate::atoms;
    
    // Decode the environment resource
    let resource: ResourceArc<EnvironmentResource> = env_resource.decode()
        .map_err(|_| Error::BadArg)?;
    
    // Decode database name (None for default database)
    let db_name_opt: Option<String> = if db_name.decode::<rustler::Atom>().is_ok() && db_name.decode::<rustler::Atom>().unwrap() == atoms::nil() {
        None
    } else if let Ok(bin) = db_name.decode::<Binary>() {
        Some(std::str::from_utf8(bin.as_slice())
            .map_err(|_| Error::BadArg)?
            .to_string())
    } else if let Ok(s) = db_name.decode::<String>() {
        Some(s)
    } else {
        return Err(Error::BadArg);
    };
    
    // Decode key
    let key_bytes: Vec<u8> = if let Ok(bin) = key.decode::<Binary>() {
        bin.as_slice().to_vec()
    } else if let Ok(s) = key.decode::<String>() {
        s.into_bytes()
    } else {
        return Err(Error::BadArg);
    };
    
    // Get or create database
    let db = match resource.0.get_or_create_db(db_name_opt.as_deref()) {
        Ok(db) => db,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    // Begin read transaction
    let txn = match resource.0.env.begin_ro_txn() {
        Ok(t) => t,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    // Get value
    match txn.get(db, &key_bytes) {
        Ok(data) => {
            let mut binary = rustler::OwnedBinary::new(data.len()).unwrap();
            binary.as_mut_slice().copy_from_slice(data);
            Ok((atoms::ok(), binary.release(env)).encode(env))
        }
        Err(lmdb::Error::NotFound) => Ok(atoms::not_found().encode(env)),
        Err(e) => Ok((atoms::error(), e.to_string()).encode(env)),
    }
}

pub fn env_put<'a>(env: RustlerEnv<'a>, env_resource: Term<'a>, db_name: Term<'a>, key: Term<'a>, value: Term<'a>) -> NifResult<Term<'a>> {
    use rustler::{Binary, Error, ResourceArc};
    use lmdb::{Transaction, WriteFlags};
    use crate::atoms;
    
    // Decode the environment resource
    let resource: ResourceArc<EnvironmentResource> = env_resource.decode()
        .map_err(|_| Error::BadArg)?;
    
    // Decode database name (None for default database)
    let db_name_opt: Option<String> = if db_name.decode::<rustler::Atom>().is_ok() && db_name.decode::<rustler::Atom>().unwrap() == atoms::nil() {
        None
    } else if let Ok(bin) = db_name.decode::<Binary>() {
        Some(std::str::from_utf8(bin.as_slice())
            .map_err(|_| Error::BadArg)?
            .to_string())
    } else if let Ok(s) = db_name.decode::<String>() {
        Some(s)
    } else {
        return Err(Error::BadArg);
    };
    
    // Decode key
    let key_bytes: Vec<u8> = if let Ok(bin) = key.decode::<Binary>() {
        bin.as_slice().to_vec()
    } else if let Ok(s) = key.decode::<String>() {
        s.into_bytes()
    } else {
        return Err(Error::BadArg);
    };
    
    // Decode value
    let value_bytes: Vec<u8> = if let Ok(bin) = value.decode::<Binary>() {
        bin.as_slice().to_vec()
    } else if let Ok(s) = value.decode::<String>() {
        s.into_bytes()
    } else {
        return Err(Error::BadArg);
    };
    
    // Get or create database
    let db = match resource.0.get_or_create_db(db_name_opt.as_deref()) {
        Ok(db) => db,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    // Begin write transaction
    let mut txn = match resource.0.env.begin_rw_txn() {
        Ok(t) => t,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    // Put value
    match txn.put(db, &key_bytes, &value_bytes, WriteFlags::empty()) {
        Ok(_) => {
            match txn.commit() {
                Ok(_) => Ok(atoms::ok().encode(env)),
                Err(e) => Ok((atoms::error(), e.to_string()).encode(env)),
            }
        }
        Err(e) => Ok((atoms::error(), e.to_string()).encode(env)),
    }
}