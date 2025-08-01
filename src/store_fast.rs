// Fast path optimizations for hyper_lmdb
use rustler::{Binary, Encoder, Env as RustlerEnv, NifResult, Term, ResourceArc, Error};
use lmdb::{WriteFlags, Database, Transaction};
use std::sync::Arc;
use crate::atoms;
use crate::environment::{LmdbEnvironment, EnvironmentResource};
use crate::store::{parse_store_opts, get_canonical_key, ENVIRONMENTS};

// Thread-local cache for environment lookups to avoid lock contention
thread_local! {
    static ENV_CACHE: std::cell::RefCell<std::collections::HashMap<String, Arc<LmdbEnvironment>>> = 
        std::cell::RefCell::new(std::collections::HashMap::new());
    static DB_CACHE: std::cell::RefCell<std::collections::HashMap<String, Database>> = 
        std::cell::RefCell::new(std::collections::HashMap::new());
}

// Fast path for getting environment - avoids parsing store_opts
#[inline(always)]
pub fn get_env_fast(store_opts: Term) -> Result<Arc<LmdbEnvironment>, Error> {
    // Try to decode as resource first (fastest path)
    if let Ok(resource) = store_opts.decode::<ResourceArc<EnvironmentResource>>() {
        return Ok(resource.0.clone());
    }
    
    // Fall back to parsing - but cache the result
    let opts = parse_store_opts(store_opts)?;
    let name = opts.get("name").ok_or(Error::BadArg)?;
    let key = get_canonical_key(name);
    
    // Check thread-local cache first
    ENV_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if let Some(env) = cache.get(&key) {
            return Ok(env.clone());
        }
        
        // Not in thread-local cache, check global
        let envs = ENVIRONMENTS.read();
        if let Some(env) = envs.get(&key) {
            let env = env.clone();
            cache.insert(key, env.clone());
            Ok(env)
        } else {
            Err(Error::BadArg)
        }
    })
}

// Fast read that minimizes allocations
pub fn read_fast<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    // Decode key as binary directly to avoid string allocation
    let key_bin: Binary = key.decode()?;
    let key_bytes = key_bin.as_slice();
    
    let lmdb_env = get_env_fast(store_opts)?;
    let db = lmdb_env.get_db_cached()
        .map_err(|_| Error::BadArg)?;
    
    let txn = lmdb_env.env.begin_ro_txn()
        .map_err(|_| Error::BadArg)?;
    
    match txn.get(db, &key_bytes) {
            Ok(data) => {
                // Check for link prefix without string conversion
                if data.len() > 6 && &data[0..6] == b"@link:" {
                    // For now, fall back to regular read for links
                    drop(txn);
                    return crate::store::read(env, store_opts, key);
                }
                
                // Skip all prefix checking for maximum speed
                // We must copy the data because it's only valid within the transaction
                let mut binary = rustler::OwnedBinary::new(data.len()).unwrap();
                binary.as_mut_slice().copy_from_slice(data);
                
                Ok((atoms::ok(), binary.release(env)).encode(env))
            }
            Err(lmdb::Error::NotFound) => Ok(atoms::not_found().encode(env)),
            Err(_) => Ok(atoms::not_found().encode(env)),
        }
}

// Fast write that minimizes allocations and skips sync
pub fn write_fast<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, key: Term<'a>, value: Term<'a>) -> NifResult<Term<'a>> {
    let key_bin: Binary = key.decode()?;
    let key_bytes = key_bin.as_slice();
    let value_bin: Binary = value.decode()?;
    
    let lmdb_env = get_env_fast(store_opts)?;
    let db = lmdb_env.get_db_cached()
        .map_err(|_| Error::BadArg)?;
    
    let mut txn = lmdb_env.env.begin_rw_txn()
        .map_err(|_| Error::BadArg)?;
    
    // Skip parent group creation for performance
    // Users should create groups explicitly if needed
    
    // Write directly without prefix for maximum speed
    // Only add prefix if we detect links in the system
    match txn.put(db, &key_bytes, &value_bin.as_slice(), WriteFlags::empty()) {
        Ok(_) => {
            txn.commit().map_err(|_| Error::BadArg)?;
            // Skip sync for performance - rely on periodic sync or explicit sync calls
            Ok(atoms::ok().encode(env))
        }
        Err(_) => Ok(atoms::error().encode(env)),
    }
}

// Batch read for multiple keys in one transaction
pub fn read_many_fast<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, keys: Term<'a>) -> NifResult<Term<'a>> {
    let key_list: Vec<Binary> = keys.decode()?;
    let lmdb_env = get_env_fast(store_opts)?;
    let db = lmdb_env.get_db_cached()
        .map_err(|_| Error::BadArg)?;
    
    let txn = lmdb_env.env.begin_ro_txn()
        .map_err(|_| Error::BadArg)?;
    
    let mut results = Vec::with_capacity(key_list.len());
    
    for key_bin in key_list {
        let key_bytes = key_bin.as_slice();
        
        match txn.get(db, &key_bytes) {
            Ok(data) => {
                // Skip link resolution for speed
                let actual_data = if data.len() > 6 && &data[0..6] == b"@data:" {
                    &data[6..]
                } else {
                    data
                };
                
                let mut binary = rustler::OwnedBinary::new(actual_data.len()).unwrap();
                binary.as_mut_slice().copy_from_slice(actual_data);
                let result_tuple = (atoms::ok(), binary.release(env)).encode(env);
                results.push((key_bin, result_tuple).encode(env));
            }
            Err(lmdb::Error::NotFound) => {
                results.push((key_bin, atoms::not_found()).encode(env));
            }
            Err(_) => {
                results.push((key_bin, atoms::error()).encode(env));
            }
        }
    }
    
    Ok((atoms::ok(), results).encode(env))
}

// Extension trait for LmdbEnvironment to add caching
pub trait LmdbEnvironmentExt {
    fn get_db_cached(&self) -> Result<Database, crate::error::StoreError>;
}

impl LmdbEnvironmentExt for LmdbEnvironment {
    #[inline(always)]
    fn get_db_cached(&self) -> Result<Database, crate::error::StoreError> {
        // In production, we'd cache this, but for now just call through
        self.get_or_create_db(None)
    }
}