use rustler::{Atom, Binary, Encoder, Env as RustlerEnv, Error, MapIterator, NifResult, Term};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;
use parking_lot::RwLock;
use once_cell::sync::Lazy;

use crate::atoms;
use crate::environment::{EnvironmentResource, LmdbEnvironment};
use crate::error::{StoreError, StoreResult};
use crate::path_ops::*;
use crate::transaction::TransactionType;
use crate::cursor::CursorType;

// Global registry of active environments
static ENVIRONMENTS: Lazy<RwLock<HashMap<String, Arc<LmdbEnvironment>>>> = 
    Lazy::new(|| RwLock::new(HashMap::new()));

// Helper to extract store options from Erlang term
fn parse_store_opts(term: Term) -> NifResult<HashMap<String, String>> {
    let mut opts = HashMap::new();
    
    let iter: MapIterator = term.decode()?;
    for (key, value) in iter {
        let key_str: String = match key.decode::<Binary>() {
            Ok(bin) => std::str::from_utf8(bin.as_slice())
                .map_err(|_| Error::BadArg)?
                .to_string(),
            Err(_) => key.decode::<String>()?,
        };
        
        let value_str: String = match value.decode::<Binary>() {
            Ok(bin) => std::str::from_utf8(bin.as_slice())
                .map_err(|_| Error::BadArg)?
                .to_string(),
            Err(_) => match value.decode::<String>() {
                Ok(s) => s,
                Err(_) => value.decode::<i64>()?.to_string(),
            },
        };
        
        opts.insert(key_str, value_str);
    }
    
    Ok(opts)
}

// Helper to decode path from Erlang term
fn decode_path(term: Term) -> NifResult<String> {
    if let Ok(bin) = term.decode::<Binary>() {
        Ok(std::str::from_utf8(bin.as_slice())
            .map_err(|_| Error::BadArg)?
            .to_string())
    } else if let Ok(s) = term.decode::<String>() {
        Ok(s)
    } else if let Ok(list) = term.decode::<Vec<Term>>() {
        let parts: Result<Vec<String>, _> = list.into_iter()
            .map(|t| {
                if let Ok(bin) = t.decode::<Binary>() {
                    std::str::from_utf8(bin.as_slice())
                        .map(|s| s.to_string())
                        .map_err(|_| Error::BadArg)
                } else {
                    t.decode::<String>()
                }
            })
            .collect();
        Ok(join_paths(&parts?))
    } else {
        Err(Error::BadArg)
    }
}

pub fn start<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>) -> NifResult<Term<'a>> {
    let opts = parse_store_opts(store_opts)?;
    
    let name = opts.get("name")
        .or_else(|| opts.get("store-module"))
        .ok_or(Error::BadArg)?
        .clone();
    
    let path = opts.get("path")
        .map(|p| PathBuf::from(p))
        .unwrap_or_else(|| PathBuf::from(format!("./lmdb/{}", name)));
    
    let map_size = opts.get("map_size")
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(10 * 1024 * 1024 * 1024); // 10GB default
    
    let max_dbs = opts.get("max_dbs")
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(128);
    
    let max_readers = opts.get("max_readers")
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(126);
    
    // Check if environment already exists
    {
        let envs = ENVIRONMENTS.read();
        if envs.contains_key(&name) {
            return Ok(atoms::ok().encode(env));
        }
    }
    
    // Create new environment
    match LmdbEnvironment::new(&path, map_size, max_dbs, max_readers) {
        Ok(lmdb_env) => {
            let arc_env = Arc::new(lmdb_env);
            ENVIRONMENTS.write().insert(name.clone(), arc_env.clone());
            
            // Return instance message with pid placeholder
            let instance = env.make_map_from_pairs(&[
                (Binary::from_slice(b"name".as_ref()), name),
                (Binary::from_slice(b"pid".as_ref()), env.pid()),
            ])?;
            
            Ok((atoms::ok(), instance).encode(env))
        }
        Err(e) => {
            Ok((atoms::error(), e.to_string()).encode(env))
        }
    }
}

pub fn stop<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>) -> NifResult<Term<'a>> {
    let opts = parse_store_opts(store_opts)?;
    let name = opts.get("name")
        .or_else(|| opts.get("store-module"))
        .ok_or(Error::BadArg)?;
    
    ENVIRONMENTS.write().remove(name);
    Ok(atoms::ok().encode(env))
}

pub fn reset<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>) -> NifResult<Term<'a>> {
    let opts = parse_store_opts(store_opts)?;
    let name = opts.get("name")
        .or_else(|| opts.get("store-module"))
        .ok_or(Error::BadArg)?;
    
    let envs = ENVIRONMENTS.read();
    if let Some(lmdb_env) = envs.get(name) {
        match lmdb_env.reset() {
            Ok(_) => Ok(atoms::ok().encode(env)),
            Err(e) => Ok((atoms::error(), e.to_string()).encode(env)),
        }
    } else {
        Ok(atoms::not_found().encode(env))
    }
}

pub fn read<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    let opts = parse_store_opts(store_opts)?;
    let name = opts.get("name")
        .or_else(|| opts.get("store-module"))
        .ok_or(Error::BadArg)?;
    
    let key_str = decode_path(key)?;
    
    let envs = ENVIRONMENTS.read();
    if let Some(lmdb_env) = envs.get(name) {
        let db = match lmdb_env.get_or_create_db(None) {
            Ok(db) => db,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        // Resolve links if necessary
        let resolved_key = resolve_path(lmdb_env, &key_str);
        
        let txn = match lmdb_env.env.begin_ro_txn() {
            Ok(t) => t,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        match txn.get(db, &resolved_key.as_bytes()) {
            Ok(data) => {
                let mut binary = rustler::OwnedBinary::new(data.len()).unwrap();
                binary.as_mut_slice().copy_from_slice(data);
                Ok((atoms::ok(), binary.release(env)).encode(env))
            }
            Err(lmdb::Error::NotFound) => Ok(atoms::not_found().encode(env)),
            Err(e) => Ok((atoms::error(), e.to_string()).encode(env)),
        }
    } else {
        Ok(atoms::not_found().encode(env))
    }
}

pub fn write<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, key: Term<'a>, value: Term<'a>) -> NifResult<Term<'a>> {
    let opts = parse_store_opts(store_opts)?;
    let name = opts.get("name")
        .or_else(|| opts.get("store-module"))
        .ok_or(Error::BadArg)?;
    
    let key_str = decode_path(key)?;
    let value_bin: Binary = value.decode()?;
    
    let envs = ENVIRONMENTS.read();
    if let Some(lmdb_env) = envs.get(name) {
        let db = match lmdb_env.get_or_create_db(None) {
            Ok(db) => db,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        let mut txn = match lmdb_env.env.begin_rw_txn() {
            Ok(t) => t,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        // Ensure parent groups exist
        ensure_parent_groups(lmdb_env, &mut txn, db, &key_str)?;
        
        match txn.put(db, &key_str.as_bytes(), &value_bin.as_slice(), lmdb::WriteFlags::empty()) {
            Ok(_) => {
                txn.commit().map_err(|e| Error::Term(Box::new(e.to_string())))?;
                Ok(atoms::ok().encode(env))
            }
            Err(e) => Ok((atoms::error(), e.to_string()).encode(env)),
        }
    } else {
        Ok(atoms::not_found().encode(env))
    }
}

pub fn get_type<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    let opts = parse_store_opts(store_opts)?;
    let name = opts.get("name")
        .or_else(|| opts.get("store-module"))
        .ok_or(Error::BadArg)?;
    
    let key_str = decode_path(key)?;
    
    let envs = ENVIRONMENTS.read();
    if let Some(lmdb_env) = envs.get(name) {
        let db = match lmdb_env.get_or_create_db(None) {
            Ok(db) => db,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        let txn = match lmdb_env.env.begin_ro_txn() {
            Ok(t) => t,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        // Check if it's a group
        let group_key = make_group_key(&key_str);
        if txn.get(db, &group_key.as_bytes()).is_ok() {
            return Ok((atoms::ok(), atoms::composite()).encode(env));
        }
        
        // Check if it's a link
        let link_key = make_link_key(&key_str);
        if txn.get(db, &link_key.as_bytes()).is_ok() {
            return Ok((atoms::ok(), atoms::link()).encode(env));
        }
        
        // Check if it's a simple value
        if txn.get(db, &key_str.as_bytes()).is_ok() {
            return Ok((atoms::ok(), atoms::simple()).encode(env));
        }
        
        Ok(atoms::not_found().encode(env))
    } else {
        Ok(atoms::not_found().encode(env))
    }
}

pub fn list<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, path: Term<'a>) -> NifResult<Term<'a>> {
    let opts = parse_store_opts(store_opts)?;
    let name = opts.get("name")
        .or_else(|| opts.get("store-module"))
        .ok_or(Error::BadArg)?;
    
    let path_str = decode_path(path)?;
    
    let envs = ENVIRONMENTS.read();
    if let Some(lmdb_env) = envs.get(name) {
        let db = match lmdb_env.get_or_create_db(None) {
            Ok(db) => db,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        let txn = match lmdb_env.env.begin_ro_txn() {
            Ok(t) => t,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        // Check if path is a group
        let group_key = make_group_key(&path_str);
        if txn.get(db, &group_key.as_bytes()).is_err() {
            return Ok(atoms::not_found().encode(env));
        }
        
        // List all children
        let prefix = if path_str.is_empty() {
            String::new()
        } else {
            format!("{}/", path_str)
        };
        
        let mut cursor = match txn.open_ro_cursor(db) {
            Ok(c) => c,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        let mut children = HashSet::new();
        
        // Position cursor at prefix
        if let Ok(_) = cursor.get(Some(prefix.as_bytes()), None, lmdb::Cursor::SET_RANGE) {
            loop {
                match cursor.get(None, None, lmdb::Cursor::CURRENT) {
                    Ok((key, _)) => {
                        let key_str = std::str::from_utf8(key).unwrap_or("");
                        
                        if !key_str.starts_with(&prefix) {
                            break;
                        }
                        
                        if let Some(child) = extract_child_name(key_str, &prefix) {
                            // Skip internal markers
                            if !child.ends_with(GROUP_MARKER) && !child.ends_with(LINK_MARKER) {
                                children.insert(child);
                            }
                        }
                        
                        if cursor.get(None, None, lmdb::Cursor::NEXT).is_err() {
                            break;
                        }
                    }
                    Err(_) => break,
                }
            }
        }
        
        let children_vec: Vec<String> = children.into_iter().collect();
        Ok((atoms::ok(), children_vec).encode(env))
    } else {
        Ok(atoms::not_found().encode(env))
    }
}

pub fn make_group<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, path: Term<'a>) -> NifResult<Term<'a>> {
    let opts = parse_store_opts(store_opts)?;
    let name = opts.get("name")
        .or_else(|| opts.get("store-module"))
        .ok_or(Error::BadArg)?;
    
    let path_str = decode_path(path)?;
    
    let envs = ENVIRONMENTS.read();
    if let Some(lmdb_env) = envs.get(name) {
        let db = match lmdb_env.get_or_create_db(None) {
            Ok(db) => db,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        let mut txn = match lmdb_env.env.begin_rw_txn() {
            Ok(t) => t,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        // Ensure parent groups exist
        ensure_parent_groups(lmdb_env, &mut txn, db, &path_str)?;
        
        // Create group marker
        let group_key = make_group_key(&path_str);
        match txn.put(db, &group_key.as_bytes(), b"1", lmdb::WriteFlags::empty()) {
            Ok(_) => {
                txn.commit().map_err(|e| Error::Term(Box::new(e.to_string())))?;
                Ok(atoms::ok().encode(env))
            }
            Err(e) => Ok((atoms::error(), e.to_string()).encode(env)),
        }
    } else {
        Ok(atoms::not_found().encode(env))
    }
}

pub fn make_link<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, existing: Term<'a>, new: Term<'a>) -> NifResult<Term<'a>> {
    let opts = parse_store_opts(store_opts)?;
    let name = opts.get("name")
        .or_else(|| opts.get("store-module"))
        .ok_or(Error::BadArg)?;
    
    let existing_str = decode_path(existing)?;
    let new_str = decode_path(new)?;
    
    let envs = ENVIRONMENTS.read();
    if let Some(lmdb_env) = envs.get(name) {
        let db = match lmdb_env.get_or_create_db(None) {
            Ok(db) => db,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        let mut txn = match lmdb_env.env.begin_rw_txn() {
            Ok(t) => t,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        // Store link target
        let link_key = make_link_key(&new_str);
        match txn.put(db, &link_key.as_bytes(), &existing_str.as_bytes(), lmdb::WriteFlags::empty()) {
            Ok(_) => {
                txn.commit().map_err(|e| Error::Term(Box::new(e.to_string())))?;
                Ok(atoms::ok().encode(env))
            }
            Err(e) => Ok((atoms::error(), e.to_string()).encode(env)),
        }
    } else {
        Ok(atoms::not_found().encode(env))
    }
}

pub fn path<'a>(env: RustlerEnv<'a>, _store_opts: Term<'a>, path: Term<'a>) -> NifResult<Term<'a>> {
    let path_str = decode_path(path)?;
    Ok(path_str.encode(env))
}

pub fn add_path<'a>(env: RustlerEnv<'a>, _store_opts: Term<'a>, path1: Term<'a>, path2: Term<'a>) -> NifResult<Term<'a>> {
    let path1_str = decode_path(path1)?;
    let path2_str = decode_path(path2)?;
    
    let combined = if path1_str.is_empty() {
        path2_str
    } else if path2_str.is_empty() {
        path1_str
    } else {
        format!("{}/{}", path1_str, path2_str)
    };
    
    Ok(combined.encode(env))
}

// Helper functions

fn resolve_path(lmdb_env: &Arc<LmdbEnvironment>, path: &str) -> String {
    let db = match lmdb_env.get_or_create_db(None) {
        Ok(db) => db,
        Err(_) => return path.to_string(),
    };
    
    let txn = match lmdb_env.env.begin_ro_txn() {
        Ok(t) => t,
        Err(_) => return path.to_string(),
    };
    
    let mut current_path = path.to_string();
    let mut visited = HashSet::new();
    
    loop {
        if visited.contains(&current_path) {
            // Circular reference
            break;
        }
        visited.insert(current_path.clone());
        
        let link_key = make_link_key(&current_path);
        match txn.get(db, &link_key.as_bytes()) {
            Ok(target) => {
                if let Ok(target_str) = std::str::from_utf8(target) {
                    current_path = target_str.to_string();
                } else {
                    break;
                }
            }
            Err(_) => break,
        }
    }
    
    current_path
}

fn ensure_parent_groups(
    lmdb_env: &Arc<LmdbEnvironment>,
    txn: &mut lmdb::RwTransaction,
    db: lmdb::Database,
    path: &str
) -> NifResult<()> {
    let parts = split_path(path);
    let mut current_path = String::new();
    
    for i in 0..parts.len().saturating_sub(1) {
        if i > 0 {
            current_path.push('/');
        }
        current_path.push_str(&parts[i]);
        
        let group_key = make_group_key(&current_path);
        if txn.get(db, &group_key.as_bytes()).is_err() {
            txn.put(db, &group_key.as_bytes(), b"1", lmdb::WriteFlags::empty())
                .map_err(|e| Error::Term(Box::new(e.to_string())))?;
        }
    }
    
    Ok(())
}