use rustler::{Binary, Encoder, Env as RustlerEnv, Error, MapIterator, NifResult, Term, ResourceArc};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;
use parking_lot::RwLock;
use once_cell::sync::Lazy;
use lmdb::{Transaction, Cursor, WriteFlags};

use crate::atoms;
use crate::environment::{LmdbEnvironment, EnvironmentResource};
use crate::path_ops::*;

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
                Err(_) => match value.decode::<i64>() {
                    Ok(i) => i.to_string(),
                    Err(_) => match value.decode::<u64>() {
                        Ok(u) => u.to_string(),
                        Err(_) => match value.decode::<rustler::Atom>() {
                            Ok(atom) => {
                                // Convert atom to string by getting its text representation
                                // For now, we'll just use the atom name which works for module names
                                format!("{:?}", atom)
                            },
                            Err(_) => return Err(Error::BadArg),
                        },
                    },
                },
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
    
    // The name IS the database path in HyperBEAM convention
    let name = opts.get("name")
        .or_else(|| opts.get("store-module"))
        .ok_or(Error::BadArg)?
        .clone();
    
    // Use name as the path directly
    let path = PathBuf::from(&name);
    
    // Use capacity if provided, otherwise use map_size, otherwise use default
    let map_size = opts.get("capacity")
        .or_else(|| opts.get("map_size"))
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
        if let Some(existing_env) = envs.get(&name) {
            let resource = ResourceArc::new(EnvironmentResource(existing_env.clone()));
            return Ok((atoms::ok(), resource).encode(env));
        }
    }
    
    // Create new environment
    match LmdbEnvironment::new(&path, map_size, max_dbs, max_readers) {
        Ok(lmdb_env) => {
            let arc_env = Arc::new(lmdb_env);
            ENVIRONMENTS.write().insert(name.clone(), arc_env.clone());
            
            // Return the environment resource
            let resource = ResourceArc::new(EnvironmentResource(arc_env));
            Ok((atoms::ok(), resource).encode(env))
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
    let key_str = decode_path(key)?;
    
    // Try to decode store_opts as an environment resource first
    let lmdb_env = if let Ok(resource) = store_opts.decode::<ResourceArc<EnvironmentResource>>() {
        resource.0.clone()
    } else {
        // Fall back to parsing store_opts and looking up by name
        let opts = parse_store_opts(store_opts)?;
        let name = opts.get("name")
            .or_else(|| opts.get("store-module"))
            .ok_or(Error::BadArg)?;
        
        let envs = ENVIRONMENTS.read();
        envs.get(name).cloned().ok_or(Error::BadArg)?
    };
    
    let db = match lmdb_env.get_or_create_db(None) {
        Ok(db) => db,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    // Resolve links if necessary
    let resolved_key = resolve_path(&lmdb_env, &key_str);
    
    let txn = match lmdb_env.env.begin_ro_txn() {
        Ok(t) => t,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    match txn.get(db, &resolved_key.as_bytes()) {
        Ok(data) => {
            // Check the prefix to determine data type
            if let Ok(data_str) = std::str::from_utf8(data) {
                if data_str.starts_with("@link:") {
                    // This is a link that couldn't be resolved
                    return Ok(atoms::not_found().encode(env));
                } else if data_str.starts_with("@data:") {
                    // Regular data - strip the prefix
                    let actual_data = &data[6..]; // Skip "@data:"
                    let mut binary = rustler::OwnedBinary::new(actual_data.len()).unwrap();
                    binary.as_mut_slice().copy_from_slice(actual_data);
                    return Ok((atoms::ok(), binary.release(env)).encode(env));
                }
            }
            // Legacy data without prefix (for backwards compatibility)
            let mut binary = rustler::OwnedBinary::new(data.len()).unwrap();
            binary.as_mut_slice().copy_from_slice(data);
            Ok((atoms::ok(), binary.release(env)).encode(env))
        }
        Err(lmdb::Error::NotFound) => Ok(atoms::not_found().encode(env)),
        Err(e) => Ok((atoms::error(), e.to_string()).encode(env)),
    }
}

pub fn write<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, key: Term<'a>, value: Term<'a>) -> NifResult<Term<'a>> {
    let key_str = decode_path(key)?;
    let value_bin: Binary = value.decode()?;
    
    // Try to decode store_opts as an environment resource first
    let lmdb_env = if let Ok(resource) = store_opts.decode::<ResourceArc<EnvironmentResource>>() {
        resource.0.clone()
    } else {
        // Fall back to parsing store_opts and looking up by name
        let opts = parse_store_opts(store_opts)?;
        let name = opts.get("name")
            .or_else(|| opts.get("store-module"))
            .ok_or(Error::BadArg)?;
        
        let envs = ENVIRONMENTS.read();
        envs.get(name).cloned().ok_or(Error::BadArg)?
    };
    
    let db = match lmdb_env.get_or_create_db(None) {
        Ok(db) => db,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    let mut txn = match lmdb_env.env.begin_rw_txn() {
        Ok(t) => t,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    // Ensure parent groups exist
    ensure_parent_groups(&lmdb_env, &mut txn, db, &key_str)?;
    
    // Prefix the value with @data: to distinguish from links
    let prefixed_value = [b"@data:", value_bin.as_slice()].concat();
    
    match txn.put(db, &key_str.as_bytes(), &prefixed_value, WriteFlags::empty()) {
        Ok(_) => {
            txn.commit().map_err(|e| Error::Term(Box::new(e.to_string())))?;
            Ok(atoms::ok().encode(env))
        }
        Err(e) => Ok((atoms::error(), e.to_string()).encode(env)),
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
        
        // Check if it's a value (link or simple)
        if let Ok(value) = txn.get(db, &key_str.as_bytes()) {
            if let Ok(value_str) = std::str::from_utf8(value) {
                if value_str.starts_with("@link:") {
                    return Ok((atoms::ok(), atoms::link()).encode(env));
                } else if value_str.starts_with("@data:") {
                    return Ok((atoms::ok(), atoms::simple()).encode(env));
                }
            }
            // Legacy data without prefix
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
        
        // Iterate through keys with prefix
        for (key, _) in cursor.iter_from(prefix.as_bytes()) {
            let key_str = std::str::from_utf8(key).unwrap_or("");
            
            if !prefix.is_empty() && !key_str.starts_with(&prefix) {
                break;
            }
            
            if let Some(child) = extract_child_name(key_str, &prefix) {
                // Skip internal markers
                if !child.ends_with(GROUP_MARKER) {
                    children.insert(child);
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
        match txn.put(db, &group_key.as_bytes(), b"1", WriteFlags::empty()) {
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
        
        // Store link target as the value of the new key
        // Prefix with special marker to identify it as a link
        let link_value = format!("@link:{}", existing_str);
        match txn.put(db, &new_str.as_bytes(), &link_value.as_bytes(), WriteFlags::empty()) {
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

pub fn list_prefix<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, prefix: Term<'a>, opts: Term<'a>) -> NifResult<Term<'a>> {
    let prefix_str = decode_path(prefix)?;
    
    // Try to decode store_opts as an environment resource first
    let lmdb_env = if let Ok(resource) = store_opts.decode::<ResourceArc<EnvironmentResource>>() {
        resource.0.clone()
    } else {
        // Fall back to parsing store_opts and looking up by name
        let opts_map = parse_store_opts(store_opts)?;
        let name = opts_map.get("name")
            .or_else(|| opts_map.get("store-module"))
            .ok_or(Error::BadArg)?;
        
        let envs = ENVIRONMENTS.read();
        envs.get(name).cloned().ok_or(Error::BadArg)?
    };
    
    // Parse options
    let mut limit: Option<usize> = None;
    let mut start_after: Option<String> = None;
    let mut return_cursor = false;
    
    if let Ok(opts_iter) = opts.decode::<MapIterator>() {
        for (key, value) in opts_iter {
            let key_str = if let Ok(s) = key.decode::<String>() {
                s
            } else if let Ok(bin) = key.decode::<Binary>() {
                std::str::from_utf8(bin.as_slice())
                    .map_err(|_| Error::BadArg)?
                    .to_string()
            } else if let Ok(atom) = key.decode::<rustler::Atom>() {
                format!("{:?}", atom)
            } else {
                continue;
            };
            
            match key_str.as_str() {
                "limit" => {
                    if let Ok(l) = value.decode::<i64>() {
                        limit = Some(l as usize);
                    }
                }
                "cursor" => {
                    if let Ok(cursor) = value.decode::<String>() {
                        start_after = Some(cursor);
                    } else if let Ok(bin) = value.decode::<Binary>() {
                        start_after = Some(std::str::from_utf8(bin.as_slice())
                            .map_err(|_| Error::BadArg)?
                            .to_string());
                    }
                }
                "return_cursor" => {
                    if let Ok(b) = value.decode::<bool>() {
                        return_cursor = b;
                    } else if value.decode::<rustler::Atom>().is_ok() {
                        return_cursor = true;
                    }
                }
                _ => {}
            }
        }
    }
    
    let db = match lmdb_env.get_or_create_db(None) {
        Ok(db) => db,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    let txn = match lmdb_env.env.begin_ro_txn() {
        Ok(t) => t,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    let mut cursor = match txn.open_ro_cursor(db) {
        Ok(c) => c,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    let mut keys = Vec::new();
    let mut last_key = None;
    
    // Determine where to start iterating
    let start_key = if let Some(ref after) = start_after {
        // Start after the cursor position
        after.as_bytes()
    } else if prefix_str.is_empty() {
        // Empty prefix means list all
        b""
    } else {
        // Start at the prefix
        prefix_str.as_bytes()
    };
    
    // Start iteration from the beginning to avoid panics
    for (key, _) in cursor.iter_start() {
        let key_str = match std::str::from_utf8(key) {
            Ok(s) => s,
            Err(_) => continue,
        };
        
        // Skip keys before our start position
        if !start_key.is_empty() && key < start_key {
            continue;
        }
        
        // Skip the cursor key itself if provided
        if let Some(ref after) = start_after {
            if key_str == after {
                continue;
            }
        }
        
        // Check if key starts with prefix
        if !prefix_str.is_empty() && !key_str.starts_with(&prefix_str) {
            break; // We've passed all keys with this prefix
        }
        
        // Skip internal markers (groups and links)
        if key_str.ends_with(GROUP_MARKER) {
            continue;
        }
        
        keys.push(key_str.to_string());
        last_key = Some(key_str.to_string());
        
        // Check limit
        if let Some(l) = limit {
            if keys.len() >= l {
                break;
            }
        }
    }
    
    // Convert keys to Erlang terms
    let keys_term: Vec<Term> = keys.iter()
        .map(|k| k.encode(env))
        .collect();
    
    if return_cursor {
        if let Some(last) = last_key {
            Ok((atoms::ok(), keys_term, last).encode(env))
        } else {
            Ok((atoms::ok(), keys_term, atoms::nil()).encode(env))
        }
    } else {
        Ok((atoms::ok(), keys_term).encode(env))
    }
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
            // Circular reference detected
            break;
        }
        visited.insert(current_path.clone());
        
        // Check if the current path has a value
        match txn.get(db, &current_path.as_bytes()) {
            Ok(value) => {
                if let Ok(value_str) = std::str::from_utf8(value) {
                    // Check if this is a link
                    if value_str.starts_with("@link:") {
                        // Extract the target path
                        current_path = value_str[6..].to_string();
                        // Continue following the link
                        continue;
                    }
                }
                // Not a link or can't parse - this is the final value
                break;
            }
            Err(_) => {
                // Key doesn't exist
                break;
            }
        }
    }
    
    current_path
}

fn ensure_parent_groups(
    _lmdb_env: &Arc<LmdbEnvironment>,
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
            txn.put(db, &group_key.as_bytes(), b"1", WriteFlags::empty())
                .map_err(|e| Error::Term(Box::new(e.to_string())))?;
        }
    }
    
    Ok(())
}