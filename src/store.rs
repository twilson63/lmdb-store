use rustler::{Binary, Encoder, Env as RustlerEnv, Error, MapIterator, NifResult, Term, ResourceArc};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;
use parking_lot::RwLock;
use once_cell::sync::Lazy;
use lmdb::{Transaction, Cursor, WriteFlags, EnvironmentFlags};

use crate::atoms;
use crate::environment::{LmdbEnvironment, EnvironmentResource};
use crate::path_ops::*;
use crate::cache::{invalidate_path_cache, invalidate_link_cache, PATH_CACHE};

// Global registry of active environments
pub(crate) static ENVIRONMENTS: Lazy<RwLock<HashMap<String, Arc<LmdbEnvironment>>>> = 
    Lazy::new(|| RwLock::new(HashMap::new()));

// Helper function to get a canonical key for environment lookup
pub(crate) fn get_canonical_key(name: &str) -> String {
    let path = PathBuf::from(name);
    
    // First, make the path absolute if it's not already
    let abs_path = if path.is_absolute() {
        path
    } else {
        match std::env::current_dir() {
            Ok(cwd) => cwd.join(&path),
            Err(_) => path,
        }
    };
    
    // Normalize the path by removing . and .. components
    // This gives us a consistent key regardless of whether the path exists
    use std::path::Component;
    let mut normalized = PathBuf::new();
    for component in abs_path.components() {
        match component {
            Component::CurDir => {} // Skip "."
            Component::ParentDir => {
                // Remove the last component for ".."
                normalized.pop();
            }
            component => normalized.push(component),
        }
    }
    
    normalized.to_string_lossy().to_string()
}

// Helper to extract store options from Erlang term
pub(crate) fn parse_store_opts(term: Term) -> NifResult<HashMap<String, String>> {
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
                Err(_) => match value.decode::<bool>() {
                    Ok(b) => b.to_string(),
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
            },
        };
        
        opts.insert(key_str, value_str);
    }
    
    Ok(opts)
}

// Helper to parse LMDB flags from store options
pub(crate) fn parse_lmdb_flags(opts: &HashMap<String, String>) -> EnvironmentFlags {
    let mut flags = EnvironmentFlags::empty();
    
    // Always use WRITE_MAP for performance
    flags |= EnvironmentFlags::WRITE_MAP;
    
    // Parse individual flags from options
    if opts.get("no_sync").map(|v| v == "true" || v == "1").unwrap_or(true) {
        flags |= EnvironmentFlags::NO_SYNC;
    }
    
    if opts.get("map_async").map(|v| v == "true" || v == "1").unwrap_or(true) {
        flags |= EnvironmentFlags::MAP_ASYNC;
    }
    
    if opts.get("no_meta_sync").map(|v| v == "true" || v == "1").unwrap_or(true) {
        flags |= EnvironmentFlags::NO_META_SYNC;
    }
    
    if opts.get("no_readahead").map(|v| v == "true" || v == "1").unwrap_or(true) {
        flags |= EnvironmentFlags::NO_READAHEAD;
    }
    
    if opts.get("no_tls").map(|v| v == "true" || v == "1").unwrap_or(false) {
        flags |= EnvironmentFlags::NO_TLS;
    }
    
    if opts.get("no_lock").map(|v| v == "true" || v == "1").unwrap_or(false) {
        flags |= EnvironmentFlags::NO_LOCK;
    }
    
    if opts.get("read_only").map(|v| v == "true" || v == "1").unwrap_or(false) {
        flags |= EnvironmentFlags::READ_ONLY;
    }
    
    flags
}

// Helper to decode path from Erlang term
pub(crate) fn decode_path(term: Term) -> NifResult<String> {
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
    
    // Get the canonical key for environment lookup
    let key = get_canonical_key(&name);
    
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
    
    // Check if environment already exists using the canonical key
    {
        let envs = ENVIRONMENTS.read();
        if let Some(existing_env) = envs.get(&key) {
            // Environment already exists, return it
            let resource = ResourceArc::new(EnvironmentResource(existing_env.clone()));
            return Ok((atoms::ok(), resource).encode(env));
        }
    }
    
    // Parse LMDB flags from options
    let flags = parse_lmdb_flags(&opts);
    
    // Create new environment with custom flags
    match LmdbEnvironment::new_with_flags(&path, map_size, max_dbs, max_readers, Some(flags)) {
        Ok(lmdb_env) => {
            let arc_env = Arc::new(lmdb_env);
            
            // Insert with the canonical key to ensure idempotency
            ENVIRONMENTS.write().insert(key, arc_env.clone());
            
            // Return the environment resource
            let resource = ResourceArc::new(EnvironmentResource(arc_env));
            Ok((atoms::ok(), resource).encode(env))
        }
        Err(e) => {
            // Check if error is about the environment already being open
            let error_str = e.to_string();
            if error_str.contains("Environment already open") || 
               error_str.contains("Device or resource busy") {
                // This might happen in a race condition, try to get it from the map again
                let envs = ENVIRONMENTS.read();
                if let Some(existing_env) = envs.get(&key) {
                    let resource = ResourceArc::new(EnvironmentResource(existing_env.clone()));
                    return Ok((atoms::ok(), resource).encode(env));
                }
            }
            Ok((atoms::error(), format!("store_start_failed: hyper_lmdb, {}, LMDB error: {}", name, error_str)).encode(env))
        }
    }
}

pub fn stop<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>) -> NifResult<Term<'a>> {
    let opts = parse_store_opts(store_opts)?;
    let name = opts.get("name")
        .or_else(|| opts.get("store-module"))
        .ok_or(Error::BadArg)?;
    
    // Get the canonical key for environment lookup
    let key = get_canonical_key(name);
    
    ENVIRONMENTS.write().remove(&key);
    Ok(atoms::ok().encode(env))
}

pub fn reset<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>) -> NifResult<Term<'a>> {
    let opts = parse_store_opts(store_opts)?;
    let name = opts.get("name")
        .or_else(|| opts.get("store-module"))
        .ok_or(Error::BadArg)?;
    
    // Get the canonical key for environment lookup
    let key = get_canonical_key(name);
    
    let envs = ENVIRONMENTS.read();
    if let Some(lmdb_env) = envs.get(&key) {
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
            .ok_or(Error::BadArg)?;
        
        // Get the canonical key for environment lookup
        let key = get_canonical_key(name);
        
        let envs = ENVIRONMENTS.read();
        envs.get(&key).cloned().ok_or(Error::BadArg)?
    };
    
    // For now, disable caching for link resolution to fix the bug
    // TODO: Implement proper cache invalidation that tracks link dependencies
    let resolved_key = resolve_path(&lmdb_env, &key_str);
    
    
    let db = match lmdb_env.get_or_create_db(None) {
        Ok(db) => db,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    let txn = match lmdb_env.env.begin_ro_txn() {
        Ok(t) => t,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    // Follow links if we find them
    let mut current_key = resolved_key;
    let mut link_depth = 0;
    const MAX_LINK_DEPTH: usize = 10;
    
    loop {
        match txn.get(db, &current_key.as_bytes()) {
            Ok(data) => {
                // Check for prefixes by looking at the raw bytes
                if data.starts_with(b"@link:") {
                    // This is a link - follow it
                    if link_depth >= MAX_LINK_DEPTH {
                        // Too many links, possible circular reference
                        return Ok(atoms::not_found().encode(env));
                    }
                    link_depth += 1;
                    
                    // Extract the link target
                    if let Ok(link_str) = std::str::from_utf8(&data[6..]) {
                        // Resolve any path components in the link target
                        current_key = resolve_path(&lmdb_env, link_str);
                        continue; // Try to read the link target
                    } else {
                        return Ok(atoms::not_found().encode(env));
                    }
                } else if data.starts_with(b"@data:") {
                    // Regular data - strip the prefix
                    let actual_data = &data[6..];
                    let mut binary = rustler::OwnedBinary::new(actual_data.len()).unwrap();
                    binary.as_mut_slice().copy_from_slice(actual_data);
                    return Ok((atoms::ok(), binary.release(env)).encode(env));
                } else {
                    // Legacy data without prefix
                    let mut binary = rustler::OwnedBinary::new(data.len()).unwrap();
                    binary.as_mut_slice().copy_from_slice(data);
                    return Ok((atoms::ok(), binary.release(env)).encode(env));
                }
            }
            Err(lmdb::Error::NotFound) => return Ok(atoms::not_found().encode(env)),
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        }
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
            .ok_or(Error::BadArg)?;
        
        // Get the canonical key for environment lookup
        let key = get_canonical_key(name);
        
        let envs = ENVIRONMENTS.read();
        envs.get(&key).cloned().ok_or(Error::BadArg)?
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
            // NO_SYNC flag is set, so we don't force sync on every write
            // Users can call sync() explicitly when needed
            
            // Invalidate cache for this path
            invalidate_path_cache(&key_str);
            
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
    
    // Get the canonical key for environment lookup
    let lookup_key = get_canonical_key(name);
    
    let envs = ENVIRONMENTS.read();
    if let Some(lmdb_env) = envs.get(&lookup_key) {
        let db = match lmdb_env.get_or_create_db(None) {
            Ok(db) => db,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        let txn = match lmdb_env.env.begin_ro_txn() {
            Ok(t) => t,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        // First check if the key itself exists and follow links
        let mut current_key = key_str.clone();
        let mut link_depth = 0;
        const MAX_LINK_DEPTH: usize = 10;
        
        loop {
            match txn.get(db, &current_key.as_bytes()) {
                Ok(data) => {
                    if data.starts_with(b"@link:") {
                        // This is a link - follow it
                        if link_depth >= MAX_LINK_DEPTH {
                            // Too many links, possible circular reference
                            return Ok(atoms::not_found().encode(env));
                        }
                        link_depth += 1;
                        
                        // Extract the link target
                        if let Ok(link_str) = std::str::from_utf8(&data[6..]) {
                            // Resolve any path components in the link target
                            current_key = resolve_path(&lmdb_env, link_str);
                            continue; // Check the link target
                        } else {
                            return Ok(atoms::not_found().encode(env));
                        }
                    } else {
                        // Found a value (with or without @data: prefix)
                        // Now check if it's a group
                        let group_key = make_group_key(&current_key);
                        if txn.get(db, &group_key.as_bytes()).is_ok() {
                            return Ok((atoms::ok(), atoms::composite()).encode(env));
                        } else {
                            return Ok((atoms::ok(), atoms::simple()).encode(env));
                        }
                    }
                }
                Err(_) => {
                    // Key not found, check if it's a group
                    let group_key = make_group_key(&current_key);
                    if txn.get(db, &group_key.as_bytes()).is_ok() {
                        return Ok((atoms::ok(), atoms::composite()).encode(env));
                    }
                    
                    // Try resolving path segments
                    let resolved_key = resolve_path(&lmdb_env, &current_key);
                    if resolved_key != current_key {
                        current_key = resolved_key;
                        // Check if resolved path is a group
                        let group_key = make_group_key(&current_key);
                        if txn.get(db, &group_key.as_bytes()).is_ok() {
                            return Ok((atoms::ok(), atoms::composite()).encode(env));
                        }
                    }
                    return Ok(atoms::not_found().encode(env));
                }
            }
        }
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
    
    // Get the canonical key for environment lookup
    let key = get_canonical_key(name);
    
    let envs = ENVIRONMENTS.read();
    if let Some(lmdb_env) = envs.get(&key) {
        let db = match lmdb_env.get_or_create_db(None) {
            Ok(db) => db,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        let txn = match lmdb_env.env.begin_ro_txn() {
            Ok(t) => t,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        // First check if the path itself is a link
        let mut resolved_path = path_str.clone();
        match txn.get(db, &resolved_path.as_bytes()) {
            Ok(data) => {
                if data.starts_with(b"@link:") {
                    // This is a link, follow it
                    if let Ok(link_str) = std::str::from_utf8(&data[6..]) {
                        resolved_path = link_str.to_string();
                    }
                }
            }
            _ => {}
        }
        
        // Now resolve any links in the path segments
        resolved_path = resolve_path(&lmdb_env, &resolved_path);
        
        // Check if path is a group
        let group_key = make_group_key(&resolved_path);
        if txn.get(db, &group_key.as_bytes()).is_err() {
            return Ok(atoms::not_found().encode(env));
        }
        
        // List all children
        let prefix = if resolved_path.is_empty() {
            String::new()
        } else {
            format!("{}/", resolved_path)
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
    
    // Get the canonical key for environment lookup
    let key = get_canonical_key(name);
    
    let envs = ENVIRONMENTS.read();
    if let Some(lmdb_env) = envs.get(&key) {
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
    
    // Get the canonical key for environment lookup
    let key = get_canonical_key(name);
    
    let envs = ENVIRONMENTS.read();
    if let Some(lmdb_env) = envs.get(&key) {
        let db = match lmdb_env.get_or_create_db(None) {
            Ok(db) => db,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        // Don't resolve the existing path - we want to create a link to whatever is specified
        // This allows creating chains of links like C -> B -> A
        let resolved_existing = existing_str.clone();
        
        let mut txn = match lmdb_env.env.begin_rw_txn() {
            Ok(t) => t,
            Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
        };
        
        // Ensure parent groups exist for the new link path
        ensure_parent_groups(&lmdb_env, &mut txn, db, &new_str)?;
        
        // Store link target as the value of the new key
        // Prefix with special marker to identify it as a link
        let link_value = format!("@link:{}", resolved_existing);
        match txn.put(db, &new_str.as_bytes(), &link_value.as_bytes(), WriteFlags::empty()) {
            Ok(_) => {
                txn.commit().map_err(|e| Error::Term(Box::new(e.to_string())))?;
                
                // Invalidate cache when links change
                invalidate_link_cache(&new_str);
                
                Ok(atoms::ok().encode(env))
            }
            Err(e) => Ok((atoms::error(), e.to_string()).encode(env)),
        }
    } else {
        Ok(atoms::not_found().encode(env))
    }
}

pub fn path<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, path: Term<'a>) -> NifResult<Term<'a>> {
    let path_str = decode_path(path)?;
    
    // Try to decode store_opts as an environment resource first
    let lmdb_env = if let Ok(resource) = store_opts.decode::<ResourceArc<EnvironmentResource>>() {
        resource.0.clone()
    } else {
        // Fall back to parsing store_opts and looking up by name
        let opts = parse_store_opts(store_opts)?;
        let name = opts.get("name")
            .ok_or(Error::BadArg)?;
        
        // Get the canonical key for environment lookup
        let key = get_canonical_key(name);
        
        let envs = ENVIRONMENTS.read();
        match envs.get(&key).cloned() {
            Some(env) => env,
            None => {
                // If environment not found, just return the path as-is
                return Ok(path_str.encode(env));
            }
        }
    };
    
    // Don't resolve links - just check what's actually stored at this path
    let db = match lmdb_env.get_or_create_db(None) {
        Ok(db) => db,
        Err(_) => return Ok(path_str.encode(env)),
    };
    
    let txn = match lmdb_env.env.begin_ro_txn() {
        Ok(t) => t,
        Err(_) => return Ok(path_str.encode(env)),
    };
    
    // Check what's stored at this exact path
    match txn.get(db, &path_str.as_bytes()) {
        Ok(data) => {
            // If it's a link, return the link target
            if let Ok(value_str) = std::str::from_utf8(data) {
                if value_str.starts_with("@link:") {
                    let link_target = &value_str[6..];
                    return Ok(link_target.encode(env));
                }
            }
            // It exists with data, return the path
            return Ok(path_str.encode(env));
        },
        Err(lmdb::Error::NotFound) => {
            // Check if it exists as a group
            let group_key = make_group_key(&path_str);
            if txn.get(db, &group_key.as_bytes()).is_ok() {
                return Ok(path_str.encode(env));
            }
            // Path doesn't exist
            Ok(atoms::not_found().encode(env))
        },
        Err(_) => Ok(atoms::not_found().encode(env)),
    }
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

pub fn resolve<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, path: Term<'a>) -> NifResult<Term<'a>> {
    let path_str = decode_path(path)?;
    
    // Try to decode store_opts as an environment resource first
    let lmdb_env = if let Ok(resource) = store_opts.decode::<ResourceArc<EnvironmentResource>>() {
        resource.0.clone()
    } else {
        // Fall back to parsing store_opts and looking up by name
        let opts = parse_store_opts(store_opts)?;
        let name = opts.get("name")
            .ok_or(Error::BadArg)?;
        
        // Get the canonical key for environment lookup
        let key = get_canonical_key(name);
        
        let envs = ENVIRONMENTS.read();
        match envs.get(&key).cloned() {
            Some(env) => env,
            None => {
                // If environment not found, return not_found
                return Ok(atoms::not_found().encode(env));
            }
        }
    };
    
    // Resolve any links in the path
    let resolved_path = resolve_path(&lmdb_env, &path_str);
    
    // Check if the resolved path exists
    let db = match lmdb_env.get_or_create_db(None) {
        Ok(db) => db,
        Err(_) => return Ok(atoms::not_found().encode(env)),
    };
    
    let txn = match lmdb_env.env.begin_ro_txn() {
        Ok(t) => t,
        Err(_) => return Ok(atoms::not_found().encode(env)),
    };
    
    // Check if it exists as a value or group
    if txn.get(db, &resolved_path.as_bytes()).is_ok() {
        return Ok((atoms::ok(), resolved_path).encode(env));
    }
    
    // Check if it exists as a group
    let group_key = make_group_key(&resolved_path);
    if txn.get(db, &group_key.as_bytes()).is_ok() {
        return Ok((atoms::ok(), resolved_path).encode(env));
    }
    
    // Path doesn't exist
    Ok(atoms::not_found().encode(env))
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
    // Check cache first
    if let Some(resolved) = PATH_CACHE.get(path) {
        return resolved;
    }
    
    let db = match lmdb_env.get_or_create_db(None) {
        Ok(db) => db,
        Err(_) => return path.to_string(),
    };
    
    let txn = match lmdb_env.env.begin_ro_txn() {
        Ok(t) => t,
        Err(_) => return path.to_string(),
    };
    
    let mut visited = HashSet::new();
    let resolved = resolve_path_with_txn(&txn, db, path, &mut visited);
    
    // Cache the result if it's different from the input
    if resolved != path {
        PATH_CACHE.insert(path.to_string(), resolved.clone());
    }
    
    resolved
}

fn resolve_path_with_txn(txn: &lmdb::RoTransaction, db: lmdb::Database, path: &str, visited: &mut HashSet<String>) -> String {
    // Split path into segments
    let segments: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();
    if segments.is_empty() {
        return path.to_string();
    }
    
    // Process each segment to check for links
    let mut current_base = String::new();
    
    for (i, segment) in segments.iter().enumerate() {
        // Build path up to current segment
        if i == 0 {
            current_base = segment.to_string();
        } else {
            current_base = format!("{}/{}", current_base, segment);
        }
        
        // Check if current path is a link
        match txn.get(db, &current_base.as_bytes()) {
            Ok(value) => {
                // Found a value, check if it's a link
                if let Ok(value_str) = std::str::from_utf8(value) {
                    if value_str.starts_with("@link:") {
                        // Prevent circular references
                        if visited.contains(&current_base) {
                            return path.to_string();
                        }
                        visited.insert(current_base.clone());
                        
                        // Get the link target
                        let link_target = &value_str[6..];
                        
                        // If there are remaining segments, append them to the link target
                        if i < segments.len() - 1 {
                            let remaining_segments = &segments[i+1..];
                            let remaining_path = remaining_segments.join("/");
                            let combined_path = if link_target.is_empty() {
                                remaining_path
                            } else {
                                format!("{}/{}", link_target, remaining_path)
                            };
                            
                            // Recursively resolve the combined path
                            return resolve_path_with_txn(txn, db, &combined_path, visited);
                        } else {
                            // This was the last segment, just resolve the link target
                            return resolve_path_with_txn(txn, db, link_target, visited);
                        }
                    }
                }
            }
            Err(e) => {
                // Path segment doesn't exist as a key, continue
            }
        }
    }
    
    // No links found in any segment, return the original path
    path.to_string()
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

pub fn read_many<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>, keys: Term<'a>) -> NifResult<Term<'a>> {
    let key_list: Vec<String> = keys.decode::<Vec<Term>>()?.into_iter()
        .filter_map(|k| decode_path(k).ok())
        .collect();
    
    // Try to decode store_opts as an environment resource first
    let lmdb_env = if let Ok(resource) = store_opts.decode::<ResourceArc<EnvironmentResource>>() {
        resource.0.clone()
    } else {
        // Fall back to parsing store_opts and looking up by name
        let opts = parse_store_opts(store_opts)?;
        let name = opts.get("name")
            .ok_or(Error::BadArg)?;
        
        // Get the canonical key for environment lookup
        let key = get_canonical_key(name);
        
        let envs = ENVIRONMENTS.read();
        envs.get(&key).cloned().ok_or(Error::BadArg)?
    };
    
    let db = match lmdb_env.get_or_create_db(None) {
        Ok(db) => db,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    // Use a single transaction for all reads
    let txn = match lmdb_env.env.begin_ro_txn() {
        Ok(t) => t,
        Err(e) => return Ok((atoms::error(), e.to_string()).encode(env)),
    };
    
    let mut results = Vec::new();
    
    for key_str in key_list {
        // Follow links if we find them
        let mut current_key = key_str.clone();
        let mut link_depth = 0;
        const MAX_LINK_DEPTH: usize = 10;
        
        let result = loop {
            match txn.get(db, &current_key.as_bytes()) {
                Ok(data) => {
                    // Check for prefixes by looking at the raw bytes
                    if data.starts_with(b"@link:") {
                        // This is a link - follow it
                        if link_depth >= MAX_LINK_DEPTH {
                            break atoms::not_found().encode(env);
                        }
                        link_depth += 1;
                        
                        // Extract the link target
                        if let Ok(link_str) = std::str::from_utf8(&data[6..]) {
                            // Resolve any path components in the link target
                            current_key = resolve_path(&lmdb_env, link_str);
                            continue; // Try to read the link target
                        } else {
                            break atoms::not_found().encode(env);
                        }
                    } else if data.starts_with(b"@data:") {
                        // Regular data - strip the prefix
                        let actual_data = &data[6..];
                        let mut binary = rustler::OwnedBinary::new(actual_data.len()).unwrap();
                        binary.as_mut_slice().copy_from_slice(actual_data);
                        break (atoms::ok(), binary.release(env)).encode(env);
                    } else {
                        // Legacy data without prefix
                        let mut binary = rustler::OwnedBinary::new(data.len()).unwrap();
                        binary.as_mut_slice().copy_from_slice(data);
                        break (atoms::ok(), binary.release(env)).encode(env);
                    }
                }
                Err(lmdb::Error::NotFound) => break atoms::not_found().encode(env),
                Err(e) => break (atoms::error(), e.to_string()).encode(env),
            }
        };
        
        results.push((key_str, result).encode(env));
    }
    
    Ok((atoms::ok(), results).encode(env))
}

pub fn sync<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>) -> NifResult<Term<'a>> {
    // Try to decode store_opts as an environment resource first
    let lmdb_env = if let Ok(resource) = store_opts.decode::<ResourceArc<EnvironmentResource>>() {
        resource.0.clone()
    } else {
        // Fall back to parsing store_opts and looking up by name
        let opts = parse_store_opts(store_opts)?;
        let name = opts.get("name")
            .ok_or(Error::BadArg)?;
        
        // Get the canonical key for environment lookup
        let key = get_canonical_key(name);
        
        let envs = ENVIRONMENTS.read();
        envs.get(&key).cloned().ok_or(Error::BadArg)?
    };
    
    // Force a full sync of the environment
    match lmdb_env.env.sync(true) {
        Ok(_) => Ok(atoms::ok().encode(env)),
        Err(e) => Ok((atoms::error(), e.to_string()).encode(env)),
    }
}