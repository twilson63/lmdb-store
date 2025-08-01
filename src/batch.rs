use crate::atoms;
use crate::environment::{EnvironmentResource, LmdbEnvironment};
use crate::cache::{invalidate_path_cache, invalidate_link_cache};
use lmdb::{Database, RwTransaction, Transaction, WriteFlags};
use parking_lot::Mutex;
use rustler::{Binary, Encoder, Env as RustlerEnv, Error, NifResult, ResourceArc, Term};
use std::sync::Arc;

/// Represents a single operation in a batch
#[derive(Debug, Clone)]
pub enum BatchOperation {
    Write { key: String, value: Vec<u8> },
    MakeGroup { path: String },
    MakeLink { existing: String, new: String },
    Delete { key: String },
}

/// A batch transaction context
pub struct BatchTransaction {
    pub env: Arc<LmdbEnvironment>,
    pub operations: Vec<BatchOperation>,
    pub committed: bool,
}

/// Resource type for batch transactions
pub struct BatchResource(pub Arc<Mutex<BatchTransaction>>);

pub fn on_load(env: RustlerEnv) -> bool {
    rustler::resource!(BatchResource, env);
    true
}

impl BatchTransaction {
    pub fn new(env: Arc<LmdbEnvironment>) -> Self {
        BatchTransaction {
            env,
            operations: Vec::new(),
            committed: false,
        }
    }

    pub fn add_operation(&mut self, op: BatchOperation) {
        if !self.committed {
            self.operations.push(op);
        }
    }

    pub fn execute(&mut self) -> Result<(), String> {
        if self.committed {
            return Err("Batch already committed".to_string());
        }

        let db = self.env.get_or_create_db(None)
            .map_err(|e| format!("Failed to get database: {}", e))?;

        let mut txn = self.env.env.begin_rw_txn()
            .map_err(|e| format!("Failed to begin transaction: {}", e))?;

        // Execute all operations in the same transaction
        for op in &self.operations {
            match op {
                BatchOperation::Write { key, value } => {
                    // Ensure parent groups exist
                    ensure_parent_groups(&mut txn, db, key)?;
                    
                    // Write with @data: prefix
                    let prefixed_value = [b"@data:", value.as_slice()].concat();
                    txn.put(db, &key.as_bytes(), &prefixed_value, WriteFlags::empty())
                        .map_err(|e| format!("Write failed for key '{}': {}", key, e))?;
                }
                
                BatchOperation::MakeGroup { path } => {
                    // Ensure parent groups exist
                    ensure_parent_groups(&mut txn, db, path)?;
                    
                    // Create group marker
                    let group_key = format!("{}/__group__", path);
                    txn.put(db, &group_key.as_bytes(), b"1", WriteFlags::empty())
                        .map_err(|e| format!("MakeGroup failed for '{}': {}", path, e))?;
                }
                
                BatchOperation::MakeLink { existing, new } => {
                    // Ensure parent groups exist for the new link
                    ensure_parent_groups(&mut txn, db, new)?;
                    
                    // Resolve existing path first
                    let resolved = resolve_in_transaction(&txn, db, existing);
                    
                    // Create link
                    let link_value = format!("@link:{}", resolved);
                    txn.put(db, &new.as_bytes(), &link_value.as_bytes(), WriteFlags::empty())
                        .map_err(|e| format!("MakeLink failed '{}' -> '{}': {}", existing, new, e))?;
                }
                
                BatchOperation::Delete { key } => {
                    txn.del(db, &key.as_bytes(), None)
                        .map_err(|e| format!("Delete failed for '{}': {}", key, e))?;
                }
            }
        }

        // Commit all operations atomically
        txn.commit()
            .map_err(|e| format!("Failed to commit transaction: {}", e))?;

        // Force sync to ensure visibility
        self.env.env.sync(true)
            .map_err(|e| format!("Failed to sync: {}", e))?;

        // Invalidate cache for all affected paths
        let mut has_links = false;
        for op in &self.operations {
            match op {
                BatchOperation::Write { key, .. } => {
                    invalidate_path_cache(key);
                }
                BatchOperation::MakeGroup { path } => {
                    invalidate_path_cache(path);
                }
                BatchOperation::MakeLink { new, .. } => {
                    has_links = true;
                    invalidate_link_cache(new);
                }
                BatchOperation::Delete { key } => {
                    invalidate_path_cache(key);
                }
            }
        }
        
        // If any links were created, we already cleared the entire cache
        // so no need to invalidate individual paths

        self.committed = true;
        Ok(())
    }
}

// Helper function to ensure parent groups exist
fn ensure_parent_groups(
    txn: &mut RwTransaction,
    db: Database,
    path: &str
) -> Result<(), String> {
    let parts: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();
    let mut current_path = String::new();
    
    for i in 0..parts.len().saturating_sub(1) {
        if i > 0 {
            current_path.push('/');
        }
        current_path.push_str(parts[i]);
        
        let group_key = format!("{}/__group__", current_path);
        if txn.get(db, &group_key.as_bytes()).is_err() {
            txn.put(db, &group_key.as_bytes(), b"1", WriteFlags::empty())
                .map_err(|e| format!("Failed to create parent group '{}': {}", current_path, e))?;
        }
    }
    
    Ok(())
}

// Resolve path within the current transaction
fn resolve_in_transaction(
    txn: &RwTransaction,
    db: Database,
    path: &str
) -> String {
    // For now, just return the path as-is
    // TODO: Implement full link resolution within transaction
    path.to_string()
}

// NIF functions for batch operations

pub fn begin_batch<'a>(env: RustlerEnv<'a>, store_opts: Term<'a>) -> NifResult<Term<'a>> {
    // Get the environment similar to how other functions do it
    let lmdb_env = if let Ok(resource) = store_opts.decode::<ResourceArc<EnvironmentResource>>() {
        resource.0.clone()
    } else {
        // Fall back to parsing store_opts and looking up by name
        let opts = crate::store::parse_store_opts(store_opts)?;
        let name = opts.get("name")
            .ok_or(Error::BadArg)?;
        
        // Get the canonical key for environment lookup
        let key = crate::store::get_canonical_key(name);
        
        let envs = crate::store::ENVIRONMENTS.read();
        match envs.get(&key).cloned() {
            Some(env) => env,
            None => return Ok((atoms::error(), "Environment not found").encode(env))
        }
    };

    // Create new batch transaction
    let batch = BatchTransaction::new(lmdb_env);
    let resource = ResourceArc::new(BatchResource(Arc::new(Mutex::new(batch))));
    
    Ok((atoms::ok(), resource).encode(env))
}

pub fn batch_write<'a>(
    env: RustlerEnv<'a>, 
    batch_term: Term<'a>, 
    key: Term<'a>, 
    value: Term<'a>
) -> NifResult<Term<'a>> {
    let batch_resource: ResourceArc<BatchResource> = batch_term.decode()?;
    let mut batch = batch_resource.0.lock();
    
    let key_str = crate::store::decode_path(key)?;
    let value_bin: Binary = value.decode()?;
    
    batch.add_operation(BatchOperation::Write {
        key: key_str,
        value: value_bin.as_slice().to_vec(),
    });
    
    Ok(atoms::ok().encode(env))
}

pub fn batch_make_group<'a>(
    env: RustlerEnv<'a>, 
    batch_term: Term<'a>, 
    path: Term<'a>
) -> NifResult<Term<'a>> {
    let batch_resource: ResourceArc<BatchResource> = batch_term.decode()?;
    let mut batch = batch_resource.0.lock();
    
    let path_str = crate::store::decode_path(path)?;
    
    batch.add_operation(BatchOperation::MakeGroup {
        path: path_str,
    });
    
    Ok(atoms::ok().encode(env))
}

pub fn batch_make_link<'a>(
    env: RustlerEnv<'a>, 
    batch_term: Term<'a>, 
    existing: Term<'a>,
    new: Term<'a>
) -> NifResult<Term<'a>> {
    let batch_resource: ResourceArc<BatchResource> = batch_term.decode()?;
    let mut batch = batch_resource.0.lock();
    
    let existing_str = crate::store::decode_path(existing)?;
    let new_str = crate::store::decode_path(new)?;
    
    batch.add_operation(BatchOperation::MakeLink {
        existing: existing_str,
        new: new_str,
    });
    
    Ok(atoms::ok().encode(env))
}

pub fn commit_batch<'a>(env: RustlerEnv<'a>, batch_term: Term<'a>) -> NifResult<Term<'a>> {
    let batch_resource: ResourceArc<BatchResource> = batch_term.decode()?;
    let mut batch = batch_resource.0.lock();
    
    match batch.execute() {
        Ok(_) => Ok(atoms::ok().encode(env)),
        Err(e) => Ok((atoms::error(), e).encode(env)),
    }
}