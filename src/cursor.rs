use lmdb::{Cursor, RoCursor, RwCursor};
use crate::error::StoreResult;

pub enum CursorType<'txn> {
    ReadOnly(RoCursor<'txn>),
    ReadWrite(RwCursor<'txn>),
}

impl<'txn> CursorType<'txn> {
    pub fn iter_from(&mut self, key: &[u8]) -> StoreResult<Vec<(Vec<u8>, Vec<u8>)>> {
        let mut results = Vec::new();
        
        // Use the cursor's iterator
        match self {
            CursorType::ReadOnly(cursor) => {
                for (k, v) in cursor.iter_from(key) {
                    results.push((k.to_vec(), v.to_vec()));
                }
            }
            CursorType::ReadWrite(cursor) => {
                for (k, v) in cursor.iter_from(key) {
                    results.push((k.to_vec(), v.to_vec()));
                }
            }
        }
        
        Ok(results)
    }
    
    pub fn iter_prefix(&mut self, prefix: &[u8]) -> StoreResult<Vec<(Vec<u8>, Vec<u8>)>> {
        let mut results = Vec::new();
        
        // Use the cursor's iterator starting from prefix
        match self {
            CursorType::ReadOnly(cursor) => {
                for (k, v) in cursor.iter_from(prefix) {
                    if !k.starts_with(prefix) {
                        break;
                    }
                    results.push((k.to_vec(), v.to_vec()));
                }
            }
            CursorType::ReadWrite(cursor) => {
                for (k, v) in cursor.iter_from(prefix) {
                    if !k.starts_with(prefix) {
                        break;
                    }
                    results.push((k.to_vec(), v.to_vec()));
                }
            }
        }
        
        Ok(results)
    }
}