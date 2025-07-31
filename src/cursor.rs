use lmdb::{Cursor, Database, RoCursor, RwCursor, Transaction};
use crate::error::{StoreError, StoreResult};

pub enum CursorType<'txn> {
    ReadOnly(RoCursor<'txn>),
    ReadWrite(RwCursor<'txn>),
}

impl<'txn> CursorType<'txn> {
    pub fn iter_from(&mut self, key: &[u8]) -> StoreResult<Vec<(Vec<u8>, Vec<u8>)>> {
        let mut results = Vec::new();
        
        // Position cursor at or after the given key
        let positioned = match self {
            CursorType::ReadOnly(cursor) => {
                cursor.get(Some(key), None, lmdb::Cursor::SET_RANGE).is_ok()
            }
            CursorType::ReadWrite(cursor) => {
                cursor.get(Some(key), None, lmdb::Cursor::SET_RANGE).is_ok()
            }
        };
        
        if !positioned {
            return Ok(results);
        }
        
        // Collect all key-value pairs from this position
        loop {
            let item = match self {
                CursorType::ReadOnly(cursor) => cursor.get(None, None, lmdb::Cursor::CURRENT),
                CursorType::ReadWrite(cursor) => cursor.get(None, None, lmdb::Cursor::CURRENT),
            };
            
            match item {
                Ok((k, v)) => {
                    results.push((k.to_vec(), v.to_vec()));
                    
                    // Move to next
                    let next = match self {
                        CursorType::ReadOnly(cursor) => cursor.get(None, None, lmdb::Cursor::NEXT),
                        CursorType::ReadWrite(cursor) => cursor.get(None, None, lmdb::Cursor::NEXT),
                    };
                    
                    if next.is_err() {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
        
        Ok(results)
    }
    
    pub fn iter_prefix(&mut self, prefix: &[u8]) -> StoreResult<Vec<(Vec<u8>, Vec<u8>)>> {
        let mut results = Vec::new();
        
        // Position cursor at or after the prefix
        let positioned = match self {
            CursorType::ReadOnly(cursor) => {
                cursor.get(Some(prefix), None, lmdb::Cursor::SET_RANGE).is_ok()
            }
            CursorType::ReadWrite(cursor) => {
                cursor.get(Some(prefix), None, lmdb::Cursor::SET_RANGE).is_ok()
            }
        };
        
        if !positioned {
            return Ok(results);
        }
        
        // Collect all key-value pairs that start with the prefix
        loop {
            let item = match self {
                CursorType::ReadOnly(cursor) => cursor.get(None, None, lmdb::Cursor::CURRENT),
                CursorType::ReadWrite(cursor) => cursor.get(None, None, lmdb::Cursor::CURRENT),
            };
            
            match item {
                Ok((k, v)) => {
                    if !k.starts_with(prefix) {
                        break;
                    }
                    
                    results.push((k.to_vec(), v.to_vec()));
                    
                    // Move to next
                    let next = match self {
                        CursorType::ReadOnly(cursor) => cursor.get(None, None, lmdb::Cursor::NEXT),
                        CursorType::ReadWrite(cursor) => cursor.get(None, None, lmdb::Cursor::NEXT),
                    };
                    
                    if next.is_err() {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
        
        Ok(results)
    }
}