use lmdb::{Database, RoTransaction, RwTransaction, Transaction, WriteFlags};
use crate::error::{StoreError, StoreResult};

pub enum TransactionType<'a> {
    ReadOnly(RoTransaction<'a>),
    ReadWrite(RwTransaction<'a>),
}

impl<'a> TransactionType<'a> {
    pub fn get(&self, db: Database, key: &[u8]) -> StoreResult<Vec<u8>> {
        let result = match self {
            TransactionType::ReadOnly(txn) => txn.get(db, &key),
            TransactionType::ReadWrite(txn) => txn.get(db, &key),
        };
        
        match result {
            Ok(data) => Ok(data.to_vec()),
            Err(lmdb::Error::NotFound) => Err(StoreError::NotFound),
            Err(e) => Err(StoreError::from(e)),
        }
    }
    
    pub fn put(&mut self, db: Database, key: &[u8], value: &[u8]) -> StoreResult<()> {
        match self {
            TransactionType::ReadOnly(_) => Err(StoreError::InvalidValue),
            TransactionType::ReadWrite(txn) => {
                txn.put(db, &key, &value, WriteFlags::empty())?;
                Ok(())
            }
        }
    }
    
    pub fn del(&mut self, db: Database, key: &[u8]) -> StoreResult<()> {
        match self {
            TransactionType::ReadOnly(_) => Err(StoreError::InvalidValue),
            TransactionType::ReadWrite(txn) => {
                match txn.del(db, &key, None) {
                    Ok(_) => Ok(()),
                    Err(lmdb::Error::NotFound) => Err(StoreError::NotFound),
                    Err(e) => Err(StoreError::from(e)),
                }
            }
        }
    }
    
    pub fn commit(self) -> StoreResult<()> {
        match self {
            TransactionType::ReadOnly(txn) => {
                txn.commit()?;
                Ok(())
            }
            TransactionType::ReadWrite(txn) => {
                txn.commit()?;
                Ok(())
            }
        }
    }
    
    pub fn abort(self) {
        match self {
            TransactionType::ReadOnly(txn) => txn.abort(),
            TransactionType::ReadWrite(txn) => txn.abort(),
        }
    }
}