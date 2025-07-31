use rustler::{Atom, Encoder, Env, Term};
use thiserror::Error;
use crate::atoms;

#[derive(Error, Debug)]
pub enum StoreError {
    #[error("LMDB error: {0}")]
    Lmdb(#[from] lmdb::Error),
    
    #[error("Invalid path: {0}")]
    InvalidPath(String),
    
    #[error("Invalid value")]
    InvalidValue,
    
    #[error("Not found")]
    NotFound,
    
    #[error("Store not initialized")]
    NotInitialized,
    
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    
    #[error("UTF-8 error: {0}")]
    Utf8(#[from] std::str::Utf8Error),
    
    #[error("Serialization error: {0}")]
    Serialization(String),
}

impl StoreError {
    pub fn to_atom(&self) -> Atom {
        match self {
            StoreError::Lmdb(e) => match e {
                lmdb::Error::NotFound => atoms::not_found(),
                lmdb::Error::MapFull => atoms::db_full(),
                lmdb::Error::ReadersFull => atoms::readers_full(),
                lmdb::Error::TxnFull => atoms::txn_full(),
                lmdb::Error::CursorFull => atoms::cursor_full(),
                lmdb::Error::PageFull => atoms::page_full(),
                lmdb::Error::Corrupted => atoms::corrupted(),
                lmdb::Error::Panic => atoms::panic(),
                lmdb::Error::VersionMismatch => atoms::version_mismatch(),
                lmdb::Error::Invalid => atoms::invalid(),
                lmdb::Error::MapResized => atoms::db_full(),
                lmdb::Error::Incompatible => atoms::version_mismatch(),
                lmdb::Error::BadRslot => atoms::invalid(),
                lmdb::Error::BadTxn => atoms::invalid(),
                lmdb::Error::BadValSize => atoms::invalid_value(),
                lmdb::Error::BadDbi => atoms::invalid(),
                lmdb::Error::PageNotFound => atoms::not_found(),
                lmdb::Error::KeyExist => atoms::error(),
                lmdb::Error::NotFound => atoms::not_found(),
                _ => atoms::error(),
            },
            StoreError::InvalidPath(_) => atoms::invalid_path(),
            StoreError::InvalidValue => atoms::invalid_value(),
            StoreError::NotFound => atoms::not_found(),
            StoreError::NotInitialized => atoms::error(),
            StoreError::Io(_) => atoms::error(),
            StoreError::Utf8(_) => atoms::invalid_value(),
            StoreError::Serialization(_) => atoms::invalid_value(),
        }
    }
    
    pub fn to_error_tuple<'a>(&self, env: Env<'a>) -> Term<'a> {
        let error_atom = self.to_atom();
        let message = self.to_string();
        (atoms::error(), error_atom, message).encode(env)
    }
}

pub type StoreResult<T> = Result<T, StoreError>;