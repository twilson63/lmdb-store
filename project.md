# elmdb-rs

this is an erlang nif implemented in rust.

# LMDB-rs API Guide

## Overview

The `lmdb-rs` crate provides safe Rust bindings for the Lightning Memory-Mapped Database (LMDB). It offers idiomatic Rust APIs while maintaining the performance characteristics of LMDB.

## Core Types

### Environment
The central entry point for LMDB operations.

```rust
use lmdb::{Environment, EnvironmentFlags};

// Create and configure an environment
let env = Environment::new()
    .set_flags(EnvironmentFlags::WRITE_MAP)
    .set_map_size(10485760)     // 10MB
    .set_max_dbs(10)            // Max named databases
    .set_max_readers(126)       // Max concurrent readers
    .open(Path::new("./mydb"))
    .unwrap();
```

### Database
Represents a key-value store within an environment.

```rust
use lmdb::{Database, DatabaseFlags};

// Open the default database
let db = env.open_db(None).unwrap();

// Open a named database
let named_db = env.create_db(Some("mydb"), DatabaseFlags::empty()).unwrap();
```

### Transactions
All database operations require a transaction.

#### Read-Only Transactions
```rust
// Begin a read-only transaction
let ro_txn = env.begin_ro_txn().unwrap();

// Get a value
match ro_txn.get(db, &b"key1") {
    Ok(value) => println!("Value: {:?}", value),
    Err(lmdb::Error::NotFound) => println!("Key not found"),
    Err(e) => panic!("Error: {}", e),
}

// Commit or abort
ro_txn.commit().unwrap();  // or ro_txn.abort()
```

#### Read-Write Transactions
```rust
// Begin a read-write transaction
let mut rw_txn = env.begin_rw_txn().unwrap();

// Put a value
rw_txn.put(db, &b"key1", &b"value1", WriteFlags::empty()).unwrap();

// Delete a value
rw_txn.del(db, &b"key2", None).unwrap();

// Commit the transaction
rw_txn.commit().unwrap();
```

### Cursors
For iterating over database contents.

```rust
// Read-only cursor
let ro_txn = env.begin_ro_txn().unwrap();
let cursor = ro_txn.open_ro_cursor(db).unwrap();

// Iterate over all items
for (key, value) in cursor.iter() {
    println!("Key: {:?}, Value: {:?}", key, value);
}

// Position cursor and iterate
for result in cursor.iter_from(b"key5") {
    let (key, value) = result.unwrap();
    println!("Key: {:?}, Value: {:?}", key, value);
}

// Iterate over duplicates (if DB allows duplicates)
for (key, value) in cursor.iter_dup_of(b"key1") {
    println!("Duplicate value: {:?}", value);
}
```

## Common Operations

### Basic CRUD Operations

```rust
use lmdb::{Environment, Transaction, WriteFlags};

fn example_operations(env: &Environment) -> lmdb::Result<()> {
    let db = env.open_db(None)?;
    
    // Write operations
    let mut txn = env.begin_rw_txn()?;
    
    // Insert
    txn.put(db, &b"user:1", &b"Alice", WriteFlags::empty())?;
    
    // Insert without overwrite
    match txn.put(db, &b"user:1", &b"Bob", WriteFlags::NO_OVERWRITE) {
        Err(lmdb::Error::KeyExist) => println!("Key already exists"),
        _ => {}
    }
    
    // Update (will overwrite)
    txn.put(db, &b"user:1", &b"Alice Updated", WriteFlags::empty())?;
    
    // Delete
    txn.del(db, &b"user:2", None)?;
    
    txn.commit()?;
    
    // Read operations
    let txn = env.begin_ro_txn()?;
    
    // Check if key exists
    if let Ok(value) = txn.get(db, &b"user:1") {
        println!("Found: {:?}", std::str::from_utf8(value).unwrap());
    }
    
    txn.commit()?;
    
    Ok(())
}
```

### Bulk Operations

```rust
fn bulk_insert(env: &Environment, data: Vec<(&[u8], &[u8])>) -> lmdb::Result<()> {
    let db = env.open_db(None)?;
    let mut txn = env.begin_rw_txn()?;
    
    for (key, value) in data {
        txn.put(db, &key, &value, WriteFlags::empty())?;
    }
    
    txn.commit()
}
```

### Using Sorted Keys

```rust
fn range_query(env: &Environment, start: &[u8], end: &[u8]) -> lmdb::Result<Vec<(Vec<u8>, Vec<u8>)>> {
    let db = env.open_db(None)?;
    let txn = env.begin_ro_txn()?;
    let mut cursor = txn.open_ro_cursor(db)?;
    
    let mut results = Vec::new();
    
    // Position at start key
    cursor.get(Some(start), None, lmdb::Cursor::SET_RANGE)?;
    
    loop {
        match cursor.get(None, None, lmdb::Cursor::CURRENT) {
            Ok((key, value)) => {
                if key > end {
                    break;
                }
                results.push((key.to_vec(), value.to_vec()));
                
                if cursor.get(None, None, lmdb::Cursor::NEXT).is_err() {
                    break;
                }
            }
            Err(_) => break,
        }
    }
    
    txn.commit()?;
    Ok(results)
}
```

### Zero-Copy Reads

```rust
// LMDB returns references to memory-mapped data
let txn = env.begin_ro_txn()?;
let value: &[u8] = txn.get(db, &b"key")?;
// `value` points directly to the memory-mapped data
// No allocation or copying occurred
```

### Nested Transactions

```rust
let mut parent_txn = env.begin_rw_txn()?;
parent_txn.put(db, &b"parent", &b"value", WriteFlags::empty())?;

// Create nested transaction
let mut nested_txn = env.begin_nested_txn(&mut parent_txn)?;
nested_txn.put(db, &b"nested", &b"value", WriteFlags::empty())?;

// Commit nested transaction
nested_txn.commit()?;

// Parent can now see nested changes
assert!(parent_txn.get(db, &b"nested").is_ok());

parent_txn.commit()?;
```

## Database Configuration

### Database Flags

```rust
use lmdb::DatabaseFlags;

// Create database with duplicates
let dup_db = env.create_db(
    Some("duplicates"), 
    DatabaseFlags::DUP_SORT
)?;

// Integer keys
let int_db = env.create_db(
    Some("integers"),
    DatabaseFlags::INTEGER_KEY
)?;

// Reverse key comparison
let rev_db = env.create_db(
    Some("reverse"),
    DatabaseFlags::REVERSE_KEY
)?;
```

### Environment Flags

```rust
use lmdb::EnvironmentFlags;

let env = Environment::new()
    .set_flags(
        EnvironmentFlags::WRITE_MAP |     // Use writeable mmap
        EnvironmentFlags::NO_SYNC |        // Don't fsync after commit
        EnvironmentFlags::MAP_ASYNC        // Use asynchronous msync
    )
    .open(path)?;
```

## Error Handling

```rust
use lmdb::Error;

match txn.get(db, &b"key") {
    Ok(value) => println!("Found: {:?}", value),
    Err(Error::NotFound) => println!("Key not found"),
    Err(Error::MapFull) => println!("Database is full"),
    Err(Error::ReadersFull) => println!("Too many readers"),
    Err(e) => println!("Other error: {}", e),
}
```

## Statistics

```rust
// Environment stats
let stat = env.stat()?;
println!("Page size: {}", stat.page_size());
println!("Tree depth: {}", stat.depth());
println!("Entries: {}", stat.entries());

// Database stats
let db_stat = txn.db_stat(db)?;
println!("DB entries: {}", db_stat.entries());
```

## Thread Safety

- `Environment` is thread-safe and can be shared
- Read-only transactions can be used from any thread
- Write transactions are tied to the thread that created them
- Multiple read transactions can run concurrently
- Only one write transaction at a time

## Best Practices

1. **Keep transactions short**: Long-running read transactions can cause the database to grow
2. **Reuse environments**: Opening is expensive, keep environments open
3. **Use read-only when possible**: Read transactions are cheap and concurrent
4. **Handle LMDB errors**: Especially `MapFull` and `ReadersFull`
5. **Set appropriate map size**: LMDB doesn't grow automatically

## Integration with Rustler

When using with Rustler for Erlang NIFs:

```rust
use rustler::{Encoder, Env, Term, NifResult};
use lmdb::{Environment, Transaction};

#[rustler::nif]
fn get_value<'a>(env: Env<'a>, key: String) -> NifResult<Option<String>> {
    let lmdb_env = // ... get LMDB environment
    let db = // ... get database
    
    let txn = lmdb_env.begin_ro_txn()
        .map_err(|e| rustler::Error::Term(Box::new(e.to_string())))?;
    
    match txn.get(db, &key.as_bytes()) {
        Ok(value) => {
            let string_value = String::from_utf8_lossy(value).to_string();
            Ok(Some(string_value))
        },
        Err(lmdb::Error::NotFound) => Ok(None),
        Err(e) => Err(rustler::Error::Term(Box::new(e.to_string()))),
    }
}
```
