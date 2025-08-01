# Batching and Caching Design

## Goals
1. **Atomic batch operations** - Multiple operations execute in a single transaction
2. **Path resolution caching** - Cache resolved paths to avoid repeated lookups
3. **Performance optimization** - Reduce transaction overhead

## Design

### 1. Batch Operations API

```rust
// Batch operation types
enum BatchOp {
    Write(String, Vec<u8>),
    MakeGroup(String),
    MakeLink(String, String),
    Delete(String),
}

// Execute multiple operations atomically
pub fn batch_execute(ops: Vec<BatchOp>) -> Result<()>
```

### 2. Transaction Context

Allow Erlang to manage transaction boundaries:

```erlang
{ok, Txn} = hyper_lmdb:begin_batch(Store),
ok = hyper_lmdb:batch_write(Txn, Key1, Value1),
ok = hyper_lmdb:batch_write(Txn, Key2, Value2),
ok = hyper_lmdb:batch_make_link(Txn, Path1, Path2),
ok = hyper_lmdb:commit_batch(Txn).
```

### 3. Path Resolution Cache

```rust
// Thread-safe LRU cache for resolved paths
struct PathCache {
    cache: Mutex<LruCache<String, String>>,
    ttl: Duration,
}

// Cache resolved paths with TTL
fn resolve_path_cached(path: &str) -> String {
    if let Some(resolved) = cache.get(path) {
        return resolved;
    }
    let resolved = resolve_path_uncached(path);
    cache.insert(path, resolved, ttl);
    resolved
}
```

### 4. Implementation Plan

1. Add batch operation types and NIF functions
2. Implement transaction context management
3. Add LRU cache for path resolution
4. Update hb_store operations to use batching when possible
5. Add cache invalidation on writes/links

## Benefits

- **Atomicity**: All operations in a batch succeed or fail together
- **Performance**: Single transaction for multiple operations
- **Consistency**: No visibility gaps between operations
- **Speed**: Cached path resolution reduces repeated lookups