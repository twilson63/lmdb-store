# Performance Improvements for hyper_lmdb

## Current Performance Issues

1. **Link Resolution**: Every read operation re-resolves links from scratch
2. **No Caching**: The cache module exists but isn't being used
3. **Transaction Overhead**: Creating new transactions for every operation
4. **No Read Transaction Pooling**: Read transactions could be reused
5. **Synchronous Operations**: All operations block the scheduler

## Proposed Improvements

### 1. Enable Additional LMDB Flags

```rust
// Current flags
.set_flags(EnvironmentFlags::WRITE_MAP)

// Optimized flags for better performance
.set_flags(
    EnvironmentFlags::WRITE_MAP |
    EnvironmentFlags::MAP_ASYNC |  // Async memory mapping
    EnvironmentFlags::NO_READAHEAD  // Disable OS readahead (we manage caching)
)
```

### 2. Implement Path Resolution Caching

The cache module exists but isn't being used. We need to:
- Use `resolve_path_cached` instead of `resolve_path` in read operations
- Cache resolved paths to avoid repeated link traversal
- Implement smart cache invalidation

### 3. Read Transaction Pooling

Instead of creating a new read transaction for each operation:
- Maintain a pool of read transactions
- Reuse transactions across multiple reads
- Refresh transactions periodically

### 4. Batch Read Operations

For operations that read multiple keys:
- Use a single transaction for multiple reads
- Implement a batch read API

### 5. Optimize Link Resolution

Current link resolution is recursive and inefficient:
- Cache intermediate link resolutions
- Implement iterative resolution instead of recursive
- Pre-resolve common link chains

### 6. Memory-Mapped I/O Optimizations

- Use larger map size to reduce remapping
- Implement periodic sync instead of per-operation sync
- Use WRITE_MAP more efficiently

### 7. NIF Optimization

- Use dirty schedulers for I/O operations
- Implement yielding for long operations
- Reduce data copying between Erlang and Rust

## Benchmarking Plan

1. Create comprehensive benchmarks for:
   - Single key reads/writes
   - Batch operations
   - Link resolution
   - List operations

2. Measure:
   - Operations per second
   - Latency percentiles
   - Memory usage
   - CPU usage

3. Compare with:
   - Raw LMDB performance
   - Other Erlang storage backends
   - HyperBEAM requirements