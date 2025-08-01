# Configurable LMDB Flags

## Overview

All LMDB environment flags are now configurable via store options, allowing users to choose their preferred balance between performance and durability.

## Available Flags

### Performance Flags (Default: true)

- **`no_sync`** - Don't sync after each transaction
  - Default: `true` (performance mode)
  - Set to `false` for maximum durability
  - Impact: ~7x performance difference

- **`map_async`** - Use asynchronous memory-mapped writes
  - Default: `true`
  - Works with WRITE_MAP for async writes

- **`no_meta_sync`** - Don't sync metadata
  - Default: `true`
  - Metadata sync can be expensive

- **`no_readahead`** - Disable OS readahead
  - Default: `true`
  - Better for random access patterns

### Special Purpose Flags (Default: false)

- **`no_tls`** - Disable thread-local storage
  - Default: `false`
  - Only set true if you know what you're doing

- **`no_lock`** - Disable locking (single process only)
  - Default: `false`
  - DANGER: Only for single-process scenarios

- **`read_only`** - Open in read-only mode
  - Default: `false`
  - Useful for read-only replicas

## Usage Examples

### Maximum Performance (Default)
```erlang
StoreOpts = #{
    <<"name">> => <<"mystore">>,
    <<"capacity">> => 10 * 1024 * 1024 * 1024  % 10GB
}.
% All performance flags enabled by default
```

### Maximum Durability
```erlang
StoreOpts = #{
    <<"name">> => <<"mystore">>,
    <<"capacity">> => 10 * 1024 * 1024 * 1024,
    <<"no_sync">> => false,
    <<"map_async">> => false,
    <<"no_meta_sync">> => false
}.
% Every write is synced to disk
```

### Custom Configuration
```erlang
StoreOpts = #{
    <<"name">> => <<"mystore">>,
    <<"capacity">> => 10 * 1024 * 1024 * 1024,
    <<"no_sync">> => true,       % No fsync
    <<"map_async">> => false,     % But synchronous mmap
    <<"no_readahead">> => false   % Enable readahead
}.
```

### Read-Only Mode
```erlang
StoreOpts = #{
    <<"name">> => <<"mystore">>,
    <<"read_only">> => true
}.
% Opens existing database in read-only mode
```

## Performance Impact

Based on our benchmarks:

- **NO_SYNC enabled**: 67,109 ops/sec
- **NO_SYNC disabled**: 9,602 ops/sec
- **Speedup**: ~7x faster with NO_SYNC

## Recommendations

1. **For most applications**: Use default flags (all performance flags enabled)
2. **For financial/critical data**: Disable all sync flags
3. **For read-heavy workloads**: Consider enabling readahead
4. **For SSDs**: Default flags are optimal
5. **For HDDs**: May benefit from different flag combinations

## Implementation Details

The flags are parsed in `store.rs:parse_lmdb_flags()` and applied when creating the LMDB environment. Each flag defaults to its performance-optimal setting unless explicitly overridden.

WRITE_MAP is always enabled as it's required for good performance with memory-mapped I/O.