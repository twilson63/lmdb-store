# Benchmark Issue Analysis

## Problem
The benchmark test reports 115 keys not found out of 100,000 operations.

## Investigation Results

### 1. Direct Store Operations Work
When using hyper_lmdb directly:
- All write/read operations succeed
- Keys like "key-1", "key-100000" work correctly
- The store properly handles the @data: prefix

### 2. Byte-based Prefix Checking Fix
Fixed an issue where we were checking UTF-8 strings instead of raw bytes:
```rust
// Before: if let Ok(data_str) = std::str::from_utf8(data)
// After: if data.starts_with(b"@link:") or data.starts_with(b"@data:")
```

### 3. Key Generation Pattern
The benchmark generates keys correctly:
- Writes keys from "key-1" to "key-100000"
- Reads random keys in the same range
- No out-of-bounds keys should be generated

## Remaining Issues

The problem appears to be in how the benchmark test uses the store through the hb_store wrapper. Possible causes:

1. **Store Instance Mismatch**: The benchmark might be using different store instances for write and read
2. **Environment Lookup**: The canonical key computation might differ between operations
3. **Timing**: Even with sync, there might be a timing issue in the test
4. **Test Environment**: The test might be running with different store configurations

## Next Steps

1. Add logging to track which environment instance is used for each operation
2. Verify the benchmark test configuration
3. Check if the hb_store wrapper is properly passing the store opts