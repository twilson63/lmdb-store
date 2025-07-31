# HyperBEAM Store (hb_store_lmdb) Test Suite for hyper_lmdb

## Overview

This directory contains test files that verify `hyper_lmdb` correctly implements all the functionality originally implemented in `hb_store_lmdb.erl`. The test suite extracts and adapts all test cases from the original module to ensure compatibility.

## Test Files

### `hyper_lmdb_hb_store_test.erl`
The main test module containing all test cases extracted from `hb_store_lmdb.erl`:

1. **basic_test** - Verifies fundamental read/write functionality
2. **list_test** - Tests hierarchical listing with prefix matching
3. **group_test** - Tests group creation and type detection
4. **link_test** - Tests symbolic link creation and resolution
5. **link_fragment_test** - Tests link resolution in path fragments
6. **type_test** - Tests type detection for simple and composite entries
7. **link_key_list_test** - Tests symbolic links with structured key paths
8. **path_traversal_link_test** - Tests link resolution during path traversal
9. **exact_hb_store_test** - Tests exact hb_store hierarchical pattern
10. **nested_map_cache_test** - Tests nested map storage with cache-like behavior
11. **list_with_link_test** - Tests list function link resolution
12. **scope_test** - Tests scope functions
13. **path_operations_test** - Tests path and add_path operations
14. **resolve_test** - Tests resolve function

### Running the Tests

The hb_store tests are fully integrated into the project's test suite and run automatically with:

1. **Using make**:
   ```bash
   make test
   ```

2. **Using rebar3**:
   ```bash
   rebar3 eunit
   ```

The tests are included in the standard test configuration and will run alongside all other project tests.

## Test Results

All 14 tests pass successfully, confirming that `hyper_lmdb` correctly implements all the functionality from `hb_store_lmdb`.

## Key API Differences

When migrating from `hb_store_lmdb` to `hyper_lmdb`, note these API differences:

1. **Module Initialization**: `hyper_lmdb` requires calling `start/1` before operations
2. **Type Function**: Returns atoms directly (`composite`, `simple`, `not_found`) instead of `{ok, Type}`
3. **Resolve Function**: Returns the resolved path directly or `not_found`, not `{ok, Path}`
4. **Scope Function**: Takes a parameter `scope/1` instead of being parameterless `scope/0`

## Implementation Notes

- All tests create temporary databases in `/tmp` and clean up after themselves
- The tests verify complex scenarios including:
  - Multi-level link resolution
  - Path traversal through symbolic links
  - Hierarchical group structures
  - Cache-style content-addressed storage patterns
  - Circular link detection
- Tests pass with the current implementation, demonstrating full compatibility with the original HyperBEAM store interface