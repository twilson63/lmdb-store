# Link Resolution Fix

## Problem
When nested messages are stored in hb_cache, links are created to message IDs. However, if those message IDs are themselves links or need resolution, the final read fails because the link points to an intermediate link rather than the final data.

## Solution
Updated the `make_link` function in `store.rs` to resolve the `existing` path before creating a link. This ensures that:

1. Links always point to the final resolved path
2. Chains of links are avoided
3. Nested message resolution works correctly

## Changes Made

```rust
// Before: 
let link_value = format!("@link:{}", existing_str);

// After:
let resolved_existing = resolve_path(&lmdb_env, &existing_str);
let link_value = format!("@link:{}", resolved_existing);
```

Also added:
- Call to `ensure_parent_groups` to ensure the parent directories exist for the new link

## Expected Behavior
When creating a link from path A to path B:
- If B is already a link to C, the new link will point directly to C
- This prevents link chains and ensures resolution always works
- Nested messages stored in hb_cache should now be readable

## Testing
The fix should resolve the `test_store_unsigned_nested_empty_message` test failure in hb_cache where nested messages were returning `{necessary_message_not_found, {link, ...}}` errors.