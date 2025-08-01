# Compatibility Fix for hb_cache Nested Messages

## The Core Issue

When hb_cache stores nested messages, it:
1. Takes a key like "layer1" with nested content
2. Adds "+link" suffix during normalization
3. Calculates hashpath = hash(msgID + "layer1+link")
4. Creates a link from the nested message ID to the hashpath
5. Expects "layer1+link" to appear when listing the parent message

The problem: Only the hashpath exists in the store, not "layer1+link".

## Why Other Stores Work

- **hb_store_fs**: Uses filesystem, so whatever files exist are listed
- **hb_store_lmdb**: Stores keys directly at their paths
- **hb_store_lru**: Tracks children in sets

All of these would list the hashpath, not the original key name. So there must be something else happening.

## The Real Issue

Looking more carefully at the test failure, the issue might be that hb_cache is creating additional entries we're not aware of. Or there's a transformation happening during the listing process.

## Proposed Solution

Since we need to maintain compatibility without modifying hb_cache, we need to:

1. Track all direct children of groups (both regular keys and links)
2. When listing, return all tracked children
3. Ensure that keys with "+link" suffix are properly handled

The challenge is that we don't have enough information at the hyper_lmdb level to reconstruct the original key names from hashpaths.