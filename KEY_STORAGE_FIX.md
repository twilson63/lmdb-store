# Key Storage Fix for Nested Messages

## Problem
When hb_cache writes a nested message:
1. It creates a group for the message ID
2. For each key, it calculates a hashpath
3. It writes the value and creates a link from the value to the hashpath
4. But it doesn't create any entry directly under the message ID

When listing the message, hyper_lmdb returns an empty list because there are no direct children under the message ID.

## How hb_store_lmdb Works
Looking at hb_store_lmdb, it stores keys directly. If you write to `path/key`, it's stored at exactly that path and can be listed.

## The Fix
The issue is that hb_cache expects to list keys directly under a message, but they're only stored at hashpaths. We need to modify how hb_cache interacts with hyper_lmdb.

The problem is in the hb_store adapter layer. The hyper_lmdb store needs to handle the fact that hb_cache uses hashpaths but still expects to be able to list keys.

## Possible Solutions

### Option 1: Store Keys Directly
Modify hb_cache to also store keys directly under the message, not just at hashpaths.

### Option 2: Fix in hyper_lmdb
When writing a key under a group, also store a reference at the direct path.

### Option 3: Use hb_store_fs Pattern
Look at how hb_store_fs handles this - it might have a different approach.