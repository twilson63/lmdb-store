# Group Children Tracking Fix

## The Problem
When hb_cache writes a nested message:
1. It creates a group at the message ID
2. For each key, it calculates a hashpath and creates a link
3. But it doesn't track that the key exists under the group

When listing the group, there are no children to return because they're not tracked.

## How Other Stores Handle It

### hb_store_lru
- Maintains a set of child keys for each group
- When writing `group/key`, it adds "key" to the group's set
- When listing, it returns the keys from this set

### hb_store_lmdb
- Stores keys directly under their paths
- When listing, it finds all keys with the prefix

### hb_store_fs
- Uses the filesystem, so directories naturally contain their children

## The Fix for hyper_lmdb

We need to track child keys for each group. Options:

1. **Store children in a special key** (e.g., `group/__children__`)
2. **Store each child as a marker** (e.g., `group/child` => special marker)
3. **Modify write to create direct entries**

I think option 2 is best - when writing a key under a group, also create a marker entry at the direct path that can be listed.