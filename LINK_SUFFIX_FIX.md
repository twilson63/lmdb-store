# Link Suffix Fix Plan

## Problem
The hb_cache expects that when listing a composite message, keys that are links should have a `+link` suffix. When reading a path with `+link` suffix, it should return the link data.

Currently, hyper_lmdb doesn't implement this convention, causing the nested message test to fail.

## How hb_store_fs handles it
- When listing a directory, it checks each entry
- If an entry is a symlink, it adds `+link` to the name
- When reading a path with `+link`, it reads the symlink target

## How hyper_lmdb should handle it
1. **During write_key in hb_cache**:
   - A hashpath is calculated for the key
   - The value is written (getting a path)
   - A link is created from the value path to the hashpath
   - **We need to also mark that this key is a link**

2. **During list**:
   - Check each key to see if it's a link
   - If it is, add `+link` suffix to the returned name

3. **During read/resolve**:
   - If path ends with `+link`, strip it and look for the link

## Implementation approach
We need to store metadata about which keys are links. Options:
1. Store a special marker for each link key
2. Check if the key's value starts with `@link:`
3. Use a naming convention in the database

The simplest approach is to check if the value at a key starts with `@link:` during listing.