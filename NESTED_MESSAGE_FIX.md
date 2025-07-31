# Nested Message Link Resolution Issue

## Problem
The test `test_store_unsigned_nested_empty_message` is failing with:
```
error:{necessary_message_not_found,
    {link,<<"8KpWTo0eCwGy7Jc7zG3neAXp"...>>,
        #{<<"lazy">> => true,<<"type">> => <<"link">>}}}
```

This occurs when trying to read back a nested message structure through the cache layer.

## Analysis

### How Nested Messages are Written
1. When `hb_cache:write` is called with a nested map:
   - `do_write_message` is called for the map
   - It generates an UncommittedID using `hb_message:id`
   - Creates a group at that ID: `hb_store:make_group(Store, UncommittedID)`
   - For each key-value pair, calls `write_key` which:
     - Recursively writes the value using `do_write_message`
     - Creates a link from the returned path to the key's hashpath
   - Returns the UncommittedID

2. For nested values that are also maps:
   - The same process repeats recursively
   - Each nested map gets its own ID and group

### How Messages are Read Back
1. When reading a composite message:
   - The store detects it's a composite type
   - Lists all subpaths under the group
   - Calls `prepare_links` which creates lazy links for each subpath
   - These links have `<<"lazy">> => true` and include the store

2. When `ensure_loaded` encounters a lazy link:
   - It calls `hb_cache:read(ID, MergedOpts)` where ID is the link target
   - This should read the nested message and return it

### The Issue
The error suggests that when trying to read the linked message ID, it's not found. This could be because:

1. **Store parameter issue**: The store in the link options might not be in the correct format for hyper_lmdb
2. **ID format mismatch**: The message ID might not be in the expected format for the store
3. **Missing group marker**: The group might not be created correctly or might be missing the `__group__` marker

## Potential Fixes

### Option 1: Ensure Store Compatibility
The link includes `store => Store` but this might be an Erlang term that doesn't translate correctly to the Rust NIF. We might need to ensure the store reference is properly handled.

### Option 2: Fix Group Creation
Ensure that when `make_group` is called with a message ID, it creates a proper group that can be detected by `get_type`.

### Option 3: Fix Link Resolution
The resolve_path function might not be handling links to groups correctly. When a link points to a group, it should resolve to the group path.

## Recommended Investigation Steps

1. Add logging to see the exact ID being used
2. Check if the group is created correctly with the `__group__` marker
3. Verify that the store parameter is correctly passed through the link resolution
4. Test if the ID can be read directly without going through links

## Possible Implementation Fix

The issue might be that the store reference in the link is not being used correctly. In hb_cache.erl, when ensure_loaded reads a link, it should use the store from the link options, not from the general options.