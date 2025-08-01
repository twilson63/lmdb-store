# Final Fix Plan for Nested Message Storage

## The Root Cause

When hb_cache stores nested messages:
1. It calculates hashpaths for keys ending with "+link"
2. It creates links from nested message IDs to these hashpaths
3. When listing, it expects to see the original key names (with +link suffix)
4. But only the hashpaths exist in the store, not the original key names

## The Solution

We need to create a reverse mapping when links are created at hashpaths. When hb_cache creates a link at a hashpath under a message group, we should also create an entry at the original key path that points to the hashpath.

## Implementation

The fix needs to be in hb_cache, not hyper_lmdb. When hb_cache writes a key:

```erlang
write_key(Base, Key, HPAlg, Value, Store, Opts) ->
    KeyHashPath = hb_path:hashpath(Base, hb_path:to_binary(Key), HPAlg, Opts),
    {ok, Path} = do_write_message(Value, Store, Opts),
    hb_store:make_link(Store, Path, KeyHashPath),
    % NEW: Also create a link from the hashpath to the direct key path
    % This allows listing to find the original key name
    DirectKeyPath = <<Base/binary, "/", Key/binary>>,
    hb_store:make_link(Store, KeyHashPath, DirectKeyPath),
    {ok, Path}.
```

This would create a bidirectional link structure that allows both:
1. Reading via hashpath (original functionality)
2. Listing with original key names (fixes the issue)

## Alternative Solution in hyper_lmdb

If we can't modify hb_cache, we could implement a workaround in hyper_lmdb by detecting hashpath patterns and creating metadata entries. However, this would be less clean and more fragile.