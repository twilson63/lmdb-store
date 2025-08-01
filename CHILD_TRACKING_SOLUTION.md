# Child Tracking Solution for hyper_lmdb

## The Problem

When hb_cache writes nested messages:
1. It calculates a hashpath like `msgID/hash(msgID+"layer1+link")`
2. It creates a link from the nested message ID to this hashpath
3. When listing the message, it expects to see "layer1+link" as a child
4. But hyper_lmdb only has the hashpath, not the original key name

## The Solution

We need to create a bidirectional mapping between original keys and hashpaths. When hb_cache creates a link at a hashpath, we need to also store metadata that maps back to the original key name.

### Implementation Plan

1. When `make_link` is called with a path like `msgID/hashpath`:
   - Store the link as usual
   - Also create a metadata entry that helps listing return the original key

2. When `list` is called on a message:
   - Look for both direct children and hashpath children
   - For hashpath children, check if they're links
   - Return the appropriate key names

### Key Insight

The issue is that hb_cache uses hashpaths for storage but expects original key names for listing. Other stores like hb_store_lru handle this by tracking child keys explicitly. We need a similar mechanism in hyper_lmdb.