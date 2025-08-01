# Multi-Transaction Atomicity Issue

## Problem

When hb_cache writes a message, it performs multiple LMDB transactions:
1. Create group (one transaction)
2. Write each field (multiple transactions)
3. Create links (multiple transactions)

Each operation uses its own transaction that commits independently. This means:
- No atomicity across the full message write
- Some parts may be visible while others are not
- Even with proper sync, there's a race condition

## Why Simple Keys Work

The key benchmark test works because each key write is a single transaction:
```
write(Store, "key-1", "value") -> single txn -> commit
```

## Why Messages Fail

Message writes involve multiple coordinated operations:
```
hb_cache:write(Message) ->
  1. make_group(message_id) -> txn1 -> commit
  2. write(message_id/field1, value1) -> txn2 -> commit
  3. write(message_id/field2, value2) -> txn3 -> commit
  4. make_link(path1, path2) -> txn4 -> commit
  ...
```

## Solutions

### 1. Short-term: Increase Sleep Time
Already using 5000ms, but may need more for large batches.

### 2. Medium-term: Batch Transaction Support
Implement a way to batch multiple operations in a single transaction:
```rust
pub fn batch_operations(ops: Vec<Operation>) -> Result<()> {
    let mut txn = env.begin_rw_txn()?;
    for op in ops {
        match op {
            Write(key, value) => txn.put(db, key, value)?,
            MakeGroup(path) => // handle group creation
            MakeLink(from, to) => // handle link creation
        }
    }
    txn.commit()?;
}
```

### 3. Long-term: Transaction Context API
Allow Erlang side to manage transaction boundaries:
```erlang
{ok, Txn} = hyper_lmdb:begin_transaction(Store),
ok = hyper_lmdb:write(Txn, Key1, Value1),
ok = hyper_lmdb:write(Txn, Key2, Value2),
ok = hyper_lmdb:make_link(Txn, Path1, Path2),
ok = hyper_lmdb:commit(Txn).
```

## Current Impact

The benchmark test with 115 missing keys is likely seeing this issue where:
- Some message fields are written but not all
- Links may not be created yet
- Group metadata may be incomplete

This explains why increasing sleep time helps but doesn't fully solve the problem.