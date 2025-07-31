# Security Prefix Implementation

## Overview
Implemented security prefixes to prevent injection attacks where user data could be crafted to look like internal link markers.

## Implementation Details

### 1. Value Prefixes
- **Regular data**: Stored with `@data:` prefix
- **Links**: Stored with `@link:` prefix
- **Legacy support**: Data without prefix is treated as regular data (backwards compatibility)

### 2. Write Function (src/store.rs:248)
```rust
// Prefix the value with @data: to distinguish from links
let prefixed_value = [b"@data:", value_bin.as_slice()].concat();
```

### 3. Read Function (src/store.rs:193-209)
```rust
if let Ok(data_str) = std::str::from_utf8(data) {
    if data_str.starts_with("@link:") {
        // This is a link that couldn't be resolved
        return Ok(atoms::not_found().encode(env));
    } else if data_str.starts_with("@data:") {
        // Regular data - strip the prefix
        let actual_data = &data[6..]; // Skip "@data:"
        let mut binary = rustler::OwnedBinary::new(actual_data.len()).unwrap();
        binary.as_mut_slice().copy_from_slice(actual_data);
        return Ok((atoms::ok(), binary.release(env)).encode(env));
    }
}
// Legacy data without prefix (for backwards compatibility)
```

### 4. Type Detection (src/store.rs:287-295)
```rust
if value_str.starts_with("@link:") {
    return Ok((atoms::ok(), atoms::link()).encode(env));
} else if value_str.starts_with("@data:") {
    return Ok((atoms::ok(), atoms::simple()).encode(env));
}
// Legacy data without prefix
return Ok((atoms::ok(), atoms::simple()).encode(env));
```

### 5. Link Creation (src/store.rs:427)
```rust
let link_value = format!("@link:{}", existing_str);
```

## Security Benefits

1. **Injection Prevention**: User data containing "@link:" will be stored as "@data:@link:..." and won't be interpreted as a link
2. **Clear Type Distinction**: Values are explicitly marked as either data or links
3. **Backwards Compatibility**: Existing data without prefixes continues to work

## Testing

Created test module `hyper_lmdb_prefix_test.erl` that verifies:
1. Data prefix is transparent to users
2. Values containing "@link:" are not treated as links
3. Links still work correctly with prefixes
4. Legacy data compatibility

## Example

```erlang
% Writing a malicious value
ok = hyper_lmdb:write(Store, <<"key">>, <<"@link:secret">>),

% Reading it back returns the exact value (not following it as a link)
{ok, <<"@link:secret">>} = hyper_lmdb:read(Store, <<"key">>).
```

The value is internally stored as `@data:@link:secret` preventing any link resolution.