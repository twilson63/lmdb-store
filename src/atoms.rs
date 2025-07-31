use rustler::atoms;

atoms! {
    ok,
    error,
    not_found,
    retry,
    
    // Store types
    simple,
    composite,
    link,
    
    // Error types
    invalid_path,
    invalid_value,
    db_full,
    readers_full,
    txn_full,
    cursor_full,
    page_full,
    corrupted,
    panic,
    version_mismatch,
    invalid,
    access_denied,
    
    // Store options
    store_module,
    name,
    path,
    map_size,
    max_dbs,
    max_readers,
    
    // Internal atoms
    true_atom = "true",
    false_atom = "false",
    nil,
}