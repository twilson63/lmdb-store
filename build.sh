#!/bin/bash

# Build script for elmdb-rs

echo "Building elmdb-rs LMDB NIF..."

# Create directories
mkdir -p priv
mkdir -p ebin

# Build Rust NIF
echo "Building Rust NIF..."
cargo build --release

# Copy the library to priv/
if [[ "$OSTYPE" == "darwin"* ]]; then
    cp target/release/libelmdb_rs.dylib priv/elmdb_rs.so
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    cp target/release/libelmdb_rs.so priv/elmdb_rs.so
else
    echo "Unsupported OS type: $OSTYPE"
    exit 1
fi

# Compile Erlang modules
echo "Compiling Erlang modules..."
erlc -o ebin src/*.erl

echo "Build complete!"
echo "To run tests: make test"