#!/bin/bash

# Build script for hyper_lmdb

echo "Building hyper_lmdb NIF..."

# Create directories
mkdir -p priv
mkdir -p ebin

# Build Rust NIF
echo "Building Rust NIF..."
cargo build --release

# Copy the library to priv/
if [[ "$OSTYPE" == "darwin"* ]]; then
    cp target/release/libhyper_lmdb.dylib priv/hyper_lmdb.so
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    cp target/release/libhyper_lmdb.so priv/hyper_lmdb.so
else
    echo "Unsupported OS type: $OSTYPE"
    exit 1
fi

# Compile Erlang modules
echo "Compiling Erlang modules..."
erlc -o ebin src/*.erl

echo "Build complete!"
echo "To run tests: make test"