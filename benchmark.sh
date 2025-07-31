#!/bin/bash

# ELMDB Benchmark Runner Script

# Default values
NUM_OPS=100000
KEY_SIZE=32
VALUE_SIZE=1024
NUM_READERS=4
NUM_WRITERS=2
BATCH_SIZE=1000

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --ops)
            NUM_OPS="$2"
            shift 2
            ;;
        --key-size)
            KEY_SIZE="$2"
            shift 2
            ;;
        --value-size)
            VALUE_SIZE="$2"
            shift 2
            ;;
        --readers)
            NUM_READERS="$2"
            shift 2
            ;;
        --writers)
            NUM_WRITERS="$2"
            shift 2
            ;;
        --batch-size)
            BATCH_SIZE="$2"
            shift 2
            ;;
        --quick)
            NUM_OPS=10000
            shift
            ;;
        --help)
            echo "Usage: $0 [options]"
            echo "Options:"
            echo "  --ops N          Number of operations (default: 100000)"
            echo "  --key-size N     Key size in bytes (default: 32)"
            echo "  --value-size N   Value size in bytes (default: 1024)"
            echo "  --readers N      Number of concurrent readers (default: 4)"
            echo "  --writers N      Number of concurrent writers (default: 2)"
            echo "  --batch-size N   Batch size for batch operations (default: 1000)"
            echo "  --quick          Run quick benchmark (10k operations)"
            echo "  --help           Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Build the project if needed
echo "Building ELMDB..."
make build > /dev/null 2>&1 || {
    echo "Build failed!"
    exit 1
}

# Compile the benchmark module
echo "Compiling benchmark module..."
erlc -o test test/elmdb_benchmark.erl || {
    echo "Failed to compile benchmark module!"
    exit 1
}

# Run the benchmark
echo "Running benchmarks with:"
echo "  Operations: $NUM_OPS"
echo "  Key size: $KEY_SIZE bytes"
echo "  Value size: $VALUE_SIZE bytes"
echo "  Readers: $NUM_READERS"
echo "  Writers: $NUM_WRITERS"
echo "  Batch size: $BATCH_SIZE"
echo ""

erl -pa ebin -pa test -noshell -eval "
    Config = [
        {num_operations, $NUM_OPS},
        {key_size, $KEY_SIZE},
        {value_size, $VALUE_SIZE},
        {num_readers, $NUM_READERS},
        {num_writers, $NUM_WRITERS},
        {batch_size, $BATCH_SIZE}
    ],
    elmdb_benchmark:run(Config),
    init:stop().
" || {
    echo "Benchmark failed!"
    exit 1
}

# Clean up
rm -f test/elmdb_benchmark.beam