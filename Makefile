.PHONY: all build clean test

PRIV_DIR = priv
NIF_SO = $(PRIV_DIR)/elmdb_rs.so

all: build

build: $(NIF_SO)
	@echo "Build complete"

$(NIF_SO): src/*.rs Cargo.toml
	@mkdir -p $(PRIV_DIR)
	cargo build --release
	@cp target/release/libelmdb_rs.dylib $(NIF_SO) 2>/dev/null || \
	 cp target/release/libelmdb_rs.so $(NIF_SO) 2>/dev/null || \
	 echo "Failed to copy NIF library"

clean:
	cargo clean
	rm -rf $(PRIV_DIR)
	rm -rf lmdb

test: build
	@echo "Compiling test files..."
	@erlc -o test test/hyper_lmdb_test.erl
	@echo "Running tests..."
	erl -pa ebin -pa test -noshell -eval "eunit:test(hyper_lmdb_test, [verbose]), init:stop()."

perf: build
	@echo "Running performance tests..."
	@erlc -o test test/elmdb_perf_test.erl
	erl -pa ebin -pa test -noshell -eval "eunit:test(elmdb_perf_test, [verbose]), init:stop()."

benchmark: build
	@./benchmark.sh

benchmark-quick: build
	@./benchmark.sh --quick