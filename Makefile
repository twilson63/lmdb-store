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
	@echo "Running tests..."
	erl -pa ebin -pa test -noshell -eval "eunit:test(hb_store_lmdb_test, [verbose]), init:stop()."