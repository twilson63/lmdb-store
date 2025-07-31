# hyper_lmdb

A high-performance Erlang NIF implementation of LMDB (Lightning Memory-Mapped Database) written in Rust. This library provides a robust key-value store with hierarchical key organization, symbolic links, and built-in security features.

## Features

- **High Performance**: Zero-copy reads directly from memory-mapped files
- **ACID Transactions**: Full ACID compliance with MVCC (Multi-Version Concurrency Control)
- **Hierarchical Keys**: Support for groups (directories) and nested paths
- **Symbolic Links**: Create links between keys for aliasing and redirection
- **Security**: Built-in prefix system prevents link injection attacks
- **Thread Safety**: Safe concurrent access from multiple Erlang processes
- **Resource Management**: Automatic cleanup with Rust's ownership system
- **Backwards Compatible**: Supports reading legacy data without prefixes

## Installation

### Using Rebar3

Add `hyper_lmdb` to your `rebar.config` dependencies:

```erlang
{deps, [
    {hyper_lmdb, {git, "https://github.com/yourusername/elmdb-rs.git", {branch, "main"}}}
]}.
```

Then run:
```bash
rebar3 get-deps
rebar3 compile
```

### Manual Build

Prerequisites:
- Rust toolchain (1.70 or later)
- Erlang/OTP (24 or later)
- Make

```bash
# Clone the repository
git clone https://github.com/yourusername/elmdb-rs.git
cd elmdb-rs

# Build the NIF
make build

# Run tests
make test
```

## API Reference

### Store Management

#### `start(StoreOpts) -> {ok, EnvRef} | {error, Reason}`
Initialize a new LMDB store instance.

```erlang
StoreOpts = #{
    <<"name">> => <<"./data/mystore">>,    % Required: database path
    <<"capacity">> => 1073741824,          % Optional: max DB size in bytes (1GB)
    <<"max_dbs">> => 128,                  % Optional: max named databases
    <<"max_readers">> => 126               % Optional: max concurrent readers
}.
```

#### `stop(StoreOpts) -> ok`
Stop the store and release resources.

#### `reset(StoreOpts) -> ok | {error, Reason}`
Clear all data from the store (WARNING: destructive operation).

### Basic Operations

#### `write(StoreOpts, Key, Value) -> ok | not_found`
Write a key-value pair to the store.

```erlang
ok = hyper_lmdb:write(StoreOpts, <<"key">>, <<"value">>),
ok = hyper_lmdb:write(StoreOpts, [<<"users">>, <<"alice">>], <<"data">>).
```

#### `read(StoreOpts, Key) -> {ok, Value} | not_found`
Read a value from the store. Automatically resolves links.

```erlang
{ok, <<"value">>} = hyper_lmdb:read(StoreOpts, <<"key">>).
```

#### `type(StoreOpts, Key) -> {ok, Type} | not_found`
Get the type of a key. Returns: `simple`, `composite`, or `link`.

```erlang
{ok, simple} = hyper_lmdb:type(StoreOpts, <<"file">>),
{ok, composite} = hyper_lmdb:type(StoreOpts, <<"folder">>),
{ok, link} = hyper_lmdb:type(StoreOpts, <<"alias">>).
```

### Hierarchical Organization

#### `make_group(StoreOpts, Path) -> ok | not_found`
Create a group (directory) at the given path.

```erlang
ok = hyper_lmdb:make_group(StoreOpts, <<"config">>),
ok = hyper_lmdb:make_group(StoreOpts, [<<"config">>, <<"database">>]).
```

#### `list(StoreOpts, Path) -> {ok, Children} | not_found`
List all direct children of a group.

```erlang
{ok, Children} = hyper_lmdb:list(StoreOpts, <<"config">>).
% Children = [<<"database">>, <<"server">>, <<"logging">>]
```

### Symbolic Links

#### `make_link(StoreOpts, Target, LinkName) -> ok | not_found`
Create a symbolic link pointing to an existing key.

```erlang
% Create a link
ok = hyper_lmdb:write(StoreOpts, <<"config/prod/db">>, <<"prod.example.com">>),
ok = hyper_lmdb:make_link(StoreOpts, <<"config/prod/db">>, <<"current_db">>),

% Reading the link resolves to the target value
{ok, <<"prod.example.com">>} = hyper_lmdb:read(StoreOpts, <<"current_db">>).
```

### Path Operations

#### `path(StoreOpts, Path) -> BinaryPath`
Normalize a path to its canonical form.

```erlang
<<"users/alice">> = hyper_lmdb:path(StoreOpts, [<<"users">>, <<"alice">>]).
```

#### `add_path(StoreOpts, Path1, Path2) -> CombinedPath`
Combine two paths.

```erlang
<<"users/alice/settings">> = hyper_lmdb:add_path(StoreOpts, <<"users/alice">>, <<"settings">>).
```

### Advanced Operations

#### `list_prefix(StoreOpts, Prefix) -> {ok, Keys}`
#### `list_prefix(StoreOpts, Prefix, Options) -> {ok, Keys} | {ok, Keys, Cursor}`
List all keys with a given prefix, with optional pagination.

```erlang
% Simple prefix listing
{ok, Keys} = hyper_lmdb:list_prefix(StoreOpts, <<"users/">>),

% With pagination
Options = #{
    <<"limit">> => 100,              % Max keys to return
    <<"cursor">> => PreviousCursor,  % Continue from previous query
    <<"return_cursor">> => true      % Return cursor for next page
},
{ok, Keys, NextCursor} = hyper_lmdb:list_prefix(StoreOpts, <<"users/">>, Options).
```

#### `scope(StoreOpts) -> local`
Returns the scope of the store (always `local` for LMDB).

## Usage Examples

### Basic Key-Value Operations

```erlang
% Initialize store
StoreOpts = #{
    <<"name">> => <<"./data/myapp">>
},
{ok, _} = hyper_lmdb:start(StoreOpts),

% Write and read data
ok = hyper_lmdb:write(StoreOpts, <<"version">>, <<"1.0.0">>),
{ok, <<"1.0.0">>} = hyper_lmdb:read(StoreOpts, <<"version">>),

% Clean up
hyper_lmdb:stop(StoreOpts).
```

### Working with Hierarchical Data

```erlang
% Create organizational structure
ok = hyper_lmdb:make_group(StoreOpts, <<"users">>),
ok = hyper_lmdb:make_group(StoreOpts, <<"users/admins">>),

% Store user data
ok = hyper_lmdb:write(StoreOpts, <<"users/alice/email">>, <<"alice@example.com">>),
ok = hyper_lmdb:write(StoreOpts, <<"users/alice/role">>, <<"developer">>),
ok = hyper_lmdb:write(StoreOpts, <<"users/admins/bob/email">>, <<"bob@example.com">>),

% List all users
{ok, Users} = hyper_lmdb:list(StoreOpts, <<"users">>),
% Users = [<<"alice">>, <<"admins">>]

% List admin users
{ok, Admins} = hyper_lmdb:list(StoreOpts, <<"users/admins">>),
% Admins = [<<"bob">>]
```

### Using Symbolic Links

```erlang
% Create environment-specific configurations
ok = hyper_lmdb:write(StoreOpts, <<"config/dev/api_url">>, <<"http://localhost:8080">>),
ok = hyper_lmdb:write(StoreOpts, <<"config/prod/api_url">>, <<"https://api.example.com">>),

% Create a link to current environment
ok = hyper_lmdb:make_link(StoreOpts, <<"config/dev/api_url">>, <<"current_api">>),

% Application reads from the link
{ok, <<"http://localhost:8080">>} = hyper_lmdb:read(StoreOpts, <<"current_api">>),

% Switch to production by updating the link
ok = hyper_lmdb:make_link(StoreOpts, <<"config/prod/api_url">>, <<"current_api">>),
{ok, <<"https://api.example.com">>} = hyper_lmdb:read(StoreOpts, <<"current_api">>).
```

### Link Chains and Resolution

```erlang
% Create a chain of links
ok = hyper_lmdb:write(StoreOpts, <<"data/master">>, <<"primary data">>),
ok = hyper_lmdb:make_link(StoreOpts, <<"data/master">>, <<"data/current">>),
ok = hyper_lmdb:make_link(StoreOpts, <<"data/current">>, <<"data/active">>),
ok = hyper_lmdb:make_link(StoreOpts, <<"data/active">>, <<"data/latest">>),

% Reading any link in the chain resolves to the original value
{ok, <<"primary data">>} = hyper_lmdb:read(StoreOpts, <<"data/latest">>).
```

### Recursive Path Resolution

The store supports recursive link resolution for path segments, enabling powerful organizational patterns:

```erlang
% Example: Creating environment-specific directory structures with links

% Set up base configuration
ok = hyper_lmdb:write(StoreOpts, <<"configs/production">>, <<"prod_config">>),
ok = hyper_lmdb:write(StoreOpts, <<"configs/production/database/host">>, <<"db.prod.example.com">>),
ok = hyper_lmdb:write(StoreOpts, <<"configs/production/database/port">>, <<"5432">>),

% Create a link for current environment
ok = hyper_lmdb:make_link(StoreOpts, <<"configs/production">>, <<"current_env">>),

% Access nested values through the link
{ok, <<"prod_config">>} = hyper_lmdb:read(StoreOpts, <<"current_env">>),
{ok, <<"db.prod.example.com">>} = hyper_lmdb:read(StoreOpts, <<"current_env/database/host">>),
{ok, <<"5432">>} = hyper_lmdb:read(StoreOpts, <<"current_env/database/port">>).

% Links are resolved at each path segment level
% If "foo" links to "bar", then "foo/baz" attempts to resolve to "bar/baz"
```

**Note**: For path resolution to work correctly, the link target must exist as a value in the database. Links to non-existent keys or to keys that only exist as groups (directories) will result in `not_found` when attempting to resolve paths through them.

## Security Features

### Link Injection Prevention

The store uses a prefix system to prevent malicious data from being interpreted as links:

```erlang
% Attempting to store a value that looks like a link
ok = hyper_lmdb:write(StoreOpts, <<"malicious">>, <<"@link:secret_key">>),

% The value is stored safely and returned as-is
{ok, <<"@link:secret_key">>} = hyper_lmdb:read(StoreOpts, <<"malicious">>),

% It's not treated as a link
{ok, simple} = hyper_lmdb:type(StoreOpts, <<"malicious">>).
```

## Performance Characteristics

- **Write Performance**: >100,000 ops/sec for small values
- **Read Performance**: >500,000 ops/sec (zero-copy from mmap)
- **Memory Efficiency**: Uses OS page cache, no double buffering
- **Scalability**: Linear scaling with CPU cores for reads

## Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `<<"name">>` | binary | required | Database path (LMDB database directory) |
| `<<"capacity">>` | integer | 10737418240 (10GB) | Maximum database size in bytes |
| `<<"max_dbs">>` | integer | 128 | Maximum number of named databases |
| `<<"max_readers">>` | integer | 126 | Maximum concurrent read transactions |

**Note**: For backwards compatibility, `<<"map_size">>` can be used instead of `<<"capacity">>`.

## Error Handling

All functions return either:
- `ok` or `{ok, Result}` on success
- `not_found` when a key doesn't exist
- `{error, Reason}` for other errors

## Testing

Run the complete test suite:
```bash
make test
```

Run specific test modules:
```bash
erl -pa ebin -pa test -noshell -eval "eunit:test(hyper_lmdb_test, [verbose]), init:stop()."
erl -pa ebin -pa test -noshell -eval "eunit:test(hyper_lmdb_link_test, [verbose]), init:stop()."
erl -pa ebin -pa test -noshell -eval "eunit:test(hyper_lmdb_prefix_test, [verbose]), init:stop()."
```

## License

This project is licensed under the same terms as HyperBEAM.

## Contributing

Contributions are welcome! Please ensure all tests pass and add new tests for any new functionality.