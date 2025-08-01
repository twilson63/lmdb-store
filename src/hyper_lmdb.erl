%%% @doc LMDB-based implementation of the hb_store behavior.
%%%
%%% This module provides a high-performance key-value store backed by LMDB,
%%% implemented as a Rust NIF for optimal performance and safety.
%%%
%%% The store supports:
%%% - Hierarchical key organization with groups
%%% - Symbolic links between keys
%%% - Atomic transactions
%%% - Efficient range queries and prefix matching
%%% - Zero-copy reads directly from memory-mapped files
%%%
%%% Configuration options:
%%% - <<"name">> - Store instance name (required)
%%% - <<"path">> - Directory path for LMDB files (default: "./lmdb/{name}")
%%% - <<"map_size">> or <<"capacity">> - Maximum database size in bytes (default: 10GB)
%%% - <<"max_dbs">> - Maximum number of named databases (default: 128)
%%% - <<"max_readers">> - Maximum concurrent readers (default: 126)
%%%
%%% Performance flags (all default to true for best performance):
%%% - <<"no_sync">> - Don't sync after each transaction (default: true)
%%%                   Set to false for maximum durability
%%% - <<"map_async">> - Use asynchronous memory-mapped writes (default: true)
%%% - <<"no_meta_sync">> - Don't sync metadata (default: true)
%%% - <<"no_readahead">> - Disable OS readahead (default: true)
%%% - <<"no_tls">> - Disable thread-local storage (default: false)
%%% - <<"no_lock">> - Disable locking (single process only) (default: false)
%%% - <<"read_only">> - Open in read-only mode (default: false)
%%%
%%% Example with custom flags for maximum durability:
%%% #{<<"name">> => <<"mystore">>,
%%%   <<"no_sync">> => false,       % Force sync on every write
%%%   <<"map_async">> => false,      % Synchronous writes
%%%   <<"no_meta_sync">> => false}   % Sync metadata too

-module(hyper_lmdb).
% -behavior(hb_store).

%% hb_store callbacks
-export([start/1, stop/1, reset/1]).
-export([type/2, read/2, write/3, list/2]).
-export([make_group/2, make_link/3]).
-export([path/2, add_path/3, resolve/2]).

%% Extended API
-export([list_prefix/2, list_prefix/3, read_many/2]).
-export([scope/1, sync/1]).

%% Fast path API (no link resolution, no parent group creation)
-export([read_fast/2, write_fast/3, read_many_fast/2]).

%% Batch operations
-export([begin_batch/1, batch_write/3, batch_make_group/2, batch_make_link/3, commit_batch/1]).

%% Direct LMDB environment functions removed - use store API instead

%% Initialization
-export([init/0]).

%% NIF functions
-on_load(load_nif/0).

-define(APPNAME, hyper_lmdb).
-define(LIBNAME, hyper_lmdb).

%%% NIF loading

load_nif() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            % In development, look for the priv directory relative to current dir
            case {filelib:is_file("priv/hyper_lmdb.so"), filelib:is_file("priv/hyper_lmdb.dylib")} of
                {true, _} -> "priv/hyper_lmdb";
                {_, true} -> "priv/hyper_lmdb";
                _ ->
                    case filelib:is_dir(filename:join(["..", priv])) of
                        true ->
                            filename:join(["..", priv, ?LIBNAME]);
                        _ ->
                            filename:join([priv, ?LIBNAME])
                    end
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

%%% Public API

%% @doc Initialize the NIF (called automatically on module load).
init() ->
    ok.

%%% hb_store behavior implementation

%% @doc Initialize the LMDB store with the given options.
start(StoreOpts) ->
    nif_start(StoreOpts).

%% @doc Stop the LMDB store and clean up resources.
stop(StoreOpts) ->
    nif_stop(StoreOpts).

%% @doc Reset the store to its initial empty state.
%% WARNING: This will delete all data in the store!
reset(StoreOpts) ->
    nif_reset(StoreOpts).

%% @doc Return the scope of this store (local disk storage).
scope(_StoreOpts) ->
    local.

%% @doc Get the type of value at the given key.
%% Returns: simple | composite | not_found
type(StoreOpts, Key) ->
    case nif_type(StoreOpts, normalize_key(Key)) of
        {ok, simple} -> simple;
        {ok, composite} -> composite;
        not_found -> not_found;
        {error, _} -> not_found
    end.

%% @doc Read a value from the store.
read(StoreOpts, Key) ->
    case nif_read(StoreOpts, normalize_key(Key)) of
        {ok, Value} -> {ok, Value};
        not_found -> not_found;
        {error, _} -> not_found
    end.

%% @doc Write a key-value pair to the store.
write(StoreOpts, Key, Value) when is_binary(Value) ->
    case nif_write(StoreOpts, normalize_key(Key), Value) of
        ok -> ok;
        {error, _} -> not_found
    end;
write(StoreOpts, Key, Value) ->
    % Convert non-binary values to binary
    BinValue = term_to_binary(Value),
    write(StoreOpts, Key, BinValue).

%% @doc List all child keys in a group.
list(StoreOpts, Path) ->
    case nif_list(StoreOpts, normalize_key(Path)) of
        {ok, Children} -> {ok, Children};
        not_found -> not_found;
        {error, _} -> not_found
    end.

%% @doc Create a group (directory-like container) at the given path.
make_group(StoreOpts, Path) ->
    case nif_make_group(StoreOpts, normalize_key(Path)) of
        ok -> ok;
        {error, _} -> not_found
    end.

%% @doc Create a symbolic link from an existing key to a new key.
make_link(StoreOpts, Existing, New) ->
    case nif_make_link(StoreOpts, normalize_key(Existing), normalize_key(New)) of
        ok -> ok;
        {error, _} -> not_found
    end.

%% @doc Transform a path into canonical form.
%% For hyper_lmdb, this resolves links and returns the canonical path.
%% Returns not_found if the path doesn't exist.
path(StoreOpts, Path) ->
    nif_path(StoreOpts, normalize_key(Path)).

%% @doc Add two path components together.
%% For hyper_lmdb, this concatenates paths with "/" separator.
add_path(StoreOpts, Path1, Path2) ->
    nif_add_path(StoreOpts, normalize_key(Path1), normalize_key(Path2)).

%% @doc List all keys with a given prefix using range cursors.
list_prefix(StoreOpts, Prefix) ->
    list_prefix(StoreOpts, Prefix, #{}).

%% @doc List all keys with a given prefix with options.
%% Options:
%%   - limit: Maximum number of keys to return
%%   - cursor: Continuation cursor from previous call
%%   - return_cursor: Return a cursor for pagination
list_prefix(StoreOpts, Prefix, Opts) ->
    nif_list_prefix(StoreOpts, normalize_key(Prefix), Opts).

%% @doc Resolve a path by following any symbolic links.
%% This function resolves link chains in paths, similar to filesystem symlink resolution.
%% @param StoreOpts Database configuration map
%% @param Path The path to resolve (binary or list)
%% @returns The resolved path as a binary
-spec resolve(map(), binary() | list()) -> binary() | not_found.
resolve(StoreOpts, Path) ->
    case nif_resolve(StoreOpts, normalize_key(Path)) of
        {ok, ResolvedPath} -> ResolvedPath;
        not_found -> not_found;
        {error, _} -> not_found
    end.

%% @doc Force a sync of the LMDB environment to disk.
%% This ensures all pending transactions are fully persisted.
%% @param StoreOpts Database configuration map
%% @returns ok | {error, Reason}
-spec sync(map()) -> ok | {error, any()}.
sync(StoreOpts) ->
    case nif_sync(StoreOpts) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%%% Batch operations for atomic multi-operation transactions

%% @doc Begin a new batch transaction.
%% All operations within the batch will be executed atomically.
%% @param StoreOpts Database configuration map
%% @returns {ok, BatchRef} | {error, Reason}
-spec begin_batch(map()) -> {ok, reference()} | {error, any()}.
begin_batch(StoreOpts) ->
    nif_begin_batch(StoreOpts).

%% @doc Add a write operation to a batch.
%% @param BatchRef Batch transaction reference
%% @param Key The key to write
%% @param Value The value to write
%% @returns ok | {error, Reason}
-spec batch_write(reference(), binary() | list(), binary()) -> ok | {error, any()}.
batch_write(BatchRef, Key, Value) when is_binary(Value) ->
    case nif_batch_write(BatchRef, normalize_key(Key), Value) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end;
batch_write(BatchRef, Key, Value) ->
    batch_write(BatchRef, Key, term_to_binary(Value)).

%% @doc Add a make_group operation to a batch.
%% @param BatchRef Batch transaction reference
%% @param Path The group path to create
%% @returns ok | {error, Reason}
-spec batch_make_group(reference(), binary() | list()) -> ok | {error, any()}.
batch_make_group(BatchRef, Path) ->
    case nif_batch_make_group(BatchRef, normalize_key(Path)) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Add a make_link operation to a batch.
%% @param BatchRef Batch transaction reference
%% @param Existing The existing path to link from
%% @param New The new link path
%% @returns ok | {error, Reason}
-spec batch_make_link(reference(), binary() | list(), binary() | list()) -> ok | {error, any()}.
batch_make_link(BatchRef, Existing, New) ->
    case nif_batch_make_link(BatchRef, normalize_key(Existing), normalize_key(New)) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Commit a batch transaction.
%% Executes all operations in the batch atomically.
%% @param BatchRef Batch transaction reference
%% @returns ok | {error, Reason}
-spec commit_batch(reference()) -> ok | {error, any()}.
commit_batch(BatchRef) ->
    case nif_commit_batch(BatchRef) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Read multiple keys in a single transaction for better performance.
%% @param StoreOpts Database configuration map
%% @param Keys List of keys to read
%% @returns {ok, [{Key, {ok, Value}} | {Key, not_found} | {Key, {error, Reason}}]}
-spec read_many(map(), [binary() | list()]) -> {ok, list()} | {error, any()}.
read_many(StoreOpts, Keys) ->
    NormalizedKeys = [normalize_key(Key) || Key <- Keys],
    case nif_read_many(StoreOpts, NormalizedKeys) of
        {ok, Results} -> {ok, Results};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Fast read operation without link resolution or caching.
%% This is the highest performance read path for simple key-value access.
%% @param StoreOpts Database configuration map
%% @param Key The key to read (must be binary)
%% @returns {ok, Value} | not_found | {error, Reason}
-spec read_fast(map(), binary()) -> {ok, binary()} | not_found | {error, any()}.
read_fast(StoreOpts, Key) when is_binary(Key) ->
    case nif_read_fast(StoreOpts, Key) of
        {ok, Value} -> {ok, Value};
        not_found -> not_found;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Fast write operation without parent group creation or sync.
%% This is the highest performance write path for simple key-value storage.
%% @param StoreOpts Database configuration map
%% @param Key The key to write (must be binary)
%% @param Value The value to write (must be binary)
%% @returns ok | {error, Reason}
-spec write_fast(map(), binary(), binary()) -> ok | {error, any()}.
write_fast(StoreOpts, Key, Value) when is_binary(Key), is_binary(Value) ->
    case nif_write_fast(StoreOpts, Key, Value) of
        ok -> ok;
        error -> {error, write_failed};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Fast bulk read operation without link resolution.
%% @param StoreOpts Database configuration map
%% @param Keys List of binary keys to read
%% @returns {ok, [{Key, {ok, Value}} | {Key, not_found}]} | {error, Reason}
-spec read_many_fast(map(), [binary()]) -> {ok, list()} | {error, any()}.
read_many_fast(StoreOpts, Keys) when is_list(Keys) ->
    case nif_read_many_fast(StoreOpts, Keys) of
        {ok, Results} -> {ok, Results};
        {error, Reason} -> {error, Reason}
    end.

%%% Helper functions

%% @doc Normalize a key to ensure consistent format.
normalize_key(Key) when is_binary(Key) -> Key;
normalize_key(Key) when is_list(Key) -> 
    case lists:all(fun is_binary/1, Key) of
        true -> Key;
        false -> list_to_binary(Key)
    end;
normalize_key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
normalize_key(Key) -> term_to_binary(Key).

%%% NIF stubs (replaced by Rust implementation at runtime)

nif_start(_StoreOpts) ->
    erlang:nif_error(nif_not_loaded).

nif_stop(_StoreOpts) ->
    erlang:nif_error(nif_not_loaded).

nif_reset(_StoreOpts) ->
    erlang:nif_error(nif_not_loaded).

nif_read(_StoreOpts, _Key) ->
    erlang:nif_error(nif_not_loaded).

nif_write(_StoreOpts, _Key, _Value) ->
    erlang:nif_error(nif_not_loaded).

nif_type(_StoreOpts, _Key) ->
    erlang:nif_error(nif_not_loaded).

nif_list(_StoreOpts, _Path) ->
    erlang:nif_error(nif_not_loaded).

nif_make_group(_StoreOpts, _Path) ->
    erlang:nif_error(nif_not_loaded).

nif_make_link(_StoreOpts, _Existing, _New) ->
    erlang:nif_error(nif_not_loaded).

nif_path(_StoreOpts, _Path) ->
    erlang:nif_error(nif_not_loaded).

nif_add_path(_StoreOpts, _Path1, _Path2) ->
    erlang:nif_error(nif_not_loaded).

nif_resolve(_StoreOpts, _Path) ->
    erlang:nif_error(nif_not_loaded).

nif_list_prefix(_StoreOpts, _Prefix, _Opts) ->
    erlang:nif_error(nif_not_loaded).

nif_sync(_StoreOpts) ->
    erlang:nif_error(nif_not_loaded).

nif_read_many(_StoreOpts, _Keys) ->
    erlang:nif_error(nif_not_loaded).

nif_begin_batch(_StoreOpts) ->
    erlang:nif_error(nif_not_loaded).

nif_batch_write(_BatchRef, _Key, _Value) ->
    erlang:nif_error(nif_not_loaded).

nif_batch_make_group(_BatchRef, _Path) ->
    erlang:nif_error(nif_not_loaded).

nif_batch_make_link(_BatchRef, _Existing, _New) ->
    erlang:nif_error(nif_not_loaded).

nif_commit_batch(_BatchRef) ->
    erlang:nif_error(nif_not_loaded).

nif_read_fast(_StoreOpts, _Key) ->
    erlang:nif_error(nif_not_loaded).

nif_write_fast(_StoreOpts, _Key, _Value) ->
    erlang:nif_error(nif_not_loaded).

nif_read_many_fast(_StoreOpts, _Keys) ->
    erlang:nif_error(nif_not_loaded).

%% Direct environment NIFs removed - use store API instead
