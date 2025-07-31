# Using hyper_lmdb as a Dependency

## Adding to Your Project

### With Rebar3

Add to your `rebar.config`:

```erlang
{deps, [
    {hyper_lmdb, {git, "https://github.com/twilson63/lmdb-store.git", {branch, "main"}}}
]}.
```

Then compile:
```bash
rebar3 compile
```

### With Mix (Elixir)

Add to your `mix.exs`:

```elixir
defp deps do
  [
    {:hyper_lmdb, git: "https://github.com/twilson63/lmdb-store.git", branch: "main"}
  ]
end
```

## Example Usage

```erlang
-module(my_app).
-export([example/0]).

example() ->
    % Initialize the store
    StoreOpts = #{
        <<"name">> => <<"./data/myapp">>,
        <<"capacity">> => 1073741824  % 1GB
    },
    {ok, _Env} = hyper_lmdb:start(StoreOpts),
    
    % Write data
    ok = hyper_lmdb:write(StoreOpts, <<"users/alice">>, <<"Alice Data">>),
    ok = hyper_lmdb:write(StoreOpts, <<"config/version">>, <<"1.0.0">>),
    
    % Read data
    {ok, AliceData} = hyper_lmdb:read(StoreOpts, <<"users/alice">>),
    
    % Use links
    ok = hyper_lmdb:make_link(StoreOpts, <<"config/version">>, <<"current_version">>),
    {ok, Version} = hyper_lmdb:read(StoreOpts, <<"current_version">>),
    
    % Clean up
    hyper_lmdb:stop(StoreOpts),
    ok.
```

## HyperBEAM Integration

If using with HyperBEAM, you can specify hyper_lmdb as your store module:

```erlang
StoreOpts = #{
    <<"store-module">> => hyper_lmdb,
    <<"name">> => <<"./data/hyperbeam_store">>
}.
```

The module implements the standard HyperBEAM store behavior interface.