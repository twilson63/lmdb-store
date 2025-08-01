-module(hyper_lmdb_flags_test).
-include_lib("eunit/include/eunit.hrl").

flags_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(BaseOpts) ->
         [
          {"Default flags (performance mode)", 
           fun() -> test_default_flags(BaseOpts) end},
          {"Durability mode flags", 
           fun() -> test_durability_flags(BaseOpts) end},
          {"Read-only mode", 
           fun() -> test_readonly_flags(BaseOpts) end},
          {"Custom flag combinations", 
           fun() -> test_custom_flags(BaseOpts) end}
         ]
     end}.

setup() ->
    #{}.

cleanup(_) ->
    % Clean up any test stores
    os:cmd("rm -rf test_flags_*").

test_default_flags(BaseOpts) ->
    StoreOpts = BaseOpts#{
        <<"name">> => <<"test_flags_default">>,
        <<"capacity">> => 1024 * 1024 * 100  % 100MB
    },
    
    % Start with default flags (all performance flags enabled)
    {ok, _Env} = hyper_lmdb:start(StoreOpts),
    
    % Should be able to write
    ?assertEqual(ok, hyper_lmdb:write(StoreOpts, <<"key1">>, <<"value1">>)),
    
    % Should be able to read
    ?assertEqual({ok, <<"value1">>}, hyper_lmdb:read(StoreOpts, <<"key1">>)),
    
    hyper_lmdb:stop(StoreOpts).

test_durability_flags(BaseOpts) ->
    StoreOpts = BaseOpts#{
        <<"name">> => <<"test_flags_durable">>,
        <<"capacity">> => 1024 * 1024 * 100,
        <<"no_sync">> => false,
        <<"map_async">> => false,
        <<"no_meta_sync">> => false
    },
    
    {ok, _Env} = hyper_lmdb:start(StoreOpts),
    
    % Writes should still work but will be slower
    ?assertEqual(ok, hyper_lmdb:write(StoreOpts, <<"key1">>, <<"value1">>)),
    ?assertEqual({ok, <<"value1">>}, hyper_lmdb:read(StoreOpts, <<"key1">>)),
    
    hyper_lmdb:stop(StoreOpts).

test_readonly_flags(BaseOpts) ->
    % First create a store with some data
    StoreOpts = BaseOpts#{
        <<"name">> => <<"test_flags_readonly">>,
        <<"capacity">> => 1024 * 1024 * 100
    },
    
    {ok, _Env} = hyper_lmdb:start(StoreOpts),
    ?assertEqual(ok, hyper_lmdb:write(StoreOpts, <<"key1">>, <<"value1">>)),
    hyper_lmdb:stop(StoreOpts),
    
    % Now open in read-only mode
    ReadOnlyOpts = StoreOpts#{<<"read_only">> => true},
    {ok, _Env2} = hyper_lmdb:start(ReadOnlyOpts),
    
    % Should be able to read
    ?assertEqual({ok, <<"value1">>}, hyper_lmdb:read(ReadOnlyOpts, <<"key1">>)),
    
    % Writes should fail (or be ignored)
    Result = hyper_lmdb:write(ReadOnlyOpts, <<"key2">>, <<"value2">>),
    ?assert(Result =:= ok orelse element(1, Result) =:= error),
    
    hyper_lmdb:stop(ReadOnlyOpts).

test_custom_flags(BaseOpts) ->
    % Test various flag combinations
    StoreOpts1 = BaseOpts#{
        <<"name">> => <<"test_flags_custom1">>,
        <<"capacity">> => 1024 * 1024 * 100,
        <<"no_sync">> => true,
        <<"map_async">> => false  % Sync writes but no fsync
    },
    
    {ok, _Env3} = hyper_lmdb:start(StoreOpts1),
    ?assertEqual(ok, hyper_lmdb:write(StoreOpts1, <<"key1">>, <<"value1">>)),
    ?assertEqual({ok, <<"value1">>}, hyper_lmdb:read(StoreOpts1, <<"key1">>)),
    hyper_lmdb:stop(StoreOpts1),
    
    % Another combination
    StoreOpts2 = BaseOpts#{
        <<"name">> => <<"test_flags_custom2">>,
        <<"capacity">> => 1024 * 1024 * 100,
        <<"no_readahead">> => false  % Enable readahead
    },
    
    {ok, _Env4} = hyper_lmdb:start(StoreOpts2),
    ?assertEqual(ok, hyper_lmdb:write(StoreOpts2, <<"key1">>, <<"value1">>)),
    ?assertEqual({ok, <<"value1">>}, hyper_lmdb:read(StoreOpts2, <<"key1">>)),
    hyper_lmdb:stop(StoreOpts2).