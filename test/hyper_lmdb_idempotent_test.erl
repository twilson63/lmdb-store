-module(hyper_lmdb_idempotent_test).
-include_lib("eunit/include/eunit.hrl").

%% Test that multiple start calls with the same path don't create file descriptor leaks
idempotent_start_test() ->
    StoreOpts = #{
        <<"store-module">> => hyper_lmdb,
        <<"name">> => <<"/tmp/lmdb-idempotent-test">>,
        <<"capacity">> => 1024 * 1024 * 100  % 100MB for testing
    },
    
    % Clean up any previous test data
    hyper_lmdb:reset(StoreOpts),
    
    % Start the store multiple times - should be idempotent
    {ok, Env1} = hyper_lmdb:start(StoreOpts),
    {ok, Env2} = hyper_lmdb:start(StoreOpts),
    {ok, Env3} = hyper_lmdb:start(StoreOpts),
    
    % All should return the same environment
    ?assertEqual(Env1, Env2),
    ?assertEqual(Env2, Env3),
    
    % Test that we can still write and read
    ok = hyper_lmdb:write(StoreOpts, <<"test-key">>, <<"test-value">>),
    {ok, Value} = hyper_lmdb:read(StoreOpts, <<"test-key">>),
    ?assertEqual(<<"test-value">>, Value),
    
    % Stop the store
    ok = hyper_lmdb:stop(StoreOpts),
    
    % Clean up
    hyper_lmdb:reset(StoreOpts).

%% Test that different paths create different environments
different_paths_test() ->
    StoreOpts1 = #{
        <<"store-module">> => hyper_lmdb,
        <<"name">> => <<"/tmp/lmdb-test-1">>,
        <<"capacity">> => 1024 * 1024 * 100
    },
    
    StoreOpts2 = #{
        <<"store-module">> => hyper_lmdb,
        <<"name">> => <<"/tmp/lmdb-test-2">>,
        <<"capacity">> => 1024 * 1024 * 100
    },
    
    % Clean up
    hyper_lmdb:reset(StoreOpts1),
    hyper_lmdb:reset(StoreOpts2),
    
    % Start both stores
    {ok, Env1} = hyper_lmdb:start(StoreOpts1),
    {ok, Env2} = hyper_lmdb:start(StoreOpts2),
    
    % They should be different environments
    ?assertNotEqual(Env1, Env2),
    
    % Write different data to each
    ok = hyper_lmdb:write(StoreOpts1, <<"key">>, <<"value1">>),
    ok = hyper_lmdb:write(StoreOpts2, <<"key">>, <<"value2">>),
    
    % Read back and verify isolation
    {ok, Value1} = hyper_lmdb:read(StoreOpts1, <<"key">>),
    {ok, Value2} = hyper_lmdb:read(StoreOpts2, <<"key">>),
    
    ?assertEqual(<<"value1">>, Value1),
    ?assertEqual(<<"value2">>, Value2),
    
    % Stop both stores
    ok = hyper_lmdb:stop(StoreOpts1),
    ok = hyper_lmdb:stop(StoreOpts2),
    
    % Clean up
    hyper_lmdb:reset(StoreOpts1),
    hyper_lmdb:reset(StoreOpts2).

%% Test relative vs absolute path handling
path_normalization_test() ->
    % Use both relative and absolute versions of the same path
    RelativeOpts = #{
        <<"store-module">> => hyper_lmdb,
        <<"name">> => <<"cache-TEST/lmdb">>,
        <<"capacity">> => 1024 * 1024 * 100
    },
    
    % Get the absolute path
    {ok, Cwd} = file:get_cwd(),
    AbsolutePath = filename:join(Cwd, "cache-TEST/lmdb"),
    
    AbsoluteOpts = #{
        <<"store-module">> => hyper_lmdb,
        <<"name">> => list_to_binary(AbsolutePath),
        <<"capacity">> => 1024 * 1024 * 100
    },
    
    % Clean up
    hyper_lmdb:reset(RelativeOpts),
    
    % Start with relative path
    {ok, Env1} = hyper_lmdb:start(RelativeOpts),
    
    % Write some data
    ok = hyper_lmdb:write(RelativeOpts, <<"test">>, <<"data">>),
    
    % Start with absolute path - should get the same environment
    {ok, Env2} = hyper_lmdb:start(AbsoluteOpts),
    
    % Should be the same environment
    ?assertEqual(Env1, Env2),
    
    % Should be able to read the same data
    {ok, Data} = hyper_lmdb:read(AbsoluteOpts, <<"test">>),
    ?assertEqual(<<"data">>, Data),
    
    % Stop the store
    ok = hyper_lmdb:stop(RelativeOpts),
    
    % Clean up
    hyper_lmdb:reset(RelativeOpts).