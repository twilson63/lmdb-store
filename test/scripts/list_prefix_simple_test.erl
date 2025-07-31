-module(list_prefix_simple_test).
-export([run/0]).

run() ->
    io:format("=== List Prefix Tests ===~n~n"),
    
    StoreOpts = #{
        <<"name">> => <<"list_prefix_test">>,
        <<"path">> => <<"/tmp/elmdb_list_prefix_test">>
    },
    
    % Cleanup
    os:cmd("rm -rf /tmp/elmdb_list_prefix_test"),
    
    % Start store
    {ok, Env} = hyper_lmdb:start(StoreOpts),
    
    try
        test_exact_prefix(Env),
        test_empty_prefix(Env),
        test_no_match(Env),
        test_with_limit(Env),
        test_performance(Env),
        
        io:format("~nAll tests passed! ✓~n")
    catch
        Type:Error:Stack ->
            io:format("~nTest failed: ~p:~p~n~p~n", [Type, Error, Stack]),
            error
    after
        hyper_lmdb:stop(StoreOpts),
        os:cmd("rm -rf /tmp/elmdb_list_prefix_test")
    end.

test_exact_prefix(Env) ->
    io:format("Test 1: Exact prefix matching..."),
    
    % Write test data
    ok = hyper_lmdb:write(Env, <<"users/alice/profile">>, <<"Alice Profile">>),
    ok = hyper_lmdb:write(Env, <<"users/alice/settings">>, <<"Alice Settings">>),
    ok = hyper_lmdb:write(Env, <<"users/bob/profile">>, <<"Bob Profile">>),
    ok = hyper_lmdb:write(Env, <<"products/laptop">>, <<"Laptop">>),
    
    % Test listing all under "users"
    {ok, UserKeys} = hyper_lmdb:list_prefix(Env, <<"users">>),
    3 = length(UserKeys),
    true = lists:member(<<"users/alice/profile">>, UserKeys),
    true = lists:member(<<"users/alice/settings">>, UserKeys),
    true = lists:member(<<"users/bob/profile">>, UserKeys),
    
    % Test listing under "users/alice"
    {ok, AliceKeys} = hyper_lmdb:list_prefix(Env, <<"users/alice">>),
    2 = length(AliceKeys),
    true = lists:member(<<"users/alice/profile">>, AliceKeys),
    true = lists:member(<<"users/alice/settings">>, AliceKeys),
    
    io:format(" ✓~n").

test_empty_prefix(Env) ->
    io:format("Test 2: Empty prefix (list all)..."),
    
    % Write new data (don't reset - it needs store opts not env)
    ok = hyper_lmdb:write(Env, <<"key1">>, <<"value1">>),
    ok = hyper_lmdb:write(Env, <<"key2">>, <<"value2">>),
    ok = hyper_lmdb:write(Env, <<"another/key">>, <<"value3">>),
    
    % Empty prefix should list all
    {ok, AllKeys} = hyper_lmdb:list_prefix(Env, <<"">>),
    io:format("~n  Found keys: ~p~n", [AllKeys]),
    true = length(AllKeys) >= 3,  % At least the 3 we just added
    true = lists:member(<<"key1">>, AllKeys),
    true = lists:member(<<"key2">>, AllKeys),
    true = lists:member(<<"another/key">>, AllKeys),
    
    io:format(" ✓~n").

test_no_match(Env) ->
    io:format("Test 3: No matching keys..."),
    
    % Don't reset - just test with existing data
    ok = hyper_lmdb:write(Env, <<"foo/bar">>, <<"value">>),
    
    {ok, Keys} = hyper_lmdb:list_prefix(Env, <<"nonexistent">>),
    0 = length(Keys),
    
    io:format(" ✓~n").

test_with_limit(Env) ->
    io:format("Test 4: Listing with limit..."),
    
    % Create 20 keys
    lists:foreach(fun(I) ->
        Key = list_to_binary(io_lib:format("items/~2..0B", [I])),
        ok = hyper_lmdb:write(Env, Key, <<"value">>)
    end, lists:seq(1, 20)),
    
    % Test with limit
    {ok, Keys} = hyper_lmdb:list_prefix(Env, <<"items">>, #{limit => 5}),
    5 = length(Keys),
    
    % Verify they are the first 5 lexicographically
    Expected = [<<"items/01">>, <<"items/02">>, <<"items/03">>, <<"items/04">>, <<"items/05">>],
    Expected = lists:sort(Keys),
    
    io:format(" ✓~n").

test_performance(Env) ->
    io:format("Test 5: Performance with 10K keys..."),
    
    % Create 10,000 keys across different prefixes
    Start = erlang:monotonic_time(millisecond),
    lists:foreach(fun(I) ->
        Key1 = list_to_binary(io_lib:format("data/2024/01/~5..0B", [I])),
        Key2 = list_to_binary(io_lib:format("data/2024/02/~5..0B", [I])),
        Key3 = list_to_binary(io_lib:format("data/2024/03/~5..0B", [I])),
        ok = hyper_lmdb:write(Env, Key1, <<"data">>),
        ok = hyper_lmdb:write(Env, Key2, <<"data">>),
        ok = hyper_lmdb:write(Env, Key3, <<"data">>)
    end, lists:seq(1, 3333)),
    WriteTime = erlang:monotonic_time(millisecond) - Start,
    
    % List all January entries
    ListStart = erlang:monotonic_time(millisecond),
    {ok, JanKeys} = hyper_lmdb:list_prefix(Env, <<"data/2024/01">>),
    ListTime = erlang:monotonic_time(millisecond) - ListStart,
    
    3333 = length(JanKeys),
    
    % List with limit
    LimitStart = erlang:monotonic_time(millisecond),
    {ok, LimitKeys} = hyper_lmdb:list_prefix(Env, <<"data/2024">>, #{limit => 100}),
    LimitTime = erlang:monotonic_time(millisecond) - LimitStart,
    
    100 = length(LimitKeys),
    
    io:format(" ✓~n"),
    io:format("  - Write 10K keys: ~p ms (~p keys/sec)~n", [WriteTime, round(9999000 / WriteTime)]),
    io:format("  - List 3333 keys: ~p ms~n", [ListTime]),
    io:format("  - List with limit: ~p ms~n", [LimitTime]).