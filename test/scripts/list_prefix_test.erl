-module(list_prefix_test).
-include_lib("eunit/include/eunit.hrl").

%% Test fixture
setup() ->
    StoreOpts = #{
        <<"name">> => <<"list_prefix_test">>,
        <<"path">> => <<"/tmp/elmdb_list_prefix_test">>
    },
    os:cmd("rm -rf /tmp/elmdb_list_prefix_test"),
    {ok, Env} = hyper_lmdb:start(StoreOpts),
    {Env, StoreOpts}.

cleanup({_Env, StoreOpts}) ->
    hyper_lmdb:stop(StoreOpts),
    os:cmd("rm -rf /tmp/elmdb_list_prefix_test").

%% Test listing with exact prefix match
test_list_prefix_exact() ->
    {Env, StoreOpts} = setup(),
    try
        % Create a hierarchical structure
        ok = hyper_lmdb:write(Env, <<"users/alice/profile">>, <<"Alice Profile">>),
        ok = hyper_lmdb:write(Env, <<"users/alice/settings">>, <<"Alice Settings">>),
        ok = hyper_lmdb:write(Env, <<"users/bob/profile">>, <<"Bob Profile">>),
        ok = hyper_lmdb:write(Env, <<"users/bob/settings">>, <<"Bob Settings">>),
        ok = hyper_lmdb:write(Env, <<"users/charlie/profile">>, <<"Charlie Profile">>),
        ok = hyper_lmdb:write(Env, <<"products/laptop">>, <<"Laptop">>),
        ok = hyper_lmdb:write(Env, <<"products/phone">>, <<"Phone">>),
        
        % List all items under "users"
        {ok, UserKeys} = hyper_lmdb:list_prefix(Env, <<"users">>),
        ?assertEqual(5, length(UserKeys)),
        ?assert(lists:member(<<"users/alice/profile">>, UserKeys)),
        ?assert(lists:member(<<"users/alice/settings">>, UserKeys)),
        ?assert(lists:member(<<"users/bob/profile">>, UserKeys)),
        ?assert(lists:member(<<"users/bob/settings">>, UserKeys)),
        ?assert(lists:member(<<"users/charlie/profile">>, UserKeys)),
        
        % List all items under "users/alice"
        {ok, AliceKeys} = hyper_lmdb:list_prefix(Env, <<"users/alice">>),
        ?assertEqual(2, length(AliceKeys)),
        ?assert(lists:member(<<"users/alice/profile">>, AliceKeys)),
        ?assert(lists:member(<<"users/alice/settings">>, AliceKeys)),
        
        % List all items under "products"
        {ok, ProductKeys} = hyper_lmdb:list_prefix(Env, <<"products">>),
        ?assertEqual(2, length(ProductKeys)),
        ?assert(lists:member(<<"products/laptop">>, ProductKeys)),
        ?assert(lists:member(<<"products/phone">>, ProductKeys))
    after
        cleanup({Env, StoreOpts})
    end.

%% Test with trailing slash
test_list_prefix_with_slash() ->
    {Env, StoreOpts} = setup(),
    try
        ok = hyper_lmdb:write(Env, <<"config/app/name">>, <<"MyApp">>),
        ok = hyper_lmdb:write(Env, <<"config/app/version">>, <<"1.0">>),
        ok = hyper_lmdb:write(Env, <<"config/database/host">>, <<"localhost">>),
        
        % With trailing slash
        {ok, Keys1} = hyper_lmdb:list_prefix(Env, <<"config/app/">>),
        ?assertEqual(2, length(Keys1)),
        
        % Without trailing slash
        {ok, Keys2} = hyper_lmdb:list_prefix(Env, <<"config/app">>),
        ?assertEqual(2, length(Keys2)),
        
        % Should get same results
        ?assertEqual(lists:sort(Keys1), lists:sort(Keys2))
    after
        cleanup({Env, StoreOpts})
    end.

%% Test empty prefix (list all)
test_list_prefix_empty() ->
    {Env, StoreOpts} = setup(),
    try
        ok = hyper_lmdb:write(Env, <<"key1">>, <<"value1">>),
        ok = hyper_lmdb:write(Env, <<"key2">>, <<"value2">>),
        ok = hyper_lmdb:write(Env, <<"another/key">>, <<"value3">>),
        
        % Empty prefix should list all keys
        {ok, AllKeys} = hyper_lmdb:list_prefix(Env, <<"">>),
        ?assertEqual(3, length(AllKeys)),
        ?assert(lists:member(<<"key1">>, AllKeys)),
        ?assert(lists:member(<<"key2">>, AllKeys)),
        ?assert(lists:member(<<"another/key">>, AllKeys))
    after
        cleanup({Env, StoreOpts})
    end.

%% Test no matches
test_list_prefix_no_match() ->
    {Env, StoreOpts} = setup(),
    try
        ok = hyper_lmdb:write(Env, <<"foo/bar">>, <<"value">>),
        
        {ok, Keys} = hyper_lmdb:list_prefix(Env, <<"nonexistent">>),
        ?assertEqual(0, length(Keys))
    after
        cleanup({Env, StoreOpts})
    end.

%% Test with special characters
test_list_prefix_special_chars() ->
    {Env, StoreOpts} = setup(),
    try
        ok = hyper_lmdb:write(Env, <<"data:type:1">>, <<"value1">>),
        ok = hyper_lmdb:write(Env, <<"data:type:2">>, <<"value2">>),
        ok = hyper_lmdb:write(Env, <<"data-other">>, <<"value3">>),
        
        {ok, Keys} = hyper_lmdb:list_prefix(Env, <<"data:type">>),
        ?assertEqual(2, length(Keys)),
        ?assert(lists:member(<<"data:type:1">>, Keys)),
        ?assert(lists:member(<<"data:type:2">>, Keys))
    after
        cleanup({Env, StoreOpts})
    end.

%% Test prefix boundaries
test_list_prefix_boundaries() ->
    {Env, StoreOpts} = setup(),
    try
        % Keys that share common prefixes
        ok = hyper_lmdb:write(Env, <<"test">>, <<"value1">>),
        ok = hyper_lmdb:write(Env, <<"test1">>, <<"value2">>),
        ok = hyper_lmdb:write(Env, <<"test2">>, <<"value3">>),
        ok = hyper_lmdb:write(Env, <<"testing">>, <<"value4">>),
        ok = hyper_lmdb:write(Env, <<"testament">>, <<"value5">>),
        
        % Should only get keys that start with "test"
        {ok, Keys} = hyper_lmdb:list_prefix(Env, <<"test">>),
        ?assertEqual(5, length(Keys)),
        
        % More specific prefix
        {ok, Keys2} = hyper_lmdb:list_prefix(Env, <<"test1">>),
        ?assertEqual(1, length(Keys2)),
        ?assertEqual([<<"test1">>], Keys2)
    after
        cleanup({Env, StoreOpts})
    end.

%% Test with large dataset
test_list_prefix_large() ->
    {Env, StoreOpts} = setup(),
    try
        % Create 1000 keys under different prefixes
        lists:foreach(fun(I) ->
            Key1 = list_to_binary(io_lib:format("logs/2024/01/~4..0B", [I])),
            Key2 = list_to_binary(io_lib:format("logs/2024/02/~4..0B", [I])),
            ok = hyper_lmdb:write(Env, Key1, <<"log entry">>),
            ok = hyper_lmdb:write(Env, Key2, <<"log entry">>)
        end, lists:seq(1, 500)),
        
        % List all January logs
        {ok, JanKeys} = hyper_lmdb:list_prefix(Env, <<"logs/2024/01">>),
        ?assertEqual(500, length(JanKeys)),
        
        % List all February logs  
        {ok, FebKeys} = hyper_lmdb:list_prefix(Env, <<"logs/2024/02">>),
        ?assertEqual(500, length(FebKeys)),
        
        % List all 2024 logs
        {ok, AllKeys} = hyper_lmdb:list_prefix(Env, <<"logs/2024">>),
        ?assertEqual(1000, length(AllKeys))
    after
        cleanup({Env, StoreOpts})
    end.

%% Test with limit option
test_list_prefix_with_limit() ->
    {Env, StoreOpts} = setup(),
    try
        % Create 100 keys
        lists:foreach(fun(I) ->
            Key = list_to_binary(io_lib:format("items/~3..0B", [I])),
            ok = hyper_lmdb:write(Env, Key, <<"value">>)
        end, lists:seq(1, 100)),
        
        % List with limit
        {ok, Keys} = hyper_lmdb:list_prefix(Env, <<"items">>, #{limit => 10}),
        ?assertEqual(10, length(Keys)),
        
        % Verify they are the first 10 lexicographically
        Expected = [list_to_binary(io_lib:format("items/~3..0B", [I])) || I <- lists:seq(1, 10)],
        ?assertEqual(Expected, lists:sort(Keys))
    after
        cleanup({Env, StoreOpts})
    end.

%% Test cursor continuation
test_list_prefix_with_cursor() ->
    {Env, StoreOpts} = setup(),
    try
        % Create keys
        lists:foreach(fun(I) ->
            Key = list_to_binary(io_lib:format("data/~2..0B", [I])),
            ok = hyper_lmdb:write(Env, Key, <<"value">>)
        end, lists:seq(1, 20)),
        
        % First batch
        {ok, Keys1, Cursor1} = hyper_lmdb:list_prefix(Env, <<"data">>, #{limit => 5, return_cursor => true}),
        ?assertEqual(5, length(Keys1)),
        
        % Continue with cursor
        {ok, Keys2, Cursor2} = hyper_lmdb:list_prefix(Env, <<"data">>, #{limit => 5, cursor => Cursor1, return_cursor => true}),
        ?assertEqual(5, length(Keys2)),
        
        % Verify no overlap
        ?assertEqual([], lists:filter(fun(K) -> lists:member(K, Keys1) end, Keys2)),
        
        % Continue again
        {ok, Keys3, _} = hyper_lmdb:list_prefix(Env, <<"data">>, #{limit => 5, cursor => Cursor2, return_cursor => true}),
        ?assertEqual(5, length(Keys3))
    after
        cleanup({Env, StoreOpts})
    end.

%% Run all tests
list_prefix_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        fun test_list_prefix_exact/1,
        fun test_list_prefix_with_slash/1,
        fun test_list_prefix_empty/1,
        fun test_list_prefix_no_match/1,
        fun test_list_prefix_special_chars/1,
        fun test_list_prefix_boundaries/1,
        fun test_list_prefix_large/1,
        fun test_list_prefix_with_limit/1
     ]
    }.