-module(hyper_lmdb_list_prefix_test).
-include_lib("eunit/include/eunit.hrl").

%% Test fixture
setup() ->
    StoreOpts = #{
        <<"name">> => <<"list_prefix_test">>,
        <<"db_path">> => <<"/tmp/elmdb_list_prefix_test">>
    },
    os:cmd("rm -rf /tmp/elmdb_list_prefix_test"),
    {ok, Env} = hyper_lmdb:start(StoreOpts),
    {Env, StoreOpts}.

cleanup({_Env, StoreOpts}) ->
    hyper_lmdb:stop(StoreOpts),
    os:cmd("rm -rf /tmp/elmdb_list_prefix_test").

%% Test generator
list_prefix_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        fun(X) -> {"List with exact prefix", fun() -> test_list_prefix_exact(X) end} end,
        fun(X) -> {"List with trailing slash", fun() -> test_list_prefix_with_slash(X) end} end,
        fun(X) -> {"List with empty prefix", fun() -> test_list_prefix_empty(X) end} end,
        fun(X) -> {"List with no matches", fun() -> test_list_prefix_no_match(X) end} end,
        fun(X) -> {"List with special characters", fun() -> test_list_prefix_special_chars(X) end} end,
        fun(X) -> {"List prefix boundaries", fun() -> test_list_prefix_boundaries(X) end} end,
        fun(X) -> {"List with limit", fun() -> test_list_prefix_with_limit(X) end} end
     ]
    }.

%% Individual tests

test_list_prefix_exact({Env, _StoreOpts}) ->
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
    ?assert(lists:member(<<"products/phone">>, ProductKeys)).

test_list_prefix_with_slash({Env, _StoreOpts}) ->
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
    ?assertEqual(lists:sort(Keys1), lists:sort(Keys2)).

test_list_prefix_empty({Env, _StoreOpts}) ->
    ok = hyper_lmdb:write(Env, <<"key1">>, <<"value1">>),
    ok = hyper_lmdb:write(Env, <<"key2">>, <<"value2">>),
    ok = hyper_lmdb:write(Env, <<"another/key">>, <<"value3">>),
    
    % Empty prefix should list all keys
    {ok, AllKeys} = hyper_lmdb:list_prefix(Env, <<"">>),
    ?assertEqual(3, length(AllKeys)),
    ?assert(lists:member(<<"key1">>, AllKeys)),
    ?assert(lists:member(<<"key2">>, AllKeys)),
    ?assert(lists:member(<<"another/key">>, AllKeys)).

test_list_prefix_no_match({Env, _StoreOpts}) ->
    ok = hyper_lmdb:write(Env, <<"foo/bar">>, <<"value">>),
    
    {ok, Keys} = hyper_lmdb:list_prefix(Env, <<"nonexistent">>),
    ?assertEqual(0, length(Keys)).

test_list_prefix_special_chars({Env, _StoreOpts}) ->
    ok = hyper_lmdb:write(Env, <<"data:type:1">>, <<"value1">>),
    ok = hyper_lmdb:write(Env, <<"data:type:2">>, <<"value2">>),
    ok = hyper_lmdb:write(Env, <<"data-other">>, <<"value3">>),
    
    {ok, Keys} = hyper_lmdb:list_prefix(Env, <<"data:type">>),
    ?assertEqual(2, length(Keys)),
    ?assert(lists:member(<<"data:type:1">>, Keys)),
    ?assert(lists:member(<<"data:type:2">>, Keys)).

test_list_prefix_boundaries({Env, _StoreOpts}) ->
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
    ?assertEqual([<<"test1">>], Keys2).

test_list_prefix_with_limit({Env, _StoreOpts}) ->
    % Create 20 keys
    lists:foreach(fun(I) ->
        Key = list_to_binary(io_lib:format("items/~2..0B", [I])),
        ok = hyper_lmdb:write(Env, Key, <<"value">>)
    end, lists:seq(1, 20)),
    
    % Test with limit
    {ok, Keys} = hyper_lmdb:list_prefix(Env, <<"items">>, #{limit => 5}),
    ?assertEqual(5, length(Keys)),
    
    % Verify they are the first 5 lexicographically
    Expected = [list_to_binary(io_lib:format("items/~2..0B", [I])) || I <- lists:seq(1, 5)],
    ?assertEqual(Expected, lists:sort(Keys)).