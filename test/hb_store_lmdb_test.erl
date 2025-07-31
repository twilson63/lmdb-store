-module(hb_store_lmdb_test).
-include_lib("eunit/include/eunit.hrl").

%% Test fixture setup and teardown
setup() ->
    StoreOpts = #{
        <<"store-module">> => hb_store_lmdb,
        <<"name">> => <<"test-store">>,
        <<"path">> => <<"./test-lmdb">>
    },
    ok = hb_store_lmdb:start(StoreOpts),
    StoreOpts.

cleanup(StoreOpts) ->
    hb_store_lmdb:reset(StoreOpts),
    hb_store_lmdb:stop(StoreOpts),
    os:cmd("rm -rf ./test-lmdb").

%% Test suite
lmdb_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_basic_read_write/1,
      fun test_not_found/1,
      fun test_groups/1,
      fun test_links/1,
      fun test_list/1,
      fun test_hierarchical_paths/1,
      fun test_type_detection/1
     ]}.

%% Individual test cases

test_basic_read_write(Store) ->
    fun() ->
        ?assertEqual(ok, hb_store_lmdb:write(Store, <<"key1">>, <<"value1">>)),
        ?assertEqual({ok, <<"value1">>}, hb_store_lmdb:read(Store, <<"key1">>)),
        
        % Test overwrite
        ?assertEqual(ok, hb_store_lmdb:write(Store, <<"key1">>, <<"value2">>)),
        ?assertEqual({ok, <<"value2">>}, hb_store_lmdb:read(Store, <<"key1">>))
    end.

test_not_found(Store) ->
    fun() ->
        ?assertEqual(not_found, hb_store_lmdb:read(Store, <<"nonexistent">>))
    end.

test_groups(Store) ->
    fun() ->
        % Create a group
        ?assertEqual(ok, hb_store_lmdb:make_group(Store, <<"mygroup">>)),
        
        % Write to group
        ?assertEqual(ok, hb_store_lmdb:write(Store, [<<"mygroup">>, <<"item1">>], <<"data1">>)),
        ?assertEqual(ok, hb_store_lmdb:write(Store, [<<"mygroup">>, <<"item2">>], <<"data2">>)),
        
        % Read from group
        ?assertEqual({ok, <<"data1">>}, hb_store_lmdb:read(Store, [<<"mygroup">>, <<"item1">>])),
        ?assertEqual({ok, <<"data2">>}, hb_store_lmdb:read(Store, [<<"mygroup">>, <<"item2">>]))
    end.

test_links(Store) ->
    fun() ->
        % Create original key
        ?assertEqual(ok, hb_store_lmdb:write(Store, <<"original">>, <<"data">>)),
        
        % Create link
        ?assertEqual(ok, hb_store_lmdb:make_link(Store, <<"original">>, <<"link">>)),
        
        % Read through link
        ?assertEqual({ok, <<"data">>}, hb_store_lmdb:read(Store, <<"link">>)),
        
        % Test recursive links
        ?assertEqual(ok, hb_store_lmdb:make_link(Store, <<"link">>, <<"link2">>)),
        ?assertEqual({ok, <<"data">>}, hb_store_lmdb:read(Store, <<"link2">>))
    end.

test_list(Store) ->
    fun() ->
        % Create a group with items
        ?assertEqual(ok, hb_store_lmdb:make_group(Store, <<"listgroup">>)),
        ?assertEqual(ok, hb_store_lmdb:write(Store, [<<"listgroup">>, <<"a">>], <<"1">>)),
        ?assertEqual(ok, hb_store_lmdb:write(Store, [<<"listgroup">>, <<"b">>], <<"2">>)),
        ?assertEqual(ok, hb_store_lmdb:write(Store, [<<"listgroup">>, <<"c">>], <<"3">>)),
        
        % List items
        {ok, Items} = hb_store_lmdb:list(Store, <<"listgroup">>),
        ?assertEqual(3, length(Items)),
        ?assert(lists:member(<<"a">>, Items)),
        ?assert(lists:member(<<"b">>, Items)),
        ?assert(lists:member(<<"c">>, Items))
    end.

test_hierarchical_paths(Store) ->
    fun() ->
        % Create nested groups
        ?assertEqual(ok, hb_store_lmdb:make_group(Store, <<"level1">>)),
        ?assertEqual(ok, hb_store_lmdb:make_group(Store, [<<"level1">>, <<"level2">>])),
        
        % Write to nested path
        ?assertEqual(ok, hb_store_lmdb:write(Store, [<<"level1">>, <<"level2">>, <<"item">>], <<"nested">>)),
        
        % Read from nested path
        ?assertEqual({ok, <<"nested">>}, hb_store_lmdb:read(Store, [<<"level1">>, <<"level2">>, <<"item">>]))
    end.

test_type_detection(Store) ->
    fun() ->
        % Test simple type
        ?assertEqual(ok, hb_store_lmdb:write(Store, <<"simple">>, <<"value">>)),
        ?assertEqual({ok, simple}, hb_store_lmdb:type(Store, <<"simple">>)),
        
        % Test composite type
        ?assertEqual(ok, hb_store_lmdb:make_group(Store, <<"group">>)),
        ?assertEqual({ok, composite}, hb_store_lmdb:type(Store, <<"group">>)),
        
        % Test link type
        ?assertEqual(ok, hb_store_lmdb:make_link(Store, <<"simple">>, <<"mylink">>)),
        ?assertEqual({ok, link}, hb_store_lmdb:type(Store, <<"mylink">>)),
        
        % Test not found
        ?assertEqual(not_found, hb_store_lmdb:type(Store, <<"nonexistent">>))
    end.

%% Performance benchmarks
benchmark_test_() ->
    {timeout, 60, fun benchmark/0}.

benchmark() ->
    Store = setup(),
    try
        io:format("~nRunning LMDB benchmarks...~n"),
        
        % Write benchmark
        WriteCount = 10000,
        WriteStart = erlang:monotonic_time(),
        lists:foreach(
            fun(N) ->
                Key = iolist_to_binary([<<"key-">>, integer_to_binary(N)]),
                Value = iolist_to_binary([<<"value-">>, integer_to_binary(N)]),
                ok = hb_store_lmdb:write(Store, Key, Value)
            end,
            lists:seq(1, WriteCount)
        ),
        WriteEnd = erlang:monotonic_time(),
        WriteTime = erlang:convert_time_unit(WriteEnd - WriteStart, native, microsecond),
        WriteRate = WriteCount * 1000000 / WriteTime,
        io:format("Write: ~p ops in ~.2f ms (~.0f ops/sec)~n", 
                  [WriteCount, WriteTime/1000, WriteRate]),
        
        % Read benchmark
        ReadCount = 10000,
        ReadStart = erlang:monotonic_time(),
        lists:foreach(
            fun(_) ->
                N = rand:uniform(WriteCount),
                Key = iolist_to_binary([<<"key-">>, integer_to_binary(N)]),
                {ok, _} = hb_store_lmdb:read(Store, Key)
            end,
            lists:seq(1, ReadCount)
        ),
        ReadEnd = erlang:monotonic_time(),
        ReadTime = erlang:convert_time_unit(ReadEnd - ReadStart, native, microsecond),
        ReadRate = ReadCount * 1000000 / ReadTime,
        io:format("Read: ~p ops in ~.2f ms (~.0f ops/sec)~n", 
                  [ReadCount, ReadTime/1000, ReadRate]),
        
        ?assert(WriteRate > 10000),  % Should handle >10k writes/sec
        ?assert(ReadRate > 50000)    % Should handle >50k reads/sec
    after
        cleanup(Store)
    end.