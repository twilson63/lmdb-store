-module(hyper_lmdb_link_test).
-include_lib("eunit/include/eunit.hrl").

%% Test fixture setup and teardown
setup() ->
    StoreOpts = #{
        <<"store-module">> => hyper_lmdb,
        <<"name">> => <<"link-test-store">>,
        <<"path">> => <<"./test-link-lmdb">>
    },
    case hyper_lmdb:start(StoreOpts) of
        {ok, _EnvRef} -> ok;
        ok -> ok
    end,
    StoreOpts.

cleanup(StoreOpts) ->
    hyper_lmdb:reset(StoreOpts),
    hyper_lmdb:stop(StoreOpts),
    os:cmd("rm -rf ./test-link-lmdb").

%% Test generators
link_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        fun test_simple_link/1,
        fun test_link_chain/1,
        fun test_circular_link/1,
        fun test_link_type_detection/1,
        fun test_link_overwrite/1,
        fun test_link_to_nonexistent/1,
        fun test_mixed_links_and_values/1
     ]
    }.

test_simple_link(StoreOpts) ->
    {"Simple link resolution",
     fun() ->
        % Write a value
        ok = hyper_lmdb:write(StoreOpts, <<"target">>, <<"target_value">>),
        
        % Create a link to it
        ok = hyper_lmdb:make_link(StoreOpts, <<"target">>, <<"link1">>),
        
        % Reading the link should return the target value
        ?assertEqual({ok, <<"target_value">>}, hyper_lmdb:read(StoreOpts, <<"link1">>))
     end}.

test_link_chain(StoreOpts) ->
    {"Link chain resolution",
     fun() ->
        % Create a chain: value <- link1 <- link2 <- link3
        ok = hyper_lmdb:write(StoreOpts, <<"original">>, <<"original_value">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"original">>, <<"link1">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"link1">>, <<"link2">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"link2">>, <<"link3">>),
        
        % All links should resolve to the original value
        ?assertEqual({ok, <<"original_value">>}, hyper_lmdb:read(StoreOpts, <<"link1">>)),
        ?assertEqual({ok, <<"original_value">>}, hyper_lmdb:read(StoreOpts, <<"link2">>)),
        ?assertEqual({ok, <<"original_value">>}, hyper_lmdb:read(StoreOpts, <<"link3">>))
     end}.

test_circular_link(StoreOpts) ->
    {"Circular link detection",
     fun() ->
        % Create circular links: link1 -> link2 -> link3 -> link1
        ok = hyper_lmdb:make_link(StoreOpts, <<"link2">>, <<"link1">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"link3">>, <<"link2">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"link1">>, <<"link3">>),
        
        % Reading any of them should not crash, but return not_found
        % since none of them point to an actual value
        ?assertEqual(not_found, hyper_lmdb:read(StoreOpts, <<"link1">>)),
        ?assertEqual(not_found, hyper_lmdb:read(StoreOpts, <<"link2">>)),
        ?assertEqual(not_found, hyper_lmdb:read(StoreOpts, <<"link3">>))
     end}.

test_link_type_detection(StoreOpts) ->
    {"Link type detection",
     fun() ->
        % Create different types
        ok = hyper_lmdb:write(StoreOpts, <<"value_key">>, <<"some_value">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"value_key">>, <<"link_key">>),
        ok = hyper_lmdb:make_group(StoreOpts, <<"group_key">>),
        
        % Check types
        ?assertEqual({ok, simple}, hyper_lmdb:type(StoreOpts, <<"value_key">>)),
        ?assertEqual({ok, link}, hyper_lmdb:type(StoreOpts, <<"link_key">>)),
        ?assertEqual({ok, composite}, hyper_lmdb:type(StoreOpts, <<"group_key">>))
     end}.

test_link_overwrite(StoreOpts) ->
    {"Link overwrite behavior",
     fun() ->
        % Create initial value and link
        ok = hyper_lmdb:write(StoreOpts, <<"target1">>, <<"value1">>),
        ok = hyper_lmdb:write(StoreOpts, <<"target2">>, <<"value2">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"target1">>, <<"mylink">>),
        
        ?assertEqual({ok, <<"value1">>}, hyper_lmdb:read(StoreOpts, <<"mylink">>)),
        
        % Overwrite link to point elsewhere
        ok = hyper_lmdb:make_link(StoreOpts, <<"target2">>, <<"mylink">>),
        ?assertEqual({ok, <<"value2">>}, hyper_lmdb:read(StoreOpts, <<"mylink">>)),
        
        % Overwrite link with a regular value
        ok = hyper_lmdb:write(StoreOpts, <<"mylink">>, <<"direct_value">>),
        ?assertEqual({ok, <<"direct_value">>}, hyper_lmdb:read(StoreOpts, <<"mylink">>)),
        ?assertEqual({ok, simple}, hyper_lmdb:type(StoreOpts, <<"mylink">>))
     end}.

test_link_to_nonexistent(StoreOpts) ->
    {"Link to nonexistent target",
     fun() ->
        % Create a link to a nonexistent target
        ok = hyper_lmdb:make_link(StoreOpts, <<"nonexistent">>, <<"broken_link">>),
        
        % Reading should return not_found
        ?assertEqual(not_found, hyper_lmdb:read(StoreOpts, <<"broken_link">>)),
        
        % But the link itself exists
        ?assertEqual({ok, link}, hyper_lmdb:type(StoreOpts, <<"broken_link">>))
     end}.

test_mixed_links_and_values(StoreOpts) ->
    {"Mixed links and values in paths",
     fun() ->
        % Create a hierarchy with mixed types
        ok = hyper_lmdb:make_group(StoreOpts, <<"root">>),
        ok = hyper_lmdb:write(StoreOpts, <<"root/data">>, <<"data_value">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"root/data">>, <<"root/link_to_data">>),
        ok = hyper_lmdb:write(StoreOpts, <<"root/subdir/item">>, <<"item_value">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"root/subdir/item">>, <<"root/link_to_item">>),
        
        % Test resolution
        ?assertEqual({ok, <<"data_value">>}, hyper_lmdb:read(StoreOpts, <<"root/link_to_data">>)),
        ?assertEqual({ok, <<"item_value">>}, hyper_lmdb:read(StoreOpts, <<"root/link_to_item">>)),
        
        % List should show both values and links
        {ok, Children} = hyper_lmdb:list(StoreOpts, <<"root">>),
        ?assert(lists:member(<<"data">>, Children)),
        ?assert(lists:member(<<"link_to_data">>, Children)),
        ?assert(lists:member(<<"link_to_item">>, Children)),
        ?assert(lists:member(<<"subdir">>, Children))
     end}.