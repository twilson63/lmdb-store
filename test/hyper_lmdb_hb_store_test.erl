%% @doc Test suite for hyper_lmdb using the hb_store_lmdb test cases.
%%
%% This module extracts and runs all the test cases from the hb_store_lmdb
%% module to ensure that hyper_lmdb correctly implements the same behavior.
%%
%% The tests include:
%% - Basic read/write operations
%% - Hierarchical listing with prefix matching
%% - Group creation and type detection
%% - Symbolic link creation and resolution
%% - Path traversal through links
%% - Nested map storage with cache-like behavior
%% - Complex link resolution scenarios
-module(hyper_lmdb_hb_store_test).
-include_lib("eunit/include/eunit.hrl").

%% Export test functions for eunit
-compile(export_all).
-compile(nowarn_export_all).

%% Helper to create store options for hyper_lmdb
store_opts(Name) ->
    #{
        <<"store-module">> => hyper_lmdb,
        <<"name">> => Name
    }.

%% Helper to clean up test directories
cleanup(Name) ->
    hyper_lmdb:stop(store_opts(Name)),
    os:cmd("rm -rf " ++ binary_to_list(Name)).

%% Basic store test - verifies fundamental read/write functionality
basic_test() ->
    StoreOpts = store_opts(<<"/tmp/store-1">>),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    Res = hyper_lmdb:write(StoreOpts, <<"Hello">>, <<"World2">>),
    ?assertEqual(ok, Res),
    {ok, Value} = hyper_lmdb:read(StoreOpts, <<"Hello">>),
    ?assertEqual(Value, <<"World2">>),
    cleanup(<<"/tmp/store-1">>).

%% List test - verifies prefix-based key listing functionality
list_test() ->
    StoreOpts = store_opts(<<"/tmp/store-2">>),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    % Create immediate children under colors/
    hyper_lmdb:write(StoreOpts, <<"colors/red">>, <<"1">>),
    hyper_lmdb:write(StoreOpts, <<"colors/blue">>, <<"2">>),
    hyper_lmdb:write(StoreOpts, <<"colors/green">>, <<"3">>),
    
    % Create nested directories under colors/
    hyper_lmdb:write(StoreOpts, <<"colors/multi/foo">>, <<"4">>),
    hyper_lmdb:write(StoreOpts, <<"colors/multi/bar">>, <<"5">>),
    hyper_lmdb:write(StoreOpts, <<"colors/primary/red">>, <<"6">>),
    hyper_lmdb:write(StoreOpts, <<"colors/primary/blue">>, <<"7">>),
    hyper_lmdb:write(StoreOpts, <<"colors/nested/deep/value">>, <<"8">>),
    
    % Create other top-level directories
    hyper_lmdb:write(StoreOpts, <<"foo/bar">>, <<"baz">>),
    hyper_lmdb:write(StoreOpts, <<"beep/boop">>, <<"bam">>),
    
    hyper_lmdb:read(StoreOpts, <<"colors">>), 
    % Test listing colors/ - should return immediate children only
    {ok, ListResult} = hyper_lmdb:list(StoreOpts, <<"colors">>),
    
    % Expected: red, blue, green (files) + multi, primary, nested (directories)
    ExpectedChildren = [<<"blue">>, <<"green">>, <<"multi">>, <<"nested">>, <<"primary">>, <<"red">>],
    SortedResult = lists:sort(ListResult),
    SortedExpected = lists:sort(ExpectedChildren),
    ?assertEqual(SortedExpected, SortedResult),
    
    % Test listing a nested directory
    {ok, NestedListResult} = hyper_lmdb:list(StoreOpts, <<"colors/multi">>),
    ExpectedNestedChildren = [<<"bar">>, <<"foo">>],
    ?assertEqual(lists:sort(ExpectedNestedChildren), lists:sort(NestedListResult)),
    
    % Test listing a deeper nested directory
    {ok, DeepListResult} = hyper_lmdb:list(StoreOpts, <<"colors/nested">>),
    ExpectedDeepChildren = [<<"deep">>],
    ?assertEqual(lists:sort(ExpectedDeepChildren), lists:sort(DeepListResult)),
    
    cleanup(<<"/tmp/store-2">>).

%% Group test - verifies group creation and type detection
group_test() ->
    StoreOpts = store_opts(<<"/tmp/store3">>),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    hyper_lmdb:make_group(StoreOpts, <<"colors">>),
    % Groups should be detected as composite types
    ?assertEqual(composite, hyper_lmdb:type(StoreOpts, <<"colors">>)),
    % Groups should not be readable directly (like directories in filesystem)
    ?assertEqual(not_found, hyper_lmdb:read(StoreOpts, <<"colors">>)),
    cleanup(<<"/tmp/store3">>).

%% Link test - verifies symbolic link creation and resolution
link_test() ->
    StoreOpts = store_opts(<<"/tmp/store4">>),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    hyper_lmdb:write(StoreOpts, <<"foo/bar/baz">>, <<"Bam">>),
    hyper_lmdb:make_link(StoreOpts, <<"foo/bar/baz">>, <<"foo/beep/baz">>),
    {ok, Result} = hyper_lmdb:read(StoreOpts, <<"foo/beep/baz">>),
    ?assertEqual(<<"Bam">>, Result),
    cleanup(<<"/tmp/store4">>).

%% Link fragment test - verifies link resolution in path fragments
link_fragment_test() ->
    StoreOpts = store_opts(<<"/tmp/store5">>),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    hyper_lmdb:write(StoreOpts, [<<"data">>, <<"bar">>, <<"baz">>], <<"Bam">>),
    hyper_lmdb:make_link(StoreOpts, [<<"data">>, <<"bar">>], <<"my-link">>),
    {ok, Result} = hyper_lmdb:read(StoreOpts, [<<"my-link">>, <<"baz">>]),
    ?assertEqual(<<"Bam">>, Result),
    cleanup(<<"/tmp/store5">>).

%% Type test - verifies type detection for both simple and composite entries
type_test() ->
    StoreOpts = store_opts(<<"/tmp/store-6">>),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    hyper_lmdb:make_group(StoreOpts, <<"assets">>),
    Type = hyper_lmdb:type(StoreOpts, <<"assets">>),
    ?assertEqual(composite, Type),
    hyper_lmdb:write(StoreOpts, <<"assets/1">>, <<"bam">>),
    Type2 = hyper_lmdb:type(StoreOpts, <<"assets/1">>),
    ?assertEqual(simple, Type2),
    cleanup(<<"/tmp/store-6">>).

%% Link key list test - verifies symbolic link creation using structured key paths
link_key_list_test() ->
    StoreOpts = store_opts(<<"/tmp/store-7">>),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    hyper_lmdb:write(StoreOpts, [ <<"parent">>, <<"key">> ], <<"value">>),
    hyper_lmdb:make_link(StoreOpts, [ <<"parent">>, <<"key">> ], <<"my-link">>),
    {ok, Result} = hyper_lmdb:read(StoreOpts, <<"my-link">>),
    ?assertEqual(<<"value">>, Result),
    cleanup(<<"/tmp/store-7">>).

%% Path traversal link test - verifies link resolution during path traversal
path_traversal_link_test() ->
    StoreOpts = store_opts(<<"/tmp/store-8">>),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    % Create the actual data at group/key
    hyper_lmdb:write(StoreOpts, [<<"group">>, <<"key">>], <<"target-value">>),
    % Create a link from "link" to "group"
    hyper_lmdb:make_link(StoreOpts, <<"group">>, <<"link">>),
    % Reading via the link path should resolve to the target value
    {ok, Result} = hyper_lmdb:read(StoreOpts, [<<"link">>, <<"key">>]),
    ?assertEqual(<<"target-value">>, Result),
    cleanup(<<"/tmp/store-8">>).

%% Test that matches the exact hb_store hierarchical test pattern
exact_hb_store_test() ->
    StoreOpts = store_opts(<<"/tmp/store-exact">>),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    % Follow exact same pattern as hb_store test
    hyper_lmdb:make_group(StoreOpts, <<"test-dir1">>),
    hyper_lmdb:write(StoreOpts, [<<"test-dir1">>, <<"test-file">>], <<"test-data">>),
    hyper_lmdb:make_link(StoreOpts, [<<"test-dir1">>], <<"test-link">>),
    
    % Since test-dir1 is a group and groups are unreadable, the link should also be unreadable
    LinkResult = hyper_lmdb:read(StoreOpts, <<"test-link">>),
    ?assertEqual(not_found, LinkResult),
    
    % Test direct read
    DirectResult = hyper_lmdb:read(StoreOpts, <<"test-dir1/test-file">>),
    ?assertEqual({ok, <<"test-data">>}, DirectResult),
    
    % This should work: reading via the link path  
    Result = hyper_lmdb:read(StoreOpts, [<<"test-link">>, <<"test-file">>]),
    ?assertEqual({ok, <<"test-data">>}, Result),
    cleanup(<<"/tmp/store-exact">>).

%% Test nested map storage with cache-like linking behavior
nested_map_cache_test() ->
    StoreOpts = store_opts(<<"/tmp/store-nested-cache">>),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    % Original nested map structure
    OriginalMap = #{
        <<"target">> => <<"Foo">>,
        <<"commitments">> => #{
            <<"key1">> => #{
              <<"alg">> => <<"rsa-pss-512">>,
              <<"committer">> => <<"unique-id">>
            },
            <<"key2">> => #{
              <<"alg">> => <<"hmac">>,
              <<"commiter">> => <<"unique-id-2">>              
            }
        },
        <<"other-key">> => #{
            <<"other-key-key">> => <<"other-key-value">>
        }
    },
    
    % Step 1: Store each leaf value at data/{hash}
    TargetValue = <<"Foo">>,
    TargetHash = base64:encode(crypto:hash(sha256, TargetValue)),
    hyper_lmdb:write(StoreOpts, <<"data/", TargetHash/binary>>, TargetValue),
    
    AlgValue1 = <<"rsa-pss-512">>,
    AlgHash1 = base64:encode(crypto:hash(sha256, AlgValue1)),
    hyper_lmdb:write(StoreOpts, <<"data/", AlgHash1/binary>>, AlgValue1),
    
    CommitterValue1 = <<"unique-id">>,
    CommitterHash1 = base64:encode(crypto:hash(sha256, CommitterValue1)),
    hyper_lmdb:write(StoreOpts, <<"data/", CommitterHash1/binary>>, CommitterValue1),
    
    AlgValue2 = <<"hmac">>,
    AlgHash2 = base64:encode(crypto:hash(sha256, AlgValue2)),
    hyper_lmdb:write(StoreOpts, <<"data/", AlgHash2/binary>>, AlgValue2),
    
    CommitterValue2 = <<"unique-id-2">>,
    CommitterHash2 = base64:encode(crypto:hash(sha256, CommitterValue2)),
    hyper_lmdb:write(StoreOpts, <<"data/", CommitterHash2/binary>>, CommitterValue2),
    
    OtherKeyValue = <<"other-key-value">>,
    OtherKeyHash = base64:encode(crypto:hash(sha256, OtherKeyValue)),
    hyper_lmdb:write(StoreOpts, <<"data/", OtherKeyHash/binary>>, OtherKeyValue),
    
    % Step 2: Create the nested structure with groups and links
    hyper_lmdb:make_group(StoreOpts, <<"root">>),
    hyper_lmdb:make_link(StoreOpts, <<"data/", TargetHash/binary>>, <<"root/target">>),
    
    hyper_lmdb:make_group(StoreOpts, <<"root/commitments">>),
    hyper_lmdb:make_group(StoreOpts, <<"root/commitments/key1">>),
    hyper_lmdb:make_link(StoreOpts, <<"data/", AlgHash1/binary>>, <<"root/commitments/key1/alg">>),
    hyper_lmdb:make_link(StoreOpts, <<"data/", CommitterHash1/binary>>, <<"root/commitments/key1/committer">>),
    
    hyper_lmdb:make_group(StoreOpts, <<"root/commitments/key2">>),
    hyper_lmdb:make_link(StoreOpts, <<"data/", AlgHash2/binary>>, <<"root/commitments/key2/alg">>),
    hyper_lmdb:make_link(StoreOpts, <<"data/", CommitterHash2/binary>>, <<"root/commitments/key2/commiter">>),
    
    hyper_lmdb:make_group(StoreOpts, <<"root/other-key">>),
    hyper_lmdb:make_link(StoreOpts, <<"data/", OtherKeyHash/binary>>, <<"root/other-key/other-key-key">>),
    
    % Step 3: Test reading the structure back
    ?assertEqual(composite, hyper_lmdb:type(StoreOpts, <<"root">>)),
    
    {ok, RootKeys} = hyper_lmdb:list(StoreOpts, <<"root">>),
    ExpectedRootKeys = [<<"commitments">>, <<"other-key">>, <<"target">>],
    ?assertEqual(lists:sort(ExpectedRootKeys), lists:sort(RootKeys)),
    
    {ok, TargetValueRead} = hyper_lmdb:read(StoreOpts, <<"root/target">>),
    ?assertEqual(<<"Foo">>, TargetValueRead),
    
    ?assertEqual(composite, hyper_lmdb:type(StoreOpts, <<"root/commitments">>)),
    ?assertEqual(composite, hyper_lmdb:type(StoreOpts, <<"root/other-key">>)),
    
    % Step 4: Test programmatic reconstruction of the nested map
    ReconstructedMap = reconstruct_map(StoreOpts, <<"root">>),
    
    % Verify the reconstructed map matches the original structure
    ?assert(maps_match(OriginalMap, ReconstructedMap)),
    cleanup(<<"/tmp/store-nested-cache">>).

%% Helper function to recursively reconstruct a map from the store
reconstruct_map(StoreOpts, Path) ->
    case hyper_lmdb:type(StoreOpts, Path) of
        composite ->
            % This is a group, reconstruct it as a map
            {ok, ImmediateChildren} = hyper_lmdb:list(StoreOpts, Path),
            maps:from_list([
                {Key, reconstruct_map(StoreOpts, <<Path/binary, "/", Key/binary>>)}
                || Key <- ImmediateChildren
            ]);
        simple ->
            % This is a simple value, read it directly
            {ok, Value} = hyper_lmdb:read(StoreOpts, Path),
            Value;
        _ ->
            % Path doesn't exist
            undefined
    end.

%% Helper to check if two maps match (simplified version of hb_message:match)
maps_match(Map1, Map2) when is_map(Map1), is_map(Map2) ->
    Keys1 = maps:keys(Map1),
    Keys2 = maps:keys(Map2),
    case lists:sort(Keys1) =:= lists:sort(Keys2) of
        false -> false;
        true ->
            lists:all(fun(Key) ->
                V1 = maps:get(Key, Map1),
                V2 = maps:get(Key, Map2),
                case {is_map(V1), is_map(V2)} of
                    {true, true} -> maps_match(V1, V2);
                    {false, false} -> V1 =:= V2;
                    _ -> false
                end
            end, Keys1)
    end;
maps_match(V1, V2) -> V1 =:= V2.

%% Test that list function resolves links correctly
list_with_link_test() ->
    StoreOpts = store_opts(<<"/tmp/store-list-link">>),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    % Create a group with some children
    hyper_lmdb:make_group(StoreOpts, <<"real-group">>),
    hyper_lmdb:write(StoreOpts, <<"real-group/child1">>, <<"value1">>),
    hyper_lmdb:write(StoreOpts, <<"real-group/child2">>, <<"value2">>),
    hyper_lmdb:write(StoreOpts, <<"real-group/child3">>, <<"value3">>),
    
    % Create a link to the group
    hyper_lmdb:make_link(StoreOpts, <<"real-group">>, <<"link-to-group">>),
    
    % List the real group to verify expected children
    {ok, RealGroupChildren} = hyper_lmdb:list(StoreOpts, <<"real-group">>),
    ExpectedChildren = [<<"child1">>, <<"child2">>, <<"child3">>],
    ?assertEqual(lists:sort(ExpectedChildren), lists:sort(RealGroupChildren)),
    
    % List via the link - should return the same children
    {ok, LinkChildren} = hyper_lmdb:list(StoreOpts, <<"link-to-group">>),
    ?assertEqual(lists:sort(ExpectedChildren), lists:sort(LinkChildren)),
    
    cleanup(<<"/tmp/store-list-link">>).

%% Test scope functions
scope_test() ->
    StoreOpts = store_opts(<<"/tmp/store-scope">>),
    ?assertEqual(local, hyper_lmdb:scope(StoreOpts)).

%% Test path and add_path functions
path_operations_test() ->
    StoreOpts = store_opts(<<"/tmp/store-path-ops">>),
    
    % Test path function with list
    PathList = [<<"a">>, <<"b">>, <<"c">>],
    PathResult = hyper_lmdb:path(StoreOpts, PathList),
    ?assertEqual(<<"a/b/c">>, PathResult),
    
    % Test path function with binary
    PathBin = <<"x/y/z">>,
    ?assertEqual(PathBin, hyper_lmdb:path(StoreOpts, PathBin)),
    
    % Test add_path with different combinations
    ?assertEqual(<<"a/b/c/d">>, hyper_lmdb:add_path(StoreOpts, <<"a/b">>, <<"c/d">>)),
    ?assertEqual(<<"a/b/c/d">>, hyper_lmdb:add_path(StoreOpts, [<<"a">>, <<"b">>], [<<"c">>, <<"d">>])),
    ?assertEqual(<<"a/b/c/d">>, hyper_lmdb:add_path(StoreOpts, <<"a/b">>, [<<"c">>, <<"d">>])),
    ?assertEqual(<<"a/b/c/d">>, hyper_lmdb:add_path(StoreOpts, [<<"a">>, <<"b">>], <<"c/d">>)),
    
    cleanup(<<"/tmp/store-path-ops">>).

%% Test resolve function
resolve_test() ->
    StoreOpts = store_opts(<<"/tmp/store-resolve">>),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    % Create some data and links
    hyper_lmdb:write(StoreOpts, <<"target/data">>, <<"value">>),
    hyper_lmdb:make_link(StoreOpts, <<"target">>, <<"link1">>),
    hyper_lmdb:make_link(StoreOpts, <<"link1">>, <<"link2">>),
    
    % Test resolve function
    ?assertEqual(<<"target">>, hyper_lmdb:resolve(StoreOpts, <<"link1">>)),
    ?assertEqual(<<"target">>, hyper_lmdb:resolve(StoreOpts, <<"link2">>)),
    ?assertEqual(<<"target/data">>, hyper_lmdb:resolve(StoreOpts, <<"target/data">>)),
    ?assertEqual(not_found, hyper_lmdb:resolve(StoreOpts, <<"nonexistent">>)),
    
    % Test resolve with path list
    ?assertEqual(<<"target/data">>, hyper_lmdb:resolve(StoreOpts, [<<"link1">>, <<"data">>])),
    
    cleanup(<<"/tmp/store-resolve">>).