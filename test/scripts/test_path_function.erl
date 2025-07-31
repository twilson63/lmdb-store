#!/usr/bin/env escript

main(_) ->
    code:add_path("ebin"),
    io:format("=== Testing path function ===~n~n"),
    
    StoreOpts = #{
        <<"name">> => <<"test_path_store">>,
        <<"path">> => <<"/tmp/test_path_function">>
    },
    
    % Cleanup and start
    os:cmd("rm -rf /tmp/test_path_function"),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    try
        % Test 1: Path of existing data
        io:format("1. Testing path of existing data...~n"),
        ok = hyper_lmdb:write(StoreOpts, <<"test-key">>, <<"test-value">>),
        case hyper_lmdb:path(StoreOpts, <<"test-key">>) of
            <<"test-key">> ->
                io:format("   ✓ Path returns the key itself for direct data~n");
            Other1 ->
                io:format("   ✗ ERROR: Expected <<\"test-key\">>, got ~p~n", [Other1]),
                halt(1)
        end,
        
        % Test 2: Path of non-existent key
        io:format("~n2. Testing path of non-existent key...~n"),
        case hyper_lmdb:path(StoreOpts, <<"non-existent">>) of
            not_found ->
                io:format("   ✓ Path returns not_found for non-existent keys~n");
            Other2 ->
                io:format("   ✗ ERROR: Expected not_found, got ~p~n", [Other2]),
                halt(1)
        end,
        
        % Test 3: Path through links
        io:format("~n3. Testing path resolution through links...~n"),
        ok = hyper_lmdb:write(StoreOpts, <<"target">>, <<"value">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"target">>, <<"link1">>),
        case hyper_lmdb:path(StoreOpts, <<"link1">>) of
            <<"target">> ->
                io:format("   ✓ Path correctly resolves link to target~n");
            Other3 ->
                io:format("   ✗ ERROR: Expected <<\"target\">>, got ~p~n", [Other3]),
                halt(1)
        end,
        
        % Test 4: Path through link chains
        io:format("~n4. Testing path through link chains...~n"),
        ok = hyper_lmdb:make_link(StoreOpts, <<"link1">>, <<"link2">>),
        case hyper_lmdb:path(StoreOpts, <<"link2">>) of
            <<"target">> ->
                io:format("   ✓ Path correctly resolves link chain to final target~n");
            Other4 ->
                io:format("   ✗ ERROR: Expected <<\"target\">>, got ~p~n", [Other4]),
                halt(1)
        end,
        
        % Test 5: Path of groups
        io:format("~n5. Testing path of groups...~n"),
        ok = hyper_lmdb:make_group(StoreOpts, <<"my-group">>),
        case hyper_lmdb:path(StoreOpts, <<"my-group">>) of
            <<"my-group">> ->
                io:format("   ✓ Path returns the group path itself~n");
            Other5 ->
                io:format("   ✗ ERROR: Expected <<\"my-group\">>, got ~p~n", [Other5]),
                halt(1)
        end,
        
        % Test 6: Path through hierarchical links
        io:format("~n6. Testing hierarchical path resolution...~n"),
        ok = hyper_lmdb:make_group(StoreOpts, <<"dir1">>),
        ok = hyper_lmdb:write(StoreOpts, [<<"dir1">>, <<"file">>], <<"data">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"dir1">>, <<"link-to-dir">>),
        case hyper_lmdb:path(StoreOpts, [<<"link-to-dir">>, <<"file">>]) of
            <<"dir1/file">> ->
                io:format("   ✓ Path correctly resolves hierarchical path through link~n");
            Other6 ->
                io:format("   ✗ ERROR: Expected <<\"dir1/file\">>, got ~p~n", [Other6]),
                halt(1)
        end,
        
        io:format("~n✅ All path tests passed!~n")
        
    after
        hyper_lmdb:stop(StoreOpts),
        os:cmd("rm -rf /tmp/test_path_function")
    end.