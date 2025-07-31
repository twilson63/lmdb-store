#!/usr/bin/env escript

main(_) ->
    code:add_path("ebin"),
    code:add_path("_build/default/lib/hyper_lmdb/ebin"),
    io:format("=== Testing resolve function ===~n~n"),
    
    StoreOpts = #{
        <<"name">> => <<"test_resolve_store">>,
        <<"path">> => <<"/tmp/test_resolve_function">>
    },
    
    % Cleanup and start
    os:cmd("rm -rf /tmp/test_resolve_function"),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    try
        % Test 1: Resolve existing data path
        io:format("1. Testing resolve of existing data...~n"),
        ok = hyper_lmdb:write(StoreOpts, <<"test-key">>, <<"test-value">>),
        case hyper_lmdb:resolve(StoreOpts, <<"test-key">>) of
            <<"test-key">> ->
                io:format("   ✓ Resolve returns the key itself for direct data~n");
            Other1 ->
                io:format("   ✗ ERROR: Expected <<\"test-key\">>, got ~p~n", [Other1]),
                halt(1)
        end,
        
        % Test 2: Resolve non-existent key
        io:format("~n2. Testing resolve of non-existent key...~n"),
        case hyper_lmdb:resolve(StoreOpts, <<"non-existent">>) of
            not_found ->
                io:format("   ✓ Resolve returns not_found for non-existent keys~n");
            Other2 ->
                io:format("   ✗ ERROR: Expected not_found, got ~p~n", [Other2]),
                halt(1)
        end,
        
        % Test 3: Resolve through links
        io:format("~n3. Testing resolve through links...~n"),
        ok = hyper_lmdb:write(StoreOpts, <<"target">>, <<"value">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"target">>, <<"link1">>),
        case hyper_lmdb:resolve(StoreOpts, <<"link1">>) of
            <<"target">> ->
                io:format("   ✓ Resolve correctly resolves link to target~n");
            Other3 ->
                io:format("   ✗ ERROR: Expected <<\"target\">>, got ~p~n", [Other3]),
                halt(1)
        end,
        
        % Test 4: Resolve through link chains
        io:format("~n4. Testing resolve through link chains...~n"),
        ok = hyper_lmdb:make_link(StoreOpts, <<"link1">>, <<"link2">>),
        case hyper_lmdb:resolve(StoreOpts, <<"link2">>) of
            <<"target">> ->
                io:format("   ✓ Resolve correctly resolves link chain to final target~n");
            Other4 ->
                io:format("   ✗ ERROR: Expected <<\"target\">>, got ~p~n", [Other4]),
                halt(1)
        end,
        
        % Test 5: Resolve groups
        io:format("~n5. Testing resolve of groups...~n"),
        ok = hyper_lmdb:make_group(StoreOpts, <<"my-group">>),
        case hyper_lmdb:resolve(StoreOpts, <<"my-group">>) of
            <<"my-group">> ->
                io:format("   ✓ Resolve returns the group path itself~n");
            Other5 ->
                io:format("   ✗ ERROR: Expected <<\"my-group\">>, got ~p~n", [Other5]),
                halt(1)
        end,
        
        % Test 6: Resolve hierarchical paths through links
        io:format("~n6. Testing hierarchical path resolution...~n"),
        ok = hyper_lmdb:make_group(StoreOpts, <<"dir1">>),
        ok = hyper_lmdb:write(StoreOpts, [<<"dir1">>, <<"file">>], <<"data">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"dir1">>, <<"link-to-dir">>),
        case hyper_lmdb:resolve(StoreOpts, [<<"link-to-dir">>, <<"file">>]) of
            <<"dir1/file">> ->
                io:format("   ✓ Resolve correctly resolves hierarchical path through link~n");
            Other6 ->
                io:format("   ✗ ERROR: Expected <<\"dir1/file\">>, got ~p~n", [Other6]),
                halt(1)
        end,
        
        % Test 7: Resolve with list paths
        io:format("~n7. Testing resolve with list paths...~n"),
        case hyper_lmdb:resolve(StoreOpts, [<<"dir1">>, <<"file">>]) of
            <<"dir1/file">> ->
                io:format("   ✓ Resolve handles list paths correctly~n");
            Other7 ->
                io:format("   ✗ ERROR: Expected <<\"dir1/file\">>, got ~p~n", [Other7]),
                halt(1)
        end,
        
        % Test 8: Resolve broken link
        io:format("~n8. Testing resolve of broken link...~n"),
        ok = hyper_lmdb:make_link(StoreOpts, <<"non-existent-target">>, <<"broken-link">>),
        case hyper_lmdb:resolve(StoreOpts, <<"broken-link">>) of
            not_found ->
                io:format("   ✓ Resolve returns not_found for broken links~n");
            Other8 ->
                io:format("   ✗ ERROR: Expected not_found, got ~p~n", [Other8]),
                halt(1)
        end,
        
        io:format("~n✅ All resolve tests passed!~n")
        
    after
        hyper_lmdb:stop(StoreOpts),
        os:cmd("rm -rf /tmp/test_resolve_function")
    end.