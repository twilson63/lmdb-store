#!/usr/bin/env escript

main(_) ->
    code:add_path("ebin"),
    io:format("=== Testing Hierarchical Path Resolution Through Links ===~n~n"),
    
    StoreOpts = #{
        <<"name">> => <<"test_store">>,
        <<"path">> => <<"/tmp/test_hierarchical_link">>
    },
    
    % Cleanup and start
    os:cmd("rm -rf /tmp/test_hierarchical_link"),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    try
        % Test case from HyperBEAM
        io:format("1. Creating directory structure...~n"),
        ok = hyper_lmdb:make_group(StoreOpts, <<"test-dir1">>),
        io:format("   Created group: test-dir1~n"),
        
        io:format("2. Writing file in directory...~n"),
        ok = hyper_lmdb:write(StoreOpts, [<<"test-dir1">>, <<"test-file">>], <<"test-data">>),
        io:format("   Written: test-dir1/test-file = test-data~n"),
        
        io:format("3. Creating link to directory...~n"),
        ok = hyper_lmdb:make_link(StoreOpts, [<<"test-dir1">>], <<"test-link">>),
        io:format("   Created link: test-link -> test-dir1~n"),
        
        io:format("4. Reading through the link...~n"),
        case hyper_lmdb:read(StoreOpts, [<<"test-link">>, <<"test-file">>]) of
            {ok, <<"test-data">>} ->
                io:format("   ✓ Successfully read test-data through link!~n"),
                io:format("~nTEST PASSED: Hierarchical path resolution through links works correctly.~n");
            {ok, Other} ->
                io:format("   ✗ ERROR: Expected <<\"test-data\">>, got ~p~n", [Other]),
                io:format("~nTEST FAILED~n"),
                halt(1);
            Error ->
                io:format("   ✗ ERROR: ~p~n", [Error]),
                io:format("~nTEST FAILED~n"),
                halt(1)
        end,
        
        % Additional tests
        io:format("~n5. Testing type resolution through link...~n"),
        case hyper_lmdb:type(StoreOpts, [<<"test-link">>, <<"test-file">>]) of
            {ok, simple} ->
                io:format("   ✓ Type correctly resolved as 'simple' through link~n");
            TypeResult ->
                io:format("   ✗ Type resolution failed: ~p~n", [TypeResult])
        end,
        
        io:format("~n6. Testing list through link...~n"),
        case hyper_lmdb:list(StoreOpts, <<"test-link">>) of
            {ok, Children} ->
                case lists:member(<<"test-file">>, Children) of
                    true ->
                        io:format("   ✓ List correctly shows children through link: ~p~n", [Children]);
                    false ->
                        io:format("   ✗ List failed to show test-file through link: ~p~n", [Children])
                end;
            Error2 ->
                io:format("   ✗ List failed: ~p~n", [Error2])
        end
        
    after
        hyper_lmdb:stop(StoreOpts),
        os:cmd("rm -rf /tmp/test_hierarchical_link")
    end.