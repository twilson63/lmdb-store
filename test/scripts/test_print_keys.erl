#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_patha("_build/default/lib/hyper_lmdb/ebin"),
    hyper_lmdb:init(),
    
    % Get current directory for comparison
    {ok, Cwd} = file:get_cwd(),
    io:format("Current directory: ~s~n~n", [Cwd]),
    
    % Test 1: Start with relative path
    RelPath = "cache-TEST/lmdb",
    io:format("Test 1: Relative path '~s'~n", [RelPath]),
    io:format("Expected absolute: ~s~n", [filename:join(Cwd, RelPath)]),
    
    os:cmd("rm -rf cache-TEST"),
    
    StoreOpts1 = #{
        <<"name">> => list_to_binary(RelPath),
        <<"capacity">> => 1024 * 1024
    },
    
    Res1 = hyper_lmdb:start(StoreOpts1),
    io:format("Start result: ~p~n~n", [Res1]),
    
    % Test 2: Start with absolute path
    AbsPath = "/tmp/test-abs-lmdb",
    io:format("Test 2: Absolute path '~s'~n", [AbsPath]),
    
    os:cmd("rm -rf " ++ AbsPath),
    
    StoreOpts2 = #{
        <<"name">> => list_to_binary(AbsPath),
        <<"capacity">> => 1024 * 1024
    },
    
    Res2 = hyper_lmdb:start(StoreOpts2),
    io:format("Start result: ~p~n", [Res2]),
    
    % Try write on absolute path (this should work)
    WriteRes = hyper_lmdb:write(StoreOpts2, <<"test">>, <<"value">>),
    io:format("Write result: ~p~n~n", [WriteRes]),
    
    % Clean up
    hyper_lmdb:stop(StoreOpts1),
    hyper_lmdb:stop(StoreOpts2),
    
    io:format("Test complete.~n").