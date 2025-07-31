#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_patha("_build/default/lib/hyper_lmdb/ebin"),
    hyper_lmdb:init(),
    
    io:format("Testing canonical key issue...~n"),
    
    % Get current directory
    {ok, Cwd} = file:get_cwd(),
    io:format("Current directory: ~s~n~n", [Cwd]),
    
    % Test with the same relative path used in tests
    RelPath = "./test-lmdb",
    StoreOpts = #{
        <<"store-module">> => hyper_lmdb,
        <<"name">> => list_to_binary(RelPath)
    },
    
    % Clean up
    os:cmd("rm -rf " ++ RelPath),
    
    % Start
    io:format("Starting with relative path '~s'...~n", [RelPath]),
    StartRes = hyper_lmdb:start(StoreOpts),
    io:format("Start result: ~p~n", [StartRes]),
    
    % Check if directory was created
    DirExists = filelib:is_dir(RelPath),
    io:format("Directory exists: ~p~n", [DirExists]),
    
    % Try to write
    io:format("~nAttempting write...~n"),
    WriteRes = hyper_lmdb:write(StoreOpts, <<"key">>, <<"value">>),
    io:format("Write result: ~p~n", [WriteRes]),
    
    % Try with absolute path to same directory
    AbsPath = filename:join(Cwd, RelPath),
    AbsOpts = #{
        <<"store-module">> => hyper_lmdb,
        <<"name">> => list_to_binary(AbsPath)
    },
    
    io:format("~nTrying with absolute path '~s'...~n", [AbsPath]),
    WriteRes2 = hyper_lmdb:write(AbsOpts, <<"key2">>, <<"value2">>),
    io:format("Write result: ~p~n", [WriteRes2]),
    
    % Clean up
    hyper_lmdb:stop(StoreOpts),
    
    io:format("~nTest complete.~n").