#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_patha("_build/default/lib/hyper_lmdb/ebin"),
    hyper_lmdb:init(),
    
    io:format("Testing with directory pre-created...~n"),
    
    % Clean up and create directory first
    os:cmd("rm -rf test-with-mkdir"),
    os:cmd("mkdir -p test-with-mkdir"),
    
    StoreOpts = #{
        <<"name">> => <<"test-with-mkdir">>,
        <<"capacity">> => 1024 * 1024
    },
    
    % Start
    io:format("Starting store...~n"),
    {ok, _Ref} = hyper_lmdb:start(StoreOpts),
    
    % Try write
    io:format("Attempting write...~n"),
    WriteRes = hyper_lmdb:write(StoreOpts, <<"test">>, <<"value">>),
    io:format("Write result: ~p~n", [WriteRes]),
    
    case WriteRes of
        ok ->
            ReadRes = hyper_lmdb:read(StoreOpts, <<"test">>),
            io:format("Read result: ~p~n", [ReadRes]);
        _ ->
            io:format("Write failed!~n")
    end,
    
    % Clean up
    hyper_lmdb:stop(StoreOpts),
    
    io:format("~nTest complete.~n").