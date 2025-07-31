#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_patha("_build/default/lib/hyper_lmdb/ebin"),
    hyper_lmdb:init(),
    
    io:format("Testing idempotent start...~n"),
    
    StoreOpts = #{
        <<"name">> => <<"/tmp/test-idempotent">>,
        <<"capacity">> => 1024 * 1024 * 100  % 100MB
    },
    
    % Clean up
    hyper_lmdb:reset(StoreOpts),
    
    % Start multiple times
    io:format("Starting store first time...~n"),
    Res1 = hyper_lmdb:start(StoreOpts),
    io:format("First start result: ~p~n", [Res1]),
    
    io:format("Starting store second time...~n"),
    Res2 = hyper_lmdb:start(StoreOpts),
    io:format("Second start result: ~p~n", [Res2]),
    
    % Try to write
    io:format("Testing write...~n"),
    WriteRes = hyper_lmdb:write(StoreOpts, <<"test">>, <<"value">>),
    io:format("Write result: ~p~n", [WriteRes]),
    
    % Try to read
    io:format("Testing read...~n"),
    ReadRes = hyper_lmdb:read(StoreOpts, <<"test">>),
    io:format("Read result: ~p~n", [ReadRes]),
    
    % Stop
    hyper_lmdb:stop(StoreOpts),
    
    io:format("Test complete.~n").