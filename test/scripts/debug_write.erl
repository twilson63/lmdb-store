#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_patha("_build/default/lib/hyper_lmdb/ebin"),
    hyper_lmdb:init(),
    
    io:format("Debug write issue...~n"),
    
    % First, let's use absolute path to avoid path issues
    {ok, Cwd} = file:get_cwd(),
    DbPath = filename:join([Cwd, "test-db"]),
    
    StoreOpts = #{
        <<"name">> => list_to_binary(DbPath),
        <<"capacity">> => 1024 * 1024 * 10  % 10MB
    },
    
    % Clean up
    os:cmd("rm -rf " ++ DbPath),
    
    % Start
    io:format("Starting with path: ~s~n", [DbPath]),
    StartRes = hyper_lmdb:start(StoreOpts),
    io:format("Start result: ~p~n", [StartRes]),
    
    % Let's test if the simple example from earlier works
    SimpleOpts = #{
        <<"name">> => <<"/tmp/test-debug">>,
        <<"capacity">> => 1024 * 1024 * 10
    },
    
    os:cmd("rm -rf /tmp/test-debug"),
    
    io:format("~nTrying simple path /tmp/test-debug...~n"),
    SimpleStart = hyper_lmdb:start(SimpleOpts),
    io:format("Simple start result: ~p~n", [SimpleStart]),
    
    SimpleWrite = hyper_lmdb:write(SimpleOpts, <<"key">>, <<"value">>),
    io:format("Simple write result: ~p~n", [SimpleWrite]),
    
    case SimpleWrite of
        ok ->
            SimpleRead = hyper_lmdb:read(SimpleOpts, <<"key">>),
            io:format("Simple read result: ~p~n", [SimpleRead]);
        _ ->
            io:format("Write failed!~n")
    end,
    
    % Stop
    hyper_lmdb:stop(SimpleOpts),
    
    io:format("~nTest complete.~n").