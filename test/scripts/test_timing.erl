#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_patha("_build/default/lib/hyper_lmdb/ebin"),
    hyper_lmdb:init(),
    
    % Clean up
    os:cmd("rm -rf cache-TEST"),
    
    StoreOpts = #{
        <<"name">> => <<"cache-TEST/lmdb">>,
        <<"capacity">> => 1024 * 1024
    },
    
    % Start
    io:format("Starting store...~n"),
    {ok, _Ref} = hyper_lmdb:start(StoreOpts),
    
    % Check if directory was created
    DirExists1 = filelib:is_dir("cache-TEST/lmdb"),
    io:format("Directory exists after start: ~p~n", [DirExists1]),
    
    % Now that directory exists, try starting again
    io:format("~nStarting store again (directory now exists)...~n"),
    {ok, _Ref2} = hyper_lmdb:start(StoreOpts),
    
    % Try write
    io:format("~nAttempting write...~n"),
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