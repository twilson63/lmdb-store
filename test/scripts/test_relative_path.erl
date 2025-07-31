#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_patha("_build/default/lib/hyper_lmdb/ebin"),
    hyper_lmdb:init(),
    
    io:format("Testing relative path issue...~n"),
    
    % Test with relative path
    RelativeOpts = #{
        <<"name">> => <<"cache-TEST/lmdb">>,
        <<"capacity">> => 1024 * 1024 * 10
    },
    
    % Clean up
    os:cmd("rm -rf cache-TEST"),
    
    io:format("~nTest 1: Starting with relative path...~n"),
    Res1 = hyper_lmdb:start(RelativeOpts),
    io:format("Start result: ~p~n", [Res1]),
    
    % Try to write immediately
    io:format("Writing to relative path store...~n"),
    WriteRes1 = hyper_lmdb:write(RelativeOpts, <<"test">>, <<"value">>),
    io:format("Write result: ~p~n", [WriteRes1]),
    
    % Now let's see what's in the ENVIRONMENTS map by starting with absolute path
    {ok, Cwd} = file:get_cwd(),
    AbsPath = filename:join([Cwd, "cache-TEST/lmdb"]),
    
    AbsoluteOpts = #{
        <<"name">> => list_to_binary(AbsPath),
        <<"capacity">> => 1024 * 1024 * 10
    },
    
    io:format("~nTest 2: Starting with absolute path...~n"),
    io:format("Absolute path: ~s~n", [AbsPath]),
    Res2 = hyper_lmdb:start(AbsoluteOpts),
    io:format("Start result: ~p~n", [Res2]),
    
    % Try to write with absolute path
    io:format("Writing to absolute path store...~n"),
    WriteRes2 = hyper_lmdb:write(AbsoluteOpts, <<"test2">>, <<"value2">>),
    io:format("Write result: ~p~n", [WriteRes2]),
    
    % Check if they share the same environment
    case WriteRes2 of
        ok ->
            io:format("~nChecking if environments are shared...~n"),
            % Try to read from relative using key written with absolute
            ReadRes1 = hyper_lmdb:read(RelativeOpts, <<"test2">>),
            io:format("Read test2 from relative opts: ~p~n", [ReadRes1]),
            
            % Try to read from absolute using key written with relative
            case WriteRes1 of
                ok ->
                    ReadRes2 = hyper_lmdb:read(AbsoluteOpts, <<"test">>),
                    io:format("Read test from absolute opts: ~p~n", [ReadRes2]);
                _ ->
                    io:format("Relative write failed, skipping cross-read~n")
            end;
        _ ->
            io:format("Absolute write failed~n")
    end,
    
    % Clean up
    hyper_lmdb:stop(RelativeOpts),
    hyper_lmdb:stop(AbsoluteOpts),
    
    io:format("~nTest complete.~n").