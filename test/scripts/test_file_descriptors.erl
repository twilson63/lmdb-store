#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_patha("_build/default/lib/hyper_lmdb/ebin"),
    hyper_lmdb:init(),
    
    io:format("Testing file descriptor leak...~n"),
    
    StoreOpts = #{
        <<"name">> => <<"cache-TEST/lmdb">>,
        <<"capacity">> => 1024 * 1024 * 100  % 100MB
    },
    
    % Clean up
    os:cmd("rm -rf cache-TEST"),
    
    % Get initial file descriptor count
    InitialFds = count_open_files(),
    io:format("Initial open files: ~p~n", [InitialFds]),
    
    % Start and stop the store many times
    io:format("Starting store 100 times...~n"),
    lists:foreach(fun(N) ->
        case N rem 10 of
            0 -> io:format("  Iteration ~p~n", [N]);
            _ -> ok
        end,
        
        StartRes = hyper_lmdb:start(StoreOpts),
        case StartRes of
            {ok, _} -> ok;
            Error -> 
                io:format("Start failed at iteration ~p: ~p~n", [N, Error]),
                halt(1)
        end,
        
        % Do some operations
        Key = <<"key", (integer_to_binary(N))/binary>>,
        ok = hyper_lmdb:write(StoreOpts, Key, <<"value">>),
        {ok, _} = hyper_lmdb:read(StoreOpts, Key)
    end, lists:seq(1, 100)),
    
    % Check file descriptor count after operations
    AfterFds = count_open_files(),
    io:format("Open files after 100 starts: ~p~n", [AfterFds]),
    io:format("File descriptor increase: ~p~n", [AfterFds - InitialFds]),
    
    % Stop the store
    hyper_lmdb:stop(StoreOpts),
    
    % Final count
    FinalFds = count_open_files(),
    io:format("Open files after stop: ~p~n", [FinalFds]),
    
    % Check if we leaked file descriptors
    if
        AfterFds - InitialFds > 10 ->
            io:format("~n[ERROR] Possible file descriptor leak detected!~n"),
            halt(1);
        true ->
            io:format("~n[OK] No significant file descriptor leak detected.~n"),
            halt(0)
    end.

count_open_files() ->
    % Get the process ID
    Pid = os:getpid(),
    % Count open files for this process
    Cmd = io_lib:format("lsof -p ~s 2>/dev/null | wc -l", [Pid]),
    Result = os:cmd(lists:flatten(Cmd)),
    case string:to_integer(string:trim(Result)) of
        {Count, _} -> Count;
        _ -> 0
    end.