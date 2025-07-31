#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_patha("_build/default/lib/hyper_lmdb/ebin"),
    hyper_lmdb:init(),
    
    io:format("Testing simple idempotency...~n"),
    
    StoreOpts = #{
        <<"name">> => <<"cache-TEST/lmdb">>,
        <<"capacity">> => 1024 * 1024 * 100  % 100MB
    },
    
    % Clean up
    os:cmd("rm -rf cache-TEST"),
    
    % Start multiple times and check references
    io:format("Starting store multiple times...~n"),
    Results = lists:map(fun(N) ->
        Res = hyper_lmdb:start(StoreOpts),
        io:format("  Start ~p: ~p~n", [N, Res]),
        Res
    end, lists:seq(1, 5)),
    
    % Check if all results are the same
    case lists:all(fun(R) -> R =:= hd(Results) end, Results) of
        true ->
            io:format("~n[OK] All starts returned the same result!~n");
        false ->
            io:format("~n[WARNING] Different results returned:~n"),
            lists:foreach(fun({N, R}) ->
                io:format("  Start ~p: ~p~n", [N, R])
            end, lists:zip(lists:seq(1, 5), Results))
    end,
    
    % Test that the underlying environment is shared
    io:format("~nTesting that the environment is shared...~n"),
    
    % Write with first start result
    ok = hyper_lmdb:write(StoreOpts, <<"shared-key">>, <<"shared-value">>),
    
    % Read back to verify
    {ok, Value} = hyper_lmdb:read(StoreOpts, <<"shared-key">>),
    io:format("Read value: ~p~n", [Value]),
    
    % Start again and verify we can still read the value
    {ok, _NewRef} = hyper_lmdb:start(StoreOpts),
    {ok, Value2} = hyper_lmdb:read(StoreOpts, <<"shared-key">>),
    io:format("Read value after new start: ~p~n", [Value2]),
    
    case Value =:= Value2 of
        true -> io:format("~n[OK] Environment is properly shared!~n");
        false -> io:format("~n[ERROR] Environment is not shared!~n")
    end,
    
    % Stop the store
    hyper_lmdb:stop(StoreOpts),
    
    io:format("~nTest complete.~n").