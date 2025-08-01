#!/usr/bin/env escript

main(_) ->
    % Add code paths
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/ebin"),
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/priv"),
    
    % Start the store
    Store = #{<<"name">> => <<"test-sync">>},
    case hyper_lmdb:start(Store) of
        ok -> io:format("Store started~n");
        {ok, _} -> io:format("Store started with environment~n");
        Error -> 
            io:format("Failed to start: ~p~n", [Error]),
            halt(1)
    end,
    
    % Test sync function
    io:format("~nTesting sync function...~n"),
    
    % Write some data
    lists:foreach(fun(N) ->
        Key = <<"key-", (integer_to_binary(N))/binary>>,
        Value = <<"value-", (integer_to_binary(N))/binary>>,
        ok = hyper_lmdb:write(Store, Key, Value)
    end, lists:seq(1, 10)),
    
    io:format("Wrote 10 keys~n"),
    
    % Call sync
    case hyper_lmdb:sync(Store) of
        ok -> 
            io:format("Sync successful~n");
        {error, Reason} ->
            io:format("Sync failed: ~p~n", [Reason])
    end,
    
    % Verify data is readable
    NotFound = lists:foldl(fun(N, Acc) ->
        Key = <<"key-", (integer_to_binary(N))/binary>>,
        case hyper_lmdb:read(Store, Key) of
            {ok, _} -> Acc;
            _ -> Acc + 1
        end
    end, 0, lists:seq(1, 10)),
    
    io:format("Keys not found after sync: ~p~n", [NotFound]),
    
    % Test multiple operations then sync
    io:format("~nTesting composite operations...~n"),
    
    % Create a group and write to it
    ok = hyper_lmdb:make_group(Store, <<"group1">>),
    ok = hyper_lmdb:write(Store, <<"group1/field1">>, <<"data1">>),
    ok = hyper_lmdb:write(Store, <<"group1/field2">>, <<"data2">>),
    ok = hyper_lmdb:make_link(Store, <<"group1">>, <<"link1">>),
    
    % Sync after all operations
    ok = hyper_lmdb:sync(Store),
    io:format("Synced after composite operations~n"),
    
    % Verify
    case hyper_lmdb:read(Store, <<"group1/field1">>) of
        {ok, <<"data1">>} -> io:format("✓ Field1 readable~n");
        _ -> io:format("✗ Field1 not found~n")
    end,
    
    case hyper_lmdb:read(Store, <<"link1/field2">>) of
        {ok, <<"data2">>} -> io:format("✓ Link resolution works~n");
        _ -> io:format("✗ Link resolution failed~n")
    end,
    
    io:format("~nTest complete~n"),
    ok.