#!/usr/bin/env escript

main(_) ->
    % Add code paths for hyper_lmdb NIF
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/ebin"),
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/priv"),
    
    % Load the NIF
    case hyper_lmdb:start(#{<<"name">> => <<"test-direct">>}) of
        ok -> 
            io:format("Store started successfully~n");
        {ok, _Env} ->
            io:format("Store started with environment~n");
        Error -> 
            io:format("Failed to start store: ~p~n", [Error]),
            halt(1)
    end,
    
    Store = #{<<"name">> => <<"test-direct">>},
    
    % Test a few operations
    io:format("~nTesting write/read operations...~n"),
    
    % Write and read test
    Key1 = <<"key-1">>,
    Value1 = <<"value-1">>,
    
    case hyper_lmdb:write(Store, Key1, Value1) of
        ok -> 
            io:format("Write successful for ~s~n", [Key1]);
        WriteErr1 ->
            io:format("Write failed: ~p~n", [WriteErr1])
    end,
    
    case hyper_lmdb:read(Store, Key1) of
        {ok, ReadValue1} ->
            io:format("Read successful: ~s = ~s~n", [Key1, ReadValue1]);
        not_found ->
            io:format("Read failed: ~s not found~n", [Key1]);
        ReadErr ->
            io:format("Read error: ~p~n", [ReadErr])
    end,
    
    % Test with keys like the benchmark
    io:format("~nTesting benchmark-style keys...~n"),
    TestKeys = [
        <<"key-1">>,
        <<"key-10">>,
        <<"key-100">>,
        <<"key-1000">>,
        <<"key-10000">>,
        <<"key-99999">>,
        <<"key-100000">>
    ],
    
    lists:foreach(fun(Key) ->
        Value = <<"test-data">>,
        case hyper_lmdb:write(Store, Key, Value) of
            ok -> 
                case hyper_lmdb:read(Store, Key) of
                    {ok, Value} ->
                        io:format("✓ ~s~n", [Key]);
                    {ok, Other} ->
                        io:format("✗ ~s: value mismatch ~p~n", [Key, Other]);
                    not_found ->
                        io:format("✗ ~s: not found after write~n", [Key]);
                    Err ->
                        io:format("✗ ~s: read error ~p~n", [Key, Err])
                end;
            WriteErr2 ->
                io:format("✗ ~s: write error ~p~n", [Key, WriteErr2])
        end
    end, TestKeys),
    
    io:format("~nTest complete~n"),
    ok.