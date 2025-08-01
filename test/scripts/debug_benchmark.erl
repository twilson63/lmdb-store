#!/usr/bin/env escript

% This script simulates the benchmark test to see which keys are failing

main(_) ->
    % Simulate key generation like the benchmark
    WriteOps = 100000,
    ReadOps = 100000,
    
    io:format("Generating keys for WriteOps=~p, ReadOps=~p~n", [WriteOps, ReadOps]),
    
    % Keys generated for writing (from line 500-504)
    WriteKeys = lists:map(
        fun(N) ->
            << "key-", (integer_to_binary(N))/binary >>
        end,
        lists:seq(1, ReadOps)  % NOTE: This uses ReadOps, not WriteOps!
    ),
    
    io:format("Number of keys written: ~p~n", [length(WriteKeys)]),
    io:format("First few write keys: ~p~n", [lists:sublist(WriteKeys, 5)]),
    io:format("Last few write keys: ~p~n", [lists:sublist(lists:reverse(WriteKeys), 5)]),
    
    % Keys generated for reading (from line 526-531)
    rand:seed(exsplus, {1, 2, 3}), % Fixed seed for reproducibility
    ReadKeys = lists:map(
        fun(_) ->
            N = rand:uniform(ReadOps),
            << "key-", (integer_to_binary(N))/binary >>
        end,
        lists:seq(1, ReadOps)
    ),
    
    % Find keys that would not be found
    UniqueReadKeys = lists:usort(ReadKeys),
    NotWritten = [K || K <- UniqueReadKeys, not lists:member(K, WriteKeys)],
    
    io:format("~nNumber of unique read keys: ~p~n", [length(UniqueReadKeys)]),
    io:format("Read keys NOT in write set: ~p~n", [NotWritten]),
    io:format("Count of missing keys: ~p~n", [length(NotWritten)]),
    
    % Check if any read keys are out of bounds
    MaxN = lists:max([binary_to_integer(binary:part(K, 4, byte_size(K) - 4)) || K <- UniqueReadKeys]),
    MinN = lists:min([binary_to_integer(binary:part(K, 4, byte_size(K) - 4)) || K <- UniqueReadKeys]),
    io:format("~nRead key range: ~p to ~p~n", [MinN, MaxN]),
    io:format("Write key range: 1 to ~p~n", [ReadOps]),
    
    ok.