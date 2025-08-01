#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/hyper_lmdb/ebin

main(_) ->
    StoreOpts = #{<<"name">> => <<"perf_test_fast">>, <<"capacity">> => 1024 * 1024 * 1024},
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    io:format("~nPerformance Comparison: Regular vs Fast Operations~n"),
    io:format("==================================================~n~n"),
    
    NumKeys = 10000,
    TestData = prepare_test_data(NumKeys),
    
    %% Test 1: Write Performance
    io:format("1. Write Performance (10,000 keys)~n"),
    io:format("----------------------------------~n"),
    
    % Regular writes
    Start1 = erlang:monotonic_time(microsecond),
    lists:foreach(fun({Key, Value}) ->
        ok = hyper_lmdb:write(StoreOpts, Key, Value)
    end, TestData),
    End1 = erlang:monotonic_time(microsecond),
    RegularWriteTime = End1 - Start1,
    io:format("Regular write: ~p ms (~p ops/sec)~n", 
              [RegularWriteTime div 1000, NumKeys * 1000000 div RegularWriteTime]),
    
    % Fast writes (no parent group creation, no sync)
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    Start2 = erlang:monotonic_time(microsecond),
    lists:foreach(fun({Key, Value}) ->
        ok = hyper_lmdb:write_fast(StoreOpts, Key, Value)
    end, TestData),
    End2 = erlang:monotonic_time(microsecond),
    FastWriteTime = End2 - Start2,
    io:format("Fast write:    ~p ms (~p ops/sec)~n", 
              [FastWriteTime div 1000, NumKeys * 1000000 div FastWriteTime]),
    io:format("Speedup:       ~.2fx faster~n~n", [RegularWriteTime / FastWriteTime]),
    
    %% Test 2: Read Performance
    io:format("2. Read Performance (10,000 reads)~n"),
    io:format("----------------------------------~n"),
    
    % Write data first using fast writes
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    lists:foreach(fun({Key, Value}) ->
        ok = hyper_lmdb:write_fast(StoreOpts, Key, Value)
    end, TestData),
    
    % Regular reads
    Start3 = erlang:monotonic_time(microsecond),
    lists:foreach(fun({Key, _}) ->
        {ok, _} = hyper_lmdb:read(StoreOpts, Key)
    end, TestData),
    End3 = erlang:monotonic_time(microsecond),
    RegularReadTime = End3 - Start3,
    io:format("Regular read:  ~p ms (~p ops/sec)~n", 
              [RegularReadTime div 1000, NumKeys * 1000000 div RegularReadTime]),
    
    % Fast reads (no link resolution)
    Start4 = erlang:monotonic_time(microsecond),
    lists:foreach(fun({Key, _}) ->
        {ok, _} = hyper_lmdb:read_fast(StoreOpts, Key)
    end, TestData),
    End4 = erlang:monotonic_time(microsecond),
    FastReadTime = End4 - Start4,
    io:format("Fast read:     ~p ms (~p ops/sec)~n", 
              [FastReadTime div 1000, NumKeys * 1000000 div FastReadTime]),
    io:format("Speedup:       ~.2fx faster~n~n", [RegularReadTime / FastReadTime]),
    
    %% Test 3: Bulk Read Performance
    io:format("3. Bulk Read Performance (100 keys at a time)~n"),
    io:format("----------------------------------------------~n"),
    
    Keys = [Key || {Key, _} <- lists:sublist(TestData, 100)],
    NumBulkReads = 1000,
    
    % Regular read_many
    Start5 = erlang:monotonic_time(microsecond),
    lists:foreach(fun(_) ->
        {ok, _} = hyper_lmdb:read_many(StoreOpts, Keys)
    end, lists:seq(1, NumBulkReads)),
    End5 = erlang:monotonic_time(microsecond),
    RegularBulkTime = End5 - Start5,
    io:format("Regular bulk:  ~p ms (~p batch ops/sec)~n", 
              [RegularBulkTime div 1000, NumBulkReads * 1000000 div RegularBulkTime]),
    
    % Fast read_many
    Start6 = erlang:monotonic_time(microsecond),
    lists:foreach(fun(_) ->
        {ok, _} = hyper_lmdb:read_many_fast(StoreOpts, Keys)
    end, lists:seq(1, NumBulkReads)),
    End6 = erlang:monotonic_time(microsecond),
    FastBulkTime = End6 - Start6,
    io:format("Fast bulk:     ~p ms (~p batch ops/sec)~n", 
              [FastBulkTime div 1000, NumBulkReads * 1000000 div FastBulkTime]),
    io:format("Speedup:       ~.2fx faster~n~n", [RegularBulkTime / FastBulkTime]),
    
    %% Summary
    io:format("Summary~n"),
    io:format("-------~n"),
    io:format("Write speedup: ~.2fx~n", [RegularWriteTime / FastWriteTime]),
    io:format("Read speedup:  ~.2fx~n", [RegularReadTime / FastReadTime]),
    io:format("Bulk speedup:  ~.2fx~n", [RegularBulkTime / FastBulkTime]),
    io:format("~n"),
    
    hyper_lmdb:stop(StoreOpts),
    ok.

prepare_test_data(NumKeys) ->
    [{integer_to_binary(I), crypto:strong_rand_bytes(100)} || I <- lists:seq(1, NumKeys)].