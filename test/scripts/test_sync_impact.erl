#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/hyper_lmdb/ebin

main(_) ->
    io:format("~nTesting NO_SYNC Performance Impact~n"),
    io:format("==================================~n~n"),
    
    StoreOpts = #{<<"name">> => <<"sync_test">>, <<"capacity">> => 1024 * 1024 * 1024},
    NumKeys = 5000,
    TestData = [{integer_to_binary(I), crypto:strong_rand_bytes(1000)} || I <- lists:seq(1, NumKeys)],
    
    %% Test with current flags (including NO_SYNC)
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    Start1 = erlang:monotonic_time(microsecond),
    lists:foreach(fun({Key, Value}) ->
        ok = hyper_lmdb:write(StoreOpts, Key, Value)
    end, TestData),
    % Force sync at the end
    hyper_lmdb:sync(StoreOpts),
    End1 = erlang:monotonic_time(microsecond),
    NoSyncTime = End1 - Start1,
    
    io:format("With NO_SYNC (sync at end):     ~p ms (~p ops/sec)~n", 
              [NoSyncTime div 1000, NumKeys * 1000000 div NoSyncTime]),
    
    %% For comparison, let's test batch writes which also avoid per-write sync
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    Start2 = erlang:monotonic_time(microsecond),
    lists:foreach(fun(Batch) ->
        {ok, B} = hyper_lmdb:begin_batch(StoreOpts),
        lists:foreach(fun({Key, Value}) ->
            ok = hyper_lmdb:batch_write(B, Key, Value)
        end, Batch),
        ok = hyper_lmdb:commit_batch(B)
    end, split_into_batches(TestData, 100)),
    End2 = erlang:monotonic_time(microsecond),
    BatchTime = End2 - Start2,
    
    io:format("Batch writes (100 per batch):   ~p ms (~p ops/sec)~n", 
              [BatchTime div 1000, NumKeys * 1000000 div BatchTime]),
    
    io:format("~nNO_SYNC provides ~.1fx speedup over batching~n", [BatchTime / NoSyncTime]),
    
    hyper_lmdb:stop(StoreOpts),
    ok.

split_into_batches(List, Size) ->
    split_into_batches(List, Size, []).

split_into_batches([], _Size, Acc) ->
    lists:reverse(Acc);
split_into_batches(List, Size, Acc) ->
    {Batch, Rest} = lists:split(min(Size, length(List)), List),
    split_into_batches(Rest, Size, [Batch | Acc]).