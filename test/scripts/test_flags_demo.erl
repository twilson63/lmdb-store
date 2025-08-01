#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/hyper_lmdb/ebin

main(_) ->
    io:format("~nDemonstrating Configurable LMDB Flags~n"),
    io:format("=====================================~n~n"),
    
    %% Test 1: Default flags (performance mode)
    io:format("1. Default flags (performance optimized)~n"),
    StoreOpts1 = #{<<"name">> => <<"test_flags_perf">>, <<"capacity">> => 100 * 1024 * 1024},
    hyper_lmdb:reset(StoreOpts1),
    {ok, _} = hyper_lmdb:start(StoreOpts1),
    
    Start1 = erlang:monotonic_time(microsecond),
    lists:foreach(fun(I) ->
        Key = integer_to_binary(I),
        ok = hyper_lmdb:write(StoreOpts1, Key, crypto:strong_rand_bytes(100))
    end, lists:seq(1, 1000)),
    End1 = erlang:monotonic_time(microsecond),
    PerfTime = End1 - Start1,
    
    io:format("   1000 writes in ~p ms (~p ops/sec)~n", 
              [PerfTime div 1000, 1000 * 1000000 div PerfTime]),
    hyper_lmdb:stop(StoreOpts1),
    
    %% Test 2: Durability mode (all sync flags disabled)
    io:format("~n2. Durability mode (no_sync=false)~n"),
    StoreOpts2 = #{
        <<"name">> => <<"test_flags_durable">>, 
        <<"capacity">> => 100 * 1024 * 1024,
        <<"no_sync">> => false,
        <<"map_async">> => false,
        <<"no_meta_sync">> => false
    },
    hyper_lmdb:reset(StoreOpts2),
    {ok, _} = hyper_lmdb:start(StoreOpts2),
    
    Start2 = erlang:monotonic_time(microsecond),
    lists:foreach(fun(I) ->
        Key = integer_to_binary(I),
        ok = hyper_lmdb:write(StoreOpts2, Key, crypto:strong_rand_bytes(100))
    end, lists:seq(1, 100)), % Only 100 writes because it's much slower
    End2 = erlang:monotonic_time(microsecond),
    DurableTime = End2 - Start2,
    
    io:format("   100 writes in ~p ms (~p ops/sec)~n", 
              [DurableTime div 1000, 100 * 1000000 div DurableTime]),
    hyper_lmdb:stop(StoreOpts2),
    
    %% Compare
    io:format("~n3. Performance comparison~n"),
    AvgPerfWrite = PerfTime / 1000,  % microseconds per write
    AvgDurableWrite = DurableTime / 100,
    io:format("   Average write time:~n"),
    io:format("   - Performance mode: ~.1f μs/write~n", [AvgPerfWrite]),
    io:format("   - Durability mode:  ~.1f μs/write~n", [AvgDurableWrite]),
    io:format("   - Speedup: ~.1fx faster with NO_SYNC~n", [AvgDurableWrite / AvgPerfWrite]),
    
    %% Test 3: Custom flags
    io:format("~n4. Custom flag combinations~n"),
    StoreOpts3 = #{
        <<"name">> => <<"test_flags_custom">>,
        <<"capacity">> => 100 * 1024 * 1024,
        <<"no_sync">> => true,      % No fsync
        <<"map_async">> => false,    % But synchronous memory mapping
        <<"no_readahead">> => false  % Enable OS readahead
    },
    hyper_lmdb:reset(StoreOpts3),
    {ok, _} = hyper_lmdb:start(StoreOpts3),
    
    ok = hyper_lmdb:write(StoreOpts3, <<"test">>, <<"Custom flags work!">>),
    {ok, Value} = hyper_lmdb:read(StoreOpts3, <<"test">>),
    io:format("   Custom flags test: ~s~n", [Value]),
    hyper_lmdb:stop(StoreOpts3),
    
    io:format("~nAll flags are now configurable via store options!~n"),
    ok.