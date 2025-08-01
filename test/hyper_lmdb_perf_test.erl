-module(hyper_lmdb_perf_test).
-include_lib("eunit/include/eunit.hrl").

-define(STORE_NAME, <<"perf_test_store">>).
-define(NUM_KEYS, 1000).
-define(NUM_READS, 10000).
-define(BATCH_SIZE, 100).

store_opts() ->
    #{
        <<"name">> => ?STORE_NAME,
        <<"capacity">> => 1024 * 1024 * 1024  % 1GB
    }.

setup() ->
    StoreOpts = store_opts(),
    hyper_lmdb:reset(StoreOpts),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    StoreOpts.

cleanup(_StoreOpts) ->
    hyper_lmdb:stop(store_opts()).

perf_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(StoreOpts) ->
         [
          {"Write performance", fun() -> test_write_performance(StoreOpts) end},
          {"Read performance", fun() -> test_read_performance(StoreOpts) end},
          {"Batch write performance", fun() -> test_batch_write_performance(StoreOpts) end},
          {"Read many performance", fun() -> test_read_many_performance(StoreOpts) end},
          {"Link resolution performance", fun() -> test_link_resolution_performance(StoreOpts) end}
         ]
     end}.

test_write_performance(StoreOpts) ->
    io:format("~nWrite Performance Test~n"),
    io:format("Writing ~p keys...~n", [?NUM_KEYS]),
    
    Start = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(I) ->
        Key = integer_to_binary(I),
        Value = crypto:strong_rand_bytes(100),
        ok = hyper_lmdb:write(StoreOpts, <<"data/", Key/binary>>, Value)
    end, lists:seq(1, ?NUM_KEYS)),
    
    End = erlang:monotonic_time(microsecond),
    Duration = End - Start,
    
    io:format("Wrote ~p keys in ~p ms (~p ops/sec)~n", 
              [?NUM_KEYS, Duration div 1000, ?NUM_KEYS * 1000000 div Duration]),
    
    ?assert(Duration > 0).

test_read_performance(StoreOpts) ->
    io:format("~nRead Performance Test~n"),
    io:format("Reading ~p random keys...~n", [?NUM_READS]),
    
    % First write some test data
    lists:foreach(fun(I) ->
        Key = integer_to_binary(I),
        Value = crypto:strong_rand_bytes(100),
        ok = hyper_lmdb:write(StoreOpts, <<"data/", Key/binary>>, Value)
    end, lists:seq(1, 1000)),
    
    Start = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(_) ->
        I = rand:uniform(1000),
        Key = integer_to_binary(I),
        {ok, _} = hyper_lmdb:read(StoreOpts, <<"data/", Key/binary>>)
    end, lists:seq(1, ?NUM_READS)),
    
    End = erlang:monotonic_time(microsecond),
    Duration = End - Start,
    
    io:format("Read ~p keys in ~p ms (~p ops/sec)~n", 
              [?NUM_READS, Duration div 1000, ?NUM_READS * 1000000 div Duration]),
    
    ?assert(Duration > 0).

test_batch_write_performance(StoreOpts) ->
    io:format("~nBatch Write Performance Test~n"),
    io:format("Writing ~p keys in batches of ~p...~n", [?NUM_KEYS, ?BATCH_SIZE]),
    
    Start = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(BatchNum) ->
        {ok, Batch} = hyper_lmdb:begin_batch(StoreOpts),
        
        lists:foreach(fun(I) ->
            Key = integer_to_binary(BatchNum * ?BATCH_SIZE + I),
            Value = crypto:strong_rand_bytes(100),
            ok = hyper_lmdb:batch_write(Batch, <<"batch/", Key/binary>>, Value)
        end, lists:seq(1, ?BATCH_SIZE)),
        
        ok = hyper_lmdb:commit_batch(Batch)
    end, lists:seq(0, (?NUM_KEYS div ?BATCH_SIZE) - 1)),
    
    End = erlang:monotonic_time(microsecond),
    Duration = End - Start,
    
    io:format("Batch wrote ~p keys in ~p ms (~p ops/sec)~n", 
              [?NUM_KEYS, Duration div 1000, ?NUM_KEYS * 1000000 div Duration]),
    
    ?assert(Duration > 0).

test_read_many_performance(StoreOpts) ->
    io:format("~nRead Many Performance Test~n"),
    
    % First write some test data
    lists:foreach(fun(I) ->
        Key = integer_to_binary(I),
        Value = crypto:strong_rand_bytes(100),
        ok = hyper_lmdb:write(StoreOpts, <<"many/", Key/binary>>, Value)
    end, lists:seq(1, 1000)),
    
    % Test reading 100 keys at once
    Keys = [<<"many/", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 100)],
    
    Start = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(_) ->
        {ok, _Results} = hyper_lmdb:read_many(StoreOpts, Keys)
    end, lists:seq(1, 1000)),
    
    End = erlang:monotonic_time(microsecond),
    Duration = End - Start,
    
    io:format("Read 100 keys x 1000 times in ~p ms (~p batch ops/sec)~n", 
              [Duration div 1000, 1000 * 1000000 div Duration]),
    
    ?assert(Duration > 0).

test_link_resolution_performance(StoreOpts) ->
    io:format("~nLink Resolution Performance Test~n"),
    
    % Create a chain of links
    ok = hyper_lmdb:write(StoreOpts, <<"link_test/original">>, <<"test_value">>),
    
    lists:foreach(fun(I) ->
        Prev = if I == 1 -> <<"link_test/original">>; 
                  true -> <<"link_test/link", (integer_to_binary(I-1))/binary>> 
               end,
        Current = <<"link_test/link", (integer_to_binary(I))/binary>>,
        ok = hyper_lmdb:make_link(StoreOpts, Prev, Current)
    end, lists:seq(1, 10)),
    
    % Test resolving through the chain
    Start = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(_) ->
        {ok, <<"test_value">>} = hyper_lmdb:read(StoreOpts, <<"link_test/link10">>)
    end, lists:seq(1, 10000)),
    
    End = erlang:monotonic_time(microsecond),
    Duration = End - Start,
    
    io:format("Resolved 10-deep link chain 10000 times in ~p ms (~p ops/sec)~n", 
              [Duration div 1000, 10000 * 1000000 div Duration]),
    
    ?assert(Duration > 0).