-module(elmdb_perf_test).
-include_lib("eunit/include/eunit.hrl").

%% Performance tests that can be run with eunit
-define(SMALL_OPS, 1000).
-define(MEDIUM_OPS, 10000).

performance_test_() ->
    {timeout, 300, [  % 5 minute timeout for all performance tests
        {"Small dataset performance", fun small_dataset_test/0},
        {"Medium dataset performance", fun medium_dataset_test/0},
        {"Large value performance", fun large_value_test/0},
        {"Concurrent access performance", fun concurrent_test/0}
    ]}.

setup() ->
    StoreOpts = #{
        <<"name">> => <<"perf_test">>,
        <<"path">> => <<"/tmp/elmdb_perf_test">>,
        <<"map_size">> => 1024 * 1024 * 1024  % 1GB
    },
    os:cmd("rm -rf /tmp/elmdb_perf_test"),
    case hyper_lmdb:start(StoreOpts) of
        {ok, _EnvRef} -> StoreOpts;  % Always use StoreOpts
        ok -> StoreOpts
    end.

cleanup(Store) ->
    StoreOpts = #{
        <<"name">> => <<"perf_test">>,
        <<"path">> => <<"/tmp/elmdb_perf_test">>
    },
    hyper_lmdb:stop(StoreOpts),
    os:cmd("rm -rf /tmp/elmdb_perf_test").

small_dataset_test() ->
    Store = setup(),
    try
        % Generate test data
        Keys = [<<"key", (integer_to_binary(I))/binary>> || I <- lists:seq(1, ?SMALL_OPS)],
        Values = [crypto:strong_rand_bytes(100) || _ <- lists:seq(1, ?SMALL_OPS)],
        
        % Write performance
        WriteStart = erlang:monotonic_time(microsecond),
        lists:foreach(fun({K, V}) ->
            ?assertEqual(ok, hyper_lmdb:write(Store, K, V))
        end, lists:zip(Keys, Values)),
        WriteTime = erlang:monotonic_time(microsecond) - WriteStart,
        WriteOpsPerSec = ?SMALL_OPS * 1000000 / WriteTime,
        
        % Read performance
        ReadStart = erlang:monotonic_time(microsecond),
        lists:foreach(fun(K) ->
            ?assertMatch({ok, _}, hyper_lmdb:read(Store, K))
        end, Keys),
        ReadTime = erlang:monotonic_time(microsecond) - ReadStart,
        ReadOpsPerSec = ?SMALL_OPS * 1000000 / ReadTime,
        
        io:format("~nSmall dataset (~p ops):~n", [?SMALL_OPS]),
        io:format("  Write: ~.0f ops/sec (~.2f ms total)~n", [WriteOpsPerSec, WriteTime/1000]),
        io:format("  Read:  ~.0f ops/sec (~.2f ms total)~n", [ReadOpsPerSec, ReadTime/1000]),
        
        % Performance assertions
        ?assert(WriteOpsPerSec > 10000),  % Should handle >10k writes/sec
        ?assert(ReadOpsPerSec > 50000)    % Should handle >50k reads/sec
    after
        cleanup(Store)
    end.

medium_dataset_test() ->
    Store = setup(),
    try
        % Test with medium dataset
        N = ?MEDIUM_OPS,
        
        % Sequential write test
        WriteStart = erlang:monotonic_time(microsecond),
        lists:foreach(fun(I) ->
            Key = <<"med_key_", (integer_to_binary(I))/binary>>,
            Value = crypto:strong_rand_bytes(1024),
            ?assertEqual(ok, hyper_lmdb:write(Store, Key, Value))
        end, lists:seq(1, N)),
        WriteTime = erlang:monotonic_time(microsecond) - WriteStart,
        
        % Random read test
        ReadKeys = [<<"med_key_", (integer_to_binary(rand:uniform(N)))/binary>> 
                    || _ <- lists:seq(1, N)],
        ReadStart = erlang:monotonic_time(microsecond),
        lists:foreach(fun(K) ->
            ?assertMatch({ok, _}, hyper_lmdb:read(Store, K))
        end, ReadKeys),
        ReadTime = erlang:monotonic_time(microsecond) - ReadStart,
        
        io:format("~nMedium dataset (~p ops):~n", [N]),
        io:format("  Sequential Write: ~.0f ops/sec~n", [N * 1000000 / WriteTime]),
        io:format("  Random Read:      ~.0f ops/sec~n", [N * 1000000 / ReadTime])
    after
        cleanup(Store)
    end.

large_value_test() ->
    Store = setup(),
    try
        % Test with various value sizes
        Sizes = [1024, 4096, 16384, 65536, 262144],  % 1KB to 256KB
        
        io:format("~nLarge value performance:~n"),
        io:format("Size     | Write ops/sec | Read ops/sec~n"),
        io:format("---------|---------------|-------------~n"),
        
        lists:foreach(fun(Size) ->
            N = min(1000, 10485760 div Size),  % Limit total data to ~10MB
            Keys = [<<"large_", (integer_to_binary(I))/binary>> || I <- lists:seq(1, N)],
            Values = [crypto:strong_rand_bytes(Size) || _ <- lists:seq(1, N)],
            
            % Write test
            WriteStart = erlang:monotonic_time(microsecond),
            lists:foreach(fun({K, V}) ->
                ?assertEqual(ok, hyper_lmdb:write(Store, K, V))
            end, lists:zip(Keys, Values)),
            WriteTime = erlang:monotonic_time(microsecond) - WriteStart,
            
            % Read test
            ReadStart = erlang:monotonic_time(microsecond),
            lists:foreach(fun(K) ->
                ?assertMatch({ok, _}, hyper_lmdb:read(Store, K))
            end, Keys),
            ReadTime = erlang:monotonic_time(microsecond) - ReadStart,
            
            io:format("~8w | ~13.0f | ~12.0f~n", 
                     [Size, 
                      N * 1000000 / WriteTime,
                      N * 1000000 / ReadTime])
        end, Sizes)
    after
        cleanup(Store)
    end.

concurrent_test() ->
    Store = setup(),
    try
        N = 1000,
        NumWorkers = 10,
        OpsPerWorker = N div NumWorkers,
        
        % Pre-populate some data
        lists:foreach(fun(I) ->
            Key = <<"concurrent_", (integer_to_binary(I))/binary>>,
            Value = crypto:strong_rand_bytes(512),
            ?assertEqual(ok, hyper_lmdb:write(Store, Key, Value))
        end, lists:seq(1, N)),
        
        % Concurrent read test
        Parent = self(),
        ReadStart = erlang:monotonic_time(microsecond),
        
        ReadPids = [spawn_link(fun() ->
            lists:foreach(fun(J) ->
                Key = <<"concurrent_", (integer_to_binary(J))/binary>>,
                ?assertMatch({ok, _}, hyper_lmdb:read(Store, Key))
            end, lists:seq((I-1)*OpsPerWorker + 1, I*OpsPerWorker)),
            Parent ! {done, self()}
        end) || I <- lists:seq(1, NumWorkers)],
        
        wait_for_workers(ReadPids),
        ReadTime = erlang:monotonic_time(microsecond) - ReadStart,
        
        % Concurrent mixed operations test
        MixedStart = erlang:monotonic_time(microsecond),
        
        MixedPids = [spawn_link(fun() ->
            lists:foreach(fun(J) ->
                case rand:uniform(5) of
                    1 -> % Write
                        Key = <<"mixed_", (integer_to_binary(J))/binary>>,
                        Value = crypto:strong_rand_bytes(512),
                        ?assertEqual(ok, hyper_lmdb:write(Store, Key, Value));
                    _ -> % Read
                        Key = <<"concurrent_", (integer_to_binary(rand:uniform(N)))/binary>>,
                        ?assertMatch({ok, _}, hyper_lmdb:read(Store, Key))
                end
            end, lists:seq((I-1)*OpsPerWorker + 1, I*OpsPerWorker)),
            Parent ! {done, self()}
        end) || I <- lists:seq(1, NumWorkers)],
        
        wait_for_workers(MixedPids),
        MixedTime = erlang:monotonic_time(microsecond) - MixedStart,
        
        io:format("~nConcurrent operations (~p workers):~n", [NumWorkers]),
        io:format("  Concurrent reads:  ~.0f ops/sec~n", [N * 1000000 / ReadTime]),
        io:format("  Mixed operations:  ~.0f ops/sec~n", [N * 1000000 / MixedTime])
    after
        cleanup(Store)
    end.

wait_for_workers([]) -> ok;
wait_for_workers(Pids) ->
    receive
        {done, Pid} ->
            wait_for_workers(lists:delete(Pid, Pids))
    after 5000 ->
        error(timeout)
    end.