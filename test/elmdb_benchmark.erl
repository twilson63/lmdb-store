-module(elmdb_benchmark).
-export([run/0, run/1]).

-record(bench_config, {
    store_name = <<"benchmark_store">>,
    store_path = <<"/tmp/elmdb_benchmark">>,
    num_operations = 100000,
    key_size = 32,
    value_size = 1024,
    num_readers = 4,
    num_writers = 2,
    batch_size = 1000
}).

%% Main entry point
run() ->
    run(#bench_config{}).

run(Config) when is_list(Config) ->
    run(config_from_proplist(Config, #bench_config{}));
run(Config) ->
    io:format("~n=== ELMDB Benchmark Suite ===~n~n"),
    
    % Setup
    cleanup(Config),
    {ok, EnvRef} = setup_store(Config),
    
    try
        % Run benchmarks
        bench_sequential_writes(EnvRef, Config),
        bench_sequential_reads(EnvRef, Config),
        bench_random_reads(EnvRef, Config),
        bench_mixed_operations(EnvRef, Config),
        bench_batch_writes(EnvRef, Config),
        bench_concurrent_reads(EnvRef, Config),
        bench_concurrent_mixed(EnvRef, Config),
        
        % Size benchmarks
        bench_various_value_sizes(EnvRef, Config),
        
        io:format("~n=== Benchmark Complete ===~n")
    after
        teardown_store(Config),
        cleanup(Config)
    end.

%% Setup and teardown
setup_store(#bench_config{store_name = Name, store_path = Path} = Config) ->
    StoreOpts = #{
        <<"name">> => Name,
        <<"path">> => Path,
        <<"map_size">> => integer_to_binary(1024 * 1024 * 1024 * 10),  % 10GB as binary
        <<"max_readers">> => integer_to_binary(Config#bench_config.num_readers * 2)
    },
    io:format("Starting store with opts: ~p~n", [StoreOpts]),
    Result = hyper_lmdb:start(StoreOpts),
    io:format("Store start result: ~p~n", [Result]),
    case Result of
        {ok, EnvRef} -> {ok, EnvRef};  % Return the environment reference
        ok -> {ok, StoreOpts};  % Fallback to StoreOpts if no ref returned
        Error -> 
            io:format("Failed to start store: ~p~n", [Error]),
            error({failed_to_start_store, Error})
    end.

teardown_store(#bench_config{store_name = Name, store_path = Path}) ->
    StoreOpts = #{<<"name">> => Name, <<"path">> => Path},
    hyper_lmdb:stop(StoreOpts).

cleanup(#bench_config{store_path = Path}) ->
    os:cmd(binary_to_list(<<"rm -rf ", Path/binary>>)).

%% Benchmark: Sequential Writes
bench_sequential_writes(Store, #bench_config{num_operations = N} = Config) ->
    io:format("~nSequential Writes (~p operations)~n", [N]),
    io:format("Key size: ~p bytes, Value size: ~p bytes~n", 
              [Config#bench_config.key_size, Config#bench_config.value_size]),
    
    Keys = generate_keys(N, Config#bench_config.key_size),
    Values = generate_values(N, Config#bench_config.value_size),
    
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun({K, V}) ->
            ok = hyper_lmdb:write(Store, K, V)
        end, lists:zip(Keys, Values))
    end),
    
    report_results("Sequential Writes", N, Time).

%% Benchmark: Sequential Reads
bench_sequential_reads(Store, #bench_config{num_operations = N} = Config) ->
    io:format("~nSequential Reads (~p operations)~n", [N]),
    
    Keys = generate_keys(N, Config#bench_config.key_size),
    
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(K) ->
            {ok, _} = hyper_lmdb:read(Store, K)
        end, Keys)
    end),
    
    report_results("Sequential Reads", N, Time).

%% Benchmark: Random Reads
bench_random_reads(Store, #bench_config{num_operations = N} = Config) ->
    io:format("~nRandom Reads (~p operations)~n", [N]),
    
    AllKeys = generate_keys(N, Config#bench_config.key_size),
    RandomKeys = [lists:nth(rand:uniform(N), AllKeys) || _ <- lists:seq(1, N)],
    
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(K) ->
            {ok, _} = hyper_lmdb:read(Store, K)
        end, RandomKeys)
    end),
    
    report_results("Random Reads", N, Time).

%% Benchmark: Mixed Operations (80% reads, 20% writes)
bench_mixed_operations(Store, #bench_config{num_operations = N} = Config) ->
    io:format("~nMixed Operations (~p operations, 80% read, 20% write)~n", [N]),
    
    Keys = generate_keys(N, Config#bench_config.key_size),
    Values = generate_values(N div 5, Config#bench_config.value_size),
    
    Operations = generate_mixed_operations(Keys, Values, N),
    
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun
            ({read, K}) -> {ok, _} = hyper_lmdb:read(Store, K);
            ({write, K, V}) -> ok = hyper_lmdb:write(Store, K, V)
        end, Operations)
    end),
    
    report_results("Mixed Operations", N, Time).

%% Benchmark: Batch Writes
bench_batch_writes(Store, #bench_config{num_operations = N, batch_size = BatchSize} = Config) ->
    io:format("~nBatch Writes (~p operations, batch size ~p)~n", [N, BatchSize]),
    
    Keys = generate_keys(N, Config#bench_config.key_size),
    Values = generate_values(N, Config#bench_config.value_size),
    Batches = create_batches(lists:zip(Keys, Values), BatchSize),
    
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(Batch) ->
            lists:foreach(fun({K, V}) ->
                ok = hyper_lmdb:write(Store, K, V)
            end, Batch)
        end, Batches)
    end),
    
    report_results("Batch Writes", N, Time).

%% Benchmark: Concurrent Reads
bench_concurrent_reads(Store, #bench_config{num_operations = N, num_readers = NumReaders} = Config) ->
    io:format("~nConcurrent Reads (~p operations, ~p readers)~n", [N, NumReaders]),
    
    Keys = generate_keys(N, Config#bench_config.key_size),
    OpsPerReader = N div NumReaders,
    
    Parent = self(),
    
    {Time, _} = timer:tc(fun() ->
        Pids = [spawn_link(fun() ->
            reader_worker(Store, lists:sublist(Keys, (I-1)*OpsPerReader + 1, OpsPerReader), Parent)
        end) || I <- lists:seq(1, NumReaders)],
        
        wait_for_workers(Pids)
    end),
    
    report_results("Concurrent Reads", N, Time).

%% Benchmark: Concurrent Mixed Operations
bench_concurrent_mixed(Store, #bench_config{num_operations = N, num_readers = NumReaders, num_writers = NumWriters} = Config) ->
    TotalWorkers = NumReaders + NumWriters,
    io:format("~nConcurrent Mixed (~p operations, ~p readers, ~p writers)~n", 
              [N, NumReaders, NumWriters]),
    
    Keys = generate_keys(N, Config#bench_config.key_size),
    Values = generate_values(N, Config#bench_config.value_size),  % Generate enough values
    OpsPerWorker = N div TotalWorkers,
    
    Parent = self(),
    
    {Time, _} = timer:tc(fun() ->
        % Spawn readers
        ReaderPids = [spawn_link(fun() ->
            reader_worker(Store, lists:sublist(Keys, (I-1)*OpsPerWorker + 1, OpsPerWorker), Parent)
        end) || I <- lists:seq(1, NumReaders)],
        
        % Spawn writers
        WriterPids = [spawn_link(fun() ->
            writer_worker(Store, 
                         lists:sublist(Keys, NumReaders*OpsPerWorker + (I-1)*OpsPerWorker + 1, OpsPerWorker),
                         lists:sublist(Values, NumReaders*OpsPerWorker + (I-1)*OpsPerWorker + 1, OpsPerWorker),
                         Parent)
        end) || I <- lists:seq(1, NumWriters)],
        
        wait_for_workers(ReaderPids ++ WriterPids)
    end),
    
    report_results("Concurrent Mixed", N, Time).

%% Benchmark: Various Value Sizes
bench_various_value_sizes(Store, Config) ->
    io:format("~nValue Size Impact (10,000 operations each)~n"),
    io:format("Size (bytes) | Writes (ops/sec) | Reads (ops/sec)~n"),
    io:format("-------------|------------------|----------------~n"),
    
    Sizes = [64, 256, 1024, 4096, 16384, 65536],
    N = 10000,
    
    lists:foreach(fun(Size) ->
        Keys = generate_keys(N, Config#bench_config.key_size),
        Values = generate_values(N, Size),
        
        % Write benchmark
        {WriteTime, _} = timer:tc(fun() ->
            lists:foreach(fun({K, V}) ->
                ok = hyper_lmdb:write(Store, K, V)
            end, lists:zip(Keys, Values))
        end),
        
        % Read benchmark
        {ReadTime, _} = timer:tc(fun() ->
            lists:foreach(fun(K) ->
                {ok, _} = hyper_lmdb:read(Store, K)
            end, Keys)
        end),
        
        WriteOps = (N * 1000000) div WriteTime,
        ReadOps = (N * 1000000) div ReadTime,
        
        io:format("~12w | ~16w | ~15w~n", [Size, WriteOps, ReadOps])
    end, Sizes).

%% Worker processes
reader_worker(Store, Keys, Parent) ->
    lists:foreach(fun(K) ->
        {ok, _} = hyper_lmdb:read(Store, K)
    end, Keys),
    Parent ! {done, self()}.

writer_worker(Store, Keys, Values, Parent) ->
    lists:foreach(fun({K, V}) ->
        ok = hyper_lmdb:write(Store, K, V)
    end, lists:zip(Keys, Values)),
    Parent ! {done, self()}.

wait_for_workers([]) -> ok;
wait_for_workers(Pids) ->
    receive
        {done, Pid} ->
            wait_for_workers(lists:delete(Pid, Pids))
    after 30000 ->
        error(timeout)
    end.

%% Helper functions
generate_keys(N, _Size) ->
    [<<"key_", (integer_to_binary(I))/binary>> || I <- lists:seq(1, N)].

generate_values(N, Size) ->
    [generate_random_binary(Size, I + 1000000) || I <- lists:seq(1, N)].

generate_random_binary(Size, Seed) ->
    rand:seed(exs1024s, {Seed, Seed, Seed}),
    list_to_binary([rand:uniform(256) - 1 || _ <- lists:seq(1, Size)]).

generate_mixed_operations(Keys, Values, N) ->
    ValueIndex = 1,
    generate_mixed_operations(Keys, Values, N, ValueIndex, []).

generate_mixed_operations(_, _, 0, _, Acc) ->
    lists:reverse(Acc);
generate_mixed_operations(Keys, Values, N, ValueIndex, Acc) ->
    KeyIndex = rand:uniform(length(Keys)),
    Key = lists:nth(KeyIndex, Keys),
    
    case rand:uniform(5) of  % 20% chance for write
        1 ->
            Value = lists:nth((ValueIndex rem length(Values)) + 1, Values),
            generate_mixed_operations(Keys, Values, N-1, ValueIndex+1, [{write, Key, Value}|Acc]);
        _ ->
            generate_mixed_operations(Keys, Values, N-1, ValueIndex, [{read, Key}|Acc])
    end.

create_batches(List, BatchSize) ->
    create_batches(List, BatchSize, []).

create_batches([], _BatchSize, Acc) ->
    lists:reverse(Acc);
create_batches(List, BatchSize, Acc) ->
    {Batch, Rest} = lists:split(min(BatchSize, length(List)), List),
    create_batches(Rest, BatchSize, [Batch|Acc]).

report_results(Name, Operations, TimeMicros) ->
    TimeMillis = TimeMicros / 1000,
    TimeSecs = TimeMicros / 1000000,
    OpsPerSec = Operations / TimeSecs,
    
    io:format("~s: ~p ms total, ~p ops/sec~n",
              [Name, round(TimeMillis), round(OpsPerSec)]).

config_from_proplist([], Config) -> Config;
config_from_proplist([{num_operations, N}|Rest], Config) ->
    config_from_proplist(Rest, Config#bench_config{num_operations = N});
config_from_proplist([{key_size, S}|Rest], Config) ->
    config_from_proplist(Rest, Config#bench_config{key_size = S});
config_from_proplist([{value_size, S}|Rest], Config) ->
    config_from_proplist(Rest, Config#bench_config{value_size = S});
config_from_proplist([{num_readers, N}|Rest], Config) ->
    config_from_proplist(Rest, Config#bench_config{num_readers = N});
config_from_proplist([{num_writers, N}|Rest], Config) ->
    config_from_proplist(Rest, Config#bench_config{num_writers = N});
config_from_proplist([{batch_size, S}|Rest], Config) ->
    config_from_proplist(Rest, Config#bench_config{batch_size = S});
config_from_proplist([_|Rest], Config) ->
    config_from_proplist(Rest, Config).