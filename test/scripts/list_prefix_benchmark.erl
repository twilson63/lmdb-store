#!/usr/bin/env escript

main(_) ->
    code:add_path("ebin"),
    io:format("=== List Prefix Performance Benchmark ===~n~n"),
    
    StoreOpts = #{
        <<"name">> => <<"prefix_bench">>,
        <<"path">> => <<"/tmp/elmdb_prefix_bench">>
    },
    
    % Cleanup and start
    os:cmd("rm -rf /tmp/elmdb_prefix_bench"),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    try
        benchmark_different_sizes(StoreOpts),
        benchmark_prefix_depths(StoreOpts),
        benchmark_cursor_pagination(StoreOpts)
    after
        hyper_lmdb:stop(StoreOpts),
        os:cmd("rm -rf /tmp/elmdb_prefix_bench")
    end.

benchmark_different_sizes(StoreOpts) ->
    io:format("1. Benchmark with different dataset sizes:~n"),
    io:format("   Size    | Write Time | List All | List Prefix~n"),
    io:format("   --------|------------|----------|------------~n"),
    
    lists:foreach(fun(Size) ->
        % Clear the store
        lists:foreach(fun(I) ->
            Key = list_to_binary(io_lib:format("bench/~10..0B", [I])),
            hyper_lmdb:write(StoreOpts, Key, <<"x">>)
        end, lists:seq(1, Size)),
        
        % Benchmark list all
        T1 = erlang:monotonic_time(microsecond),
        {ok, _} = hyper_lmdb:list_prefix(StoreOpts, <<"">>),
        ListAllTime = erlang:monotonic_time(microsecond) - T1,
        
        % Benchmark list prefix
        T2 = erlang:monotonic_time(microsecond),
        {ok, _} = hyper_lmdb:list_prefix(StoreOpts, <<"bench">>),
        ListPrefixTime = erlang:monotonic_time(microsecond) - T2,
        
        io:format("   ~7w | ~10w | ~8w | ~10w~n", 
                  [Size, "-", ListAllTime, ListPrefixTime])
    end, [100, 1000, 10000, 50000]),
    
    io:format("~n").

benchmark_prefix_depths(StoreOpts) ->
    io:format("2. Benchmark different prefix depths:~n"),
    
    % Create hierarchical data
    lists:foreach(fun(I) ->
        Key = list_to_binary(io_lib:format("org/dept~3..0B/team~2..0B/user~4..0B", 
                                           [I rem 10, I rem 5, I])),
        hyper_lmdb:write(StoreOpts, Key, <<"data">>)
    end, lists:seq(1, 10000)),
    
    Prefixes = [
        {<<"org">>, "Top level"},
        {<<"org/dept001">>, "Department"},
        {<<"org/dept001/team01">>, "Team"},
        {<<"org/dept001/team01/user">>, "User prefix"}
    ],
    
    io:format("   Prefix                     | Count | Time (μs)~n"),
    io:format("   ---------------------------|-------|----------~n"),
    
    lists:foreach(fun({Prefix, Desc}) ->
        T = erlang:monotonic_time(microsecond),
        {ok, Keys} = hyper_lmdb:list_prefix(StoreOpts, Prefix),
        Time = erlang:monotonic_time(microsecond) - T,
        io:format("   ~-27s| ~5w | ~8w~n", [Desc, length(Keys), Time])
    end, Prefixes),
    
    io:format("~n").

benchmark_cursor_pagination(StoreOpts) ->
    io:format("3. Benchmark cursor pagination:~n"),
    
    % Create data for pagination
    lists:foreach(fun(I) ->
        Key = list_to_binary(io_lib:format("page/item~6..0B", [I])),
        hyper_lmdb:write(StoreOpts, Key, <<"data">>)
    end, lists:seq(1, 100000)),
    
    io:format("   Page Size | Pages | Total Time | Avg/Page~n"),
    io:format("   ----------|-------|------------|----------~n"),
    
    lists:foreach(fun(PageSize) ->
        T = erlang:monotonic_time(microsecond),
        Pages = paginate(StoreOpts, <<"page">>, PageSize),
        TotalTime = erlang:monotonic_time(microsecond) - T,
        AvgTime = TotalTime div Pages,
        io:format("   ~9w | ~5w | ~10w | ~8w~n", 
                  [PageSize, Pages, TotalTime, AvgTime])
    end, [10, 100, 1000, 10000]),
    
    io:format("~n").

paginate(StoreOpts, Prefix, PageSize) ->
    paginate(StoreOpts, Prefix, PageSize, undefined, 0).

paginate(StoreOpts, Prefix, PageSize, Cursor, Count) ->
    Opts = case Cursor of
        undefined -> #{limit => PageSize, return_cursor => true};
        _ -> #{limit => PageSize, cursor => Cursor, return_cursor => true}
    end,
    
    case hyper_lmdb:list_prefix(StoreOpts, Prefix, Opts) of
        {ok, [], _} -> Count;
        {ok, Keys, NewCursor} when length(Keys) < PageSize -> Count + 1;
        {ok, _, NewCursor} -> paginate(StoreOpts, Prefix, PageSize, NewCursor, Count + 1)
    end.