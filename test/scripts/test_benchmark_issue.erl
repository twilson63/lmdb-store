#!/usr/bin/env escript

main(_) ->
    % Set up code paths
    true = code:add_pathz("/Users/rakis/code/m3/hb3/ebin"),
    true = code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/ebin"),
    
    % Initialize HyperBEAM
    ok = hb:init(),
    
    % Create store options
    Store = #{
        <<"store-module">> => hyper_lmdb,
        <<"name">> => <<"test-benchmark-issue">>
    },
    
    % Start the store
    hb_store:start(Store),
    
    % Test simple write and read
    io:format("Testing simple write/read...~n"),
    ok = hb_store:write(Store, <<"key-1">>, <<"value-1">>),
    case hb_store:read(Store, <<"key-1">>) of
        {ok, <<"value-1">>} -> 
            io:format("✓ Simple write/read works~n");
        Other ->
            io:format("✗ Simple write/read failed: ~p~n", [Other])
    end,
    
    % Test multiple writes and reads like the benchmark
    io:format("~nTesting benchmark pattern...~n"),
    NumKeys = 100,
    RandomData = <<"test-data">>,
    
    % Write keys
    WriteKeys = lists:map(
        fun(N) ->
            Key = << "key-", (integer_to_binary(N))/binary >>,
            ok = hb_store:write(Store, Key, RandomData),
            Key
        end,
        lists:seq(1, NumKeys)
    ),
    io:format("Wrote ~p keys~n", [NumKeys]),
    
    % Test reading all written keys
    NotFound1 = lists:foldl(
        fun(Key, Count) ->
            case hb_store:read(Store, Key) of
                {ok, RandomData} -> Count;
                _ -> 
                    io:format("Key not found: ~s~n", [Key]),
                    Count + 1
            end
        end,
        0,
        WriteKeys
    ),
    io:format("Sequential read: ~p keys not found~n", [NotFound1]),
    
    % Test random reads like the benchmark
    ReadKeys = lists:map(
        fun(_) ->
            N = rand:uniform(NumKeys),
            << "key-", (integer_to_binary(N))/binary >>
        end,
        lists:seq(1, NumKeys)
    ),
    
    NotFound2 = lists:foldl(
        fun(Key, Count) ->
            case hb_store:read(Store, Key) of
                {ok, RandomData} -> Count;
                _ -> 
                    io:format("Random key not found: ~s~n", [Key]),
                    Count + 1
            end
        end,
        0,
        ReadKeys
    ),
    io:format("Random read: ~p keys not found~n", [NotFound2]),
    
    % Stop the store
    hb_store:stop(Store),
    ok.