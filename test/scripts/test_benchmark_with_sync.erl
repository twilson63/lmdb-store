#!/usr/bin/env escript

main(_) ->
    % Add code paths
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/ebin"),
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/priv"),
    
    % Create store like the benchmark test
    Store = #{
        <<"store-module">> => hyper_lmdb,
        <<"name">> => <<"cache-mainnet/hyper_lmdb">>
    },
    
    % Start the store
    ok = hb_store:start(Store),
    io:format("Store started~n"),
    
    % Test simple key writes like the benchmark
    io:format("~nTesting benchmark-style key writes...~n"),
    NumKeys = 1000,
    RandomData = <<"test-data-", (integer_to_binary(rand:uniform(1000000)))/binary>>,
    
    % Write keys
    lists:foreach(fun(N) ->
        Key = <<"key-", (integer_to_binary(N))/binary>>,
        ok = hb_store:write(Store, Key, RandomData)
    end, lists:seq(1, NumKeys)),
    
    io:format("Wrote ~p keys~n", [NumKeys]),
    
    % Call sync explicitly
    io:format("Calling sync...~n"),
    ok = hyper_lmdb:sync(Store),
    
    % Now read random keys
    ReadOps = 1000,
    NotFound = lists:foldl(fun(_, Acc) ->
        N = rand:uniform(NumKeys),
        Key = <<"key-", (integer_to_binary(N))/binary>>,
        case hb_store:read(Store, Key) of
            {ok, RandomData} -> Acc;
            {ok, _Other} -> 
                io:format("Wrong data for ~s~n", [Key]),
                Acc + 1;
            _ -> 
                io:format("Not found: ~s~n", [Key]),
                Acc + 1
        end
    end, 0, lists:seq(1, ReadOps)),
    
    io:format("~nSimple keys - Not found: ~p out of ~p reads~n", [NotFound, ReadOps]),
    
    % Now test message writes
    io:format("~nTesting message writes...~n"),
    Opts = #{ store => Store, priv_wallet => hb:wallet() },
    
    % Write a few messages
    NumMsgs = 10,
    MsgPairs = lists:map(fun(N) ->
        Msg = #{
            <<"process">> => <<"test-process">>,
            <<"slot">> => N,
            <<"message">> => hb_message:commit(
                #{
                    <<"body">> => <<"test message ", (integer_to_binary(N))/binary>>,
                    <<"field1">> => <<"value1">>,
                    <<"field2">> => <<"value2">>
                },
                Opts
            )
        },
        {ok, MsgID} = hb_cache:write(Msg, Opts),
        {MsgID, Msg}
    end, lists:seq(1, NumMsgs)),
    
    io:format("Wrote ~p messages~n", [NumMsgs]),
    
    % Sync after all message writes
    io:format("Calling sync after message writes...~n"),
    ok = hyper_lmdb:sync(Store),
    
    % Small sleep to be sure
    timer:sleep(100),
    
    % Try to read them back
    MsgNotFound = lists:foldl(fun({MsgID, OrigMsg}, Acc) ->
        case hb_cache:read(MsgID, Opts) of
            {ok, ReadMsg} ->
                case hb_cache:ensure_all_loaded(ReadMsg, Opts) of
                    OrigMsg -> 
                        io:format("✓ Message ~p~n", [element(2, lists:keyfind(<<"slot">>, 1, OrigMsg))]),
                        Acc;
                    _ -> 
                        io:format("✗ Message ~p: data mismatch~n", [element(2, lists:keyfind(<<"slot">>, 1, OrigMsg))]),
                        Acc + 1
                end;
            Error -> 
                io:format("✗ Message ~p: ~p~n", [element(2, lists:keyfind(<<"slot">>, 1, OrigMsg)), Error]),
                Acc + 1
        end
    end, 0, MsgPairs),
    
    io:format("~nMessages - Not found/mismatched: ~p out of ~p~n", [MsgNotFound, NumMsgs]),
    
    % Stop the store
    hb_store:stop(Store),
    
    io:format("~nTest complete~n"),
    ok.