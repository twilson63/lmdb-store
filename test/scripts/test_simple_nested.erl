#!/usr/bin/env escript

main([]) ->
    % Set up path
    code:add_pathz("./ebin"),
    code:add_pathz("./test"),
    code:add_pathz("/Users/rakis/code/m3/hb3/ebin"),
    code:add_pathz("/Users/rakis/code/m3/hb3/test"),
    
    % Run the specific test
    io:format("Running test_store_unsigned_nested_empty_message test...~n"),
    
    % Create test store
    Store = hb_store:start(test_store(<<"/tmp/test_cache_store">>), #{}),
    
    % Run the test from hb_cache
    try
        hb_cache:test_store_unsigned_nested_empty_message(Store),
        io:format("Test passed!~n")
    catch
        Type:Reason:Stack ->
            io:format("Test failed:~n"),
            io:format("Type: ~p~n", [Type]),
            io:format("Reason: ~p~n", [Reason]),
            io:format("Stack: ~p~n", [Stack])
    end,
    
    % Cleanup
    hb_store:stop(Store),
    ok.

test_store(Name) ->
    #{
        name => Name,
        <<"store-module">> => <<"hyper_lmdb">>,
        store_app => hyper_lmdb,
        type => hyper_lmdb,
        options => #{
            directory => <<"/tmp/test_cache_store">>
        }
    }.