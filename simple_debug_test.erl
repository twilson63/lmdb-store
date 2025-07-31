-module(simple_debug_test).
-export([run/0]).

run() ->
    hyper_lmdb:init(),
    
    StoreOpts = #{
        <<"name">> => <<"test-rel-path">>,
        <<"capacity">> => 1024 * 1024
    },
    
    os:cmd("rm -rf test-rel-path"),
    
    io:format("Starting...~n"),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    io:format("Writing...~n"),
    Result = hyper_lmdb:write(StoreOpts, <<"key">>, <<"value">>),
    io:format("Write result: ~p~n", [Result]).