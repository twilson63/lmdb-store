#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_patha("_build/default/lib/hyper_lmdb/ebin"),
    hyper_lmdb:init(),
    
    % Clean up
    os:cmd("rm -rf /tmp/test-nif-direct"),
    
    StoreOpts = #{
        <<"name">> => <<"/tmp/test-nif-direct">>,
        <<"capacity">> => 1024 * 1024
    },
    
    % Start and capture the result
    io:format("Starting store...~n"),
    StartRes = hyper_lmdb:start(StoreOpts),
    io:format("Start result: ~p~n", [StartRes]),
    
    case StartRes of
        {ok, EnvRef} ->
            io:format("~nGot environment reference: ~p~n", [EnvRef]),
            
            % Try calling the NIF directly with the reference
            io:format("~nTrying nif_write with EnvRef...~n"),
            try
                % Note: nif_write is not exported, so this won't work
                io:format("Cannot call nif_write directly (not exported)~n")
            catch
                _:_ ->
                    io:format("As expected, nif_write is not accessible~n")
            end,
            
            % Call write normally
            io:format("~nCalling write with StoreOpts...~n"),
            WriteRes = hyper_lmdb:write(StoreOpts, <<"key">>, <<"value">>),
            io:format("Write result: ~p~n", [WriteRes]),
            
            case WriteRes of
                ok ->
                    ReadRes = hyper_lmdb:read(StoreOpts, <<"key">>),
                    io:format("Read result: ~p~n", [ReadRes]);
                _ ->
                    % Try with a simpler StoreOpts
                    io:format("~nTrying with minimal StoreOpts...~n"),
                    MinimalOpts = #{<<"name">> => <<"/tmp/test-nif-direct">>},
                    WriteRes2 = hyper_lmdb:write(MinimalOpts, <<"key2">>, <<"value2">>),
                    io:format("Minimal write result: ~p~n", [WriteRes2])
            end;
        _ ->
            io:format("Start failed!~n")
    end,
    
    % Clean up
    hyper_lmdb:stop(StoreOpts),
    
    io:format("~nTest complete.~n").