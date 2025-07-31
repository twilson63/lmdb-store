#!/usr/bin/env escript
%%! -pa ebin

main(_) ->
    io:format("Testing LMDB NIF minimal...~n"),
    
    % Try to load the module
    code:load_file(hyper_lmdb),
    
    % Test basic write/read
    Store = #{<<"store-module">> => hyper_lmdb, <<"name">> => <<"test">>},
    
    io:format("Starting store...~n"),
    case hyper_lmdb:start(Store) of
        ok ->
            io:format("✓ Store started~n"),
            
            io:format("Writing key...~n"),
            case hyper_lmdb:write(Store, <<"test_key">>, <<"test_value">>) of
                ok ->
                    io:format("✓ Write successful~n"),
                    
                    io:format("Reading key...~n"),
                    case hyper_lmdb:read(Store, <<"test_key">>) of
                        {ok, <<"test_value">>} ->
                            io:format("✓ Read successful: got expected value~n"),
                            io:format("~nSUCCESS: Basic operations work!~n");
                        {ok, Other} ->
                            io:format("✗ Read returned unexpected value: ~p~n", [Other]);
                        Error ->
                            io:format("✗ Read failed: ~p~n", [Error])
                    end;
                Error ->
                    io:format("✗ Write failed: ~p~n", [Error])
            end,
            
            % Clean up
            hyper_lmdb:stop(Store);
        Error ->
            io:format("✗ Store start failed: ~p~n", [Error])
    end,
    
    % Clean up test data
    os:cmd("rm -rf lmdb").