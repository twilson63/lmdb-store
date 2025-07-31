#!/usr/bin/env escript
%%! -pa ebin

main(_) ->
    io:format("Testing LMDB NIF loading...~n"),
    
    % Test NIF loading
    case hyper_lmdb:start(#{<<"store-module">> => hyper_lmdb, <<"name">> => <<"test">>}) of
        ok ->
            io:format("✓ NIF loaded successfully!~n"),
            
            % Test basic operations
            Store = #{<<"store-module">> => hyper_lmdb, <<"name">> => <<"test">>},
            
            % Test write
            case hyper_lmdb:write(Store, <<"key1">>, <<"value1">>) of
                ok ->
                    io:format("✓ Write succeeded~n"),
                    
                    % Test read
                    case hyper_lmdb:read(Store, <<"key1">>) of
                        {ok, <<"value1">>} ->
                            io:format("✓ Read succeeded~n"),
                            
                            % Test group creation
                            case hyper_lmdb:make_group(Store, <<"group1">>) of
                                ok ->
                                    io:format("✓ Group creation succeeded~n"),
                                    
                                    % Clean up
                                    hyper_lmdb:stop(Store),
                                    io:format("~nAll tests passed!~n");
                                Error3 ->
                                    io:format("✗ Group creation failed: ~p~n", [Error3])
                            end;
                        Error2 ->
                            io:format("✗ Read failed: ~p~n", [Error2])
                    end;
                Error1 ->
                    io:format("✗ Write failed: ~p~n", [Error1])
            end;
        Error ->
            io:format("✗ NIF loading failed: ~p~n", [Error])
    end.