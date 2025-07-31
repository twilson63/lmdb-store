-module(test_hb_store).
-export([test/0]).

test() ->
    io:format("Testing hb_store read/write with LMDB environment...~n"),
    
    % Test configuration
    StoreOpts = #{
        <<"name">> => <<"test_store">>,
        <<"path">> => <<"/tmp/test_hb_store">>,
        <<"map_size">> => 1024 * 1024 * 10  % 10MB
    },
    
    % Test 1: Start the store (should return environment resource)
    case hb_store_lmdb:start(StoreOpts) of
        {ok, EnvRef} ->
            io:format("Store started successfully, got environment reference~n"),
            
            % Test 2: Write using store_opts map
            Key1 = <<"test_key_1">>,
            Value1 = <<"test_value_1">>,
            case hb_store_lmdb:write(StoreOpts, Key1, Value1) of
                ok ->
                    io:format("Write with store_opts successful~n");
                Error ->
                    io:format("ERROR: Write with store_opts failed: ~p~n", [Error])
            end,
            
            % Test 3: Read using store_opts map
            case hb_store_lmdb:read(StoreOpts, Key1) of
                {ok, Value1} ->
                    io:format("Read with store_opts returned correct value~n");
                Error2 ->
                    io:format("ERROR: Read with store_opts failed: ~p~n", [Error2])
            end,
            
            % Test 4: Write using environment resource directly
            Key2 = <<"test_key_2">>,
            Value2 = <<"test_value_2">>,
            case hb_store_lmdb:write(EnvRef, Key2, Value2) of
                ok ->
                    io:format("Write with environment resource successful~n");
                Error3 ->
                    io:format("ERROR: Write with environment resource failed: ~p~n", [Error3])
            end,
            
            % Test 5: Read using environment resource directly
            case hb_store_lmdb:read(EnvRef, Key2) of
                {ok, Value2} ->
                    io:format("Read with environment resource returned correct value~n");
                Error4 ->
                    io:format("ERROR: Read with environment resource failed: ~p~n", [Error4])
            end,
            
            % Test 6: Cross-check - read Key2 using store_opts
            case hb_store_lmdb:read(StoreOpts, Key2) of
                {ok, Value2} ->
                    io:format("Cross-check: Both methods access same data~n");
                Error5 ->
                    io:format("ERROR: Cross-check failed: ~p~n", [Error5])
            end,
            
            % Test 7: Stop the store
            case hb_store_lmdb:stop(StoreOpts) of
                ok ->
                    io:format("Store stopped successfully~n");
                Error6 ->
                    io:format("ERROR: Stop failed: ~p~n", [Error6])
            end;
        ok ->
            io:format("ERROR: Store start returned 'ok' instead of {ok, EnvRef}~n");
        Error ->
            io:format("ERROR: Store start failed: ~p~n", [Error])
    end,
    
    % Clean up
    os:cmd("rm -rf /tmp/test_hb_store"),
    ok.