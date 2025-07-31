-module(test_get_put).
-export([test/0]).

test() ->
    io:format("Testing LMDB get/put operations...~n"),
    
    % Load the NIF
    ok = hyper_lmdb:init(),
    io:format("NIF loaded successfully~n"),
    
    % Test opening an environment
    TestPath = <<"/tmp/test_lmdb_get_put">>,
    Opts = #{
        <<"map_size">> => 1024 * 1024 * 100,  % 100MB
        <<"max_dbs">> => 10,
        <<"max_readers">> => 50
    },
    
    case hyper_lmdb:nif_env_open(TestPath, Opts) of
        {ok, EnvRef} ->
            io:format("Environment opened successfully~n"),
            
            % Test 1: Put and get with default database (nil)
            Key1 = <<"test_key_1">>,
            Value1 = <<"test_value_1">>,
            
            case hyper_lmdb:nif_env_put(EnvRef, nil, Key1, Value1) of
                ok ->
                    io:format("Put Key1 successfully~n"),
                    
                    case hyper_lmdb:nif_env_get(EnvRef, nil, Key1) of
                        {ok, Value1} ->
                            io:format("Get Key1 returned correct value~n");
                        {ok, Other} ->
                            io:format("ERROR: Get Key1 returned wrong value: ~p~n", [Other]);
                        Error ->
                            io:format("ERROR: Get Key1 failed: ~p~n", [Error])
                    end;
                Error ->
                    io:format("ERROR: Put Key1 failed: ~p~n", [Error])
            end,
            
            % Test 2: Put and get with named database
            DbName = <<"test_db">>,
            Key2 = <<"test_key_2">>,
            Value2 = <<"test_value_2">>,
            
            case hyper_lmdb:nif_env_put(EnvRef, DbName, Key2, Value2) of
                ok ->
                    io:format("Put Key2 in named db successfully~n"),
                    
                    case hyper_lmdb:nif_env_get(EnvRef, DbName, Key2) of
                        {ok, Value2} ->
                            io:format("Get Key2 from named db returned correct value~n");
                        {ok, Other2} ->
                            io:format("ERROR: Get Key2 returned wrong value: ~p~n", [Other2]);
                        Error2 ->
                            io:format("ERROR: Get Key2 failed: ~p~n", [Error2])
                    end;
                Error3 ->
                    io:format("ERROR: Put Key2 failed: ~p~n", [Error3])
            end,
            
            % Test 3: Get non-existent key
            case hyper_lmdb:nif_env_get(EnvRef, nil, <<"non_existent_key">>) of
                not_found ->
                    io:format("Get non-existent key correctly returned not_found~n");
                Other3 ->
                    io:format("ERROR: Get non-existent key returned: ~p~n", [Other3])
            end,
            
            % Test 4: Cross-database isolation
            case hyper_lmdb:nif_env_get(EnvRef, DbName, Key1) of
                not_found ->
                    io:format("Cross-database isolation verified (Key1 not in named db)~n");
                Other4 ->
                    io:format("ERROR: Cross-database isolation failed: ~p~n", [Other4])
            end,
            
            % Close the environment
            case hyper_lmdb:nif_env_close(EnvRef) of
                ok ->
                    io:format("Environment closed successfully~n"),
                    ok;
                Error4 ->
                    io:format("Failed to close environment: ~p~n", [Error4]),
                    error
            end;
        {error, Reason} ->
            io:format("Failed to open environment: ~p~n", [Reason]),
            error
    end.