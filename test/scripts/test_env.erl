-module(test_env).
-export([test/0]).

test() ->
    io:format("Testing LMDB environment open/close...~n"),
    
    % Load the NIF
    ok = hb_store_lmdb:init(),
    io:format("NIF loaded successfully~n"),
    
    % Test opening an environment
    TestPath = <<"/tmp/test_lmdb_env">>,
    Opts = #{
        <<"map_size">> => 1024 * 1024 * 100,  % 100MB
        <<"max_dbs">> => 10,
        <<"max_readers">> => 50
    },
    
    case hb_store_lmdb:nif_env_open(TestPath, Opts) of
        {ok, EnvRef} ->
            io:format("Environment opened successfully: ~p~n", [EnvRef]),
            
            % Test closing the environment
            case hb_store_lmdb:nif_env_close(EnvRef) of
                ok ->
                    io:format("Environment closed successfully~n"),
                    ok;
                Error ->
                    io:format("Failed to close environment: ~p~n", [Error]),
                    error
            end;
        {error, Reason} ->
            io:format("Failed to open environment: ~p~n", [Reason]),
            error
    end.