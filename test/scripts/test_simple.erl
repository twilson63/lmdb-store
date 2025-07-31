#!/usr/bin/env escript
%%! -pa ebin

main(_) ->
    io:format("Testing LMDB NIF...~n"),
    
    % First check if module loads
    case code:ensure_loaded(hyper_lmdb) of
        {module, hyper_lmdb} ->
            io:format("✓ Module loaded~n"),
            
            % Check module info
            io:format("Module info: ~p~n", [hyper_lmdb:module_info()]),
            
            % Try calling a simple NIF function  
            try
                Result = hyper_lmdb:nif_start(#{<<"name">> => <<"test">>, <<"store-module">> => hyper_lmdb}),
                io:format("NIF call result: ~p~n", [Result])
            catch
                error:Reason ->
                    io:format("NIF call failed: ~p~n", [Reason]),
                    io:format("Stack trace: ~p~n", [erlang:get_stacktrace()])
            end;
        Error ->
            io:format("✗ Module loading failed: ~p~n", [Error])
    end.