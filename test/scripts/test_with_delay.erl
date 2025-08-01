#!/usr/bin/env escript

main(_) ->
    % Add code paths
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/ebin"),
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/priv"),
    
    % Create store
    Store = #{<<"name">> => <<"test-delay">>},
    
    % Start the store
    case hyper_lmdb:start(Store) of
        ok -> ok;
        {ok, _} -> ok;
        Error -> 
            io:format("Failed to start: ~p~n", [Error]),
            halt(1)
    end,
    
    io:format("Testing with delays between operations...~n~n"),
    
    % Create data with delays
    ok = hyper_lmdb:write(Store, <<"simple">>, <<"hello">>),
    timer:sleep(10),
    ok = hyper_lmdb:make_link(Store, <<"simple">>, <<"mylink">>),
    timer:sleep(10),
    
    % Try to read immediately
    io:format("1. Reading immediately after creation:~n"),
    case hyper_lmdb:read(Store, <<"mylink">>) of
        {ok, V1} -> io:format("   mylink = ~p~n", [V1]);
        E1 -> io:format("   mylink = ~p~n", [E1])
    end,
    
    % Force sync and wait
    io:format("~n2. After sync and 100ms wait:~n"),
    ok = hyper_lmdb:sync(Store),
    timer:sleep(100),
    
    case hyper_lmdb:read(Store, <<"mylink">>) of
        {ok, V2} -> io:format("   mylink = ~p~n", [V2]);
        E2 -> io:format("   mylink = ~p~n", [E2])
    end,
    
    % Test resolution directly
    io:format("~n3. Direct resolution:~n"),
    case hyper_lmdb:resolve(Store, <<"mylink">>) of
        Resolved when is_binary(Resolved) -> 
            io:format("   resolve(mylink) = ~p~n", [Resolved]),
            
            % Now try to read the resolved path
            case hyper_lmdb:read(Store, Resolved) of
                {ok, V3} -> io:format("   read(~p) = ~p~n", [Resolved, V3]);
                E3 -> io:format("   read(~p) = ~p~n", [Resolved, E3])
            end;
        E4 -> 
            io:format("   resolve(mylink) = ~p~n", [E4])
    end,
    
    ok.