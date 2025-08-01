#!/usr/bin/env escript

main(_) ->
    % Add code paths
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/ebin"),
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/priv"),
    
    % Create store
    Store = #{<<"name">> => <<"test-link-debug">>},
    
    % Start the store
    case hyper_lmdb:start(Store) of
        ok -> ok;
        {ok, _} -> ok;
        Error -> 
            io:format("Failed to start: ~p~n", [Error]),
            halt(1)
    end,
    
    io:format("Debugging link resolution...~n~n"),
    
    % Create a simple setup
    io:format("1. Creating test data...~n"),
    ok = hyper_lmdb:make_group(Store, <<"target">>),
    ok = hyper_lmdb:write(Store, <<"target/data">>, <<"hello">>),
    ok = hyper_lmdb:make_link(Store, <<"target">>, <<"mylink">>),
    
    % Test direct reads
    io:format("~n2. Direct reads:~n"),
    case hyper_lmdb:read(Store, <<"target/data">>) of
        {ok, Value1} -> io:format("   target/data = ~p~n", [Value1]);
        E1 -> io:format("   target/data = ERROR: ~p~n", [E1])
    end,
    
    case hyper_lmdb:read(Store, <<"mylink">>) of
        {ok, Value2} -> io:format("   mylink = ~p~n", [Value2]);
        E2 -> io:format("   mylink = ERROR: ~p~n", [E2])
    end,
    
    % Test link resolution
    io:format("~n3. Reading through link:~n"),
    case hyper_lmdb:read(Store, <<"mylink/data">>) of
        {ok, Value3} -> io:format("   mylink/data = ~p~n", [Value3]);
        E3 -> io:format("   mylink/data = ERROR: ~p~n", [E3])
    end,
    
    % Test the resolve function directly
    io:format("~n4. Testing resolve function:~n"),
    case hyper_lmdb:resolve(Store, <<"mylink">>) of
        ResolvedPath when is_binary(ResolvedPath) -> 
            io:format("   resolve(mylink) = ~p~n", [ResolvedPath]);
        E4 -> 
            io:format("   resolve(mylink) = ERROR: ~p~n", [E4])
    end,
    
    case hyper_lmdb:resolve(Store, <<"mylink/data">>) of
        ResolvedPath2 when is_binary(ResolvedPath2) -> 
            io:format("   resolve(mylink/data) = ~p~n", [ResolvedPath2]);
        E5 -> 
            io:format("   resolve(mylink/data) = ERROR: ~p~n", [E5])
    end,
    
    % Check what's actually stored
    io:format("~n5. Checking raw storage:~n"),
    case hyper_lmdb:type(Store, <<"target">>) of
        Type1 -> io:format("   type(target) = ~p~n", [Type1])
    end,
    case hyper_lmdb:type(Store, <<"mylink">>) of
        Type2 -> io:format("   type(mylink) = ~p~n", [Type2])
    end,
    
    % List children of target
    io:format("~n6. Listing children:~n"),
    case hyper_lmdb:list(Store, <<"target">>) of
        {ok, Children1} -> io:format("   list(target) = ~p~n", [Children1]);
        E6 -> io:format("   list(target) = ERROR: ~p~n", [E6])
    end,
    
    case hyper_lmdb:list(Store, <<"mylink">>) of
        {ok, Children2} -> io:format("   list(mylink) = ~p~n", [Children2]);
        E7 -> io:format("   list(mylink) = ERROR: ~p~n", [E7])
    end,
    
    ok.