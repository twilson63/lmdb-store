#!/usr/bin/env escript

main(_) ->
    % Add code paths
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/ebin"),
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/priv"),
    
    % Create store
    Store = #{<<"name">> => <<"test-raw-debug">>},
    
    % Start the store
    case hyper_lmdb:start(Store) of
        ok -> ok;
        {ok, _} -> ok;
        Error -> 
            io:format("Failed to start: ~p~n", [Error]),
            halt(1)
    end,
    
    io:format("Creating test setup...~n"),
    
    % Create data
    ok = hyper_lmdb:write(Store, <<"simple">>, <<"hello">>),
    ok = hyper_lmdb:make_link(Store, <<"simple">>, <<"link_to_simple">>),
    
    % Try to read both
    io:format("~nReading values:~n"),
    case hyper_lmdb:read(Store, <<"simple">>) of
        {ok, V1} -> io:format("simple = ~p~n", [V1]);
        E1 -> io:format("simple = ERROR: ~p~n", [E1])
    end,
    
    case hyper_lmdb:read(Store, <<"link_to_simple">>) of
        {ok, V2} -> io:format("link_to_simple = ~p~n", [V2]);
        E2 -> io:format("link_to_simple = ERROR: ~p~n", [E2])
    end,
    
    % Now test with groups
    io:format("~nTesting with groups:~n"),
    ok = hyper_lmdb:make_group(Store, <<"group1">>),
    ok = hyper_lmdb:write(Store, <<"group1/item">>, <<"data">>),
    ok = hyper_lmdb:make_link(Store, <<"group1">>, <<"link_to_group">>),
    
    case hyper_lmdb:read(Store, <<"group1/item">>) of
        {ok, V3} -> io:format("group1/item = ~p~n", [V3]);
        E3 -> io:format("group1/item = ERROR: ~p~n", [E3])
    end,
    
    case hyper_lmdb:read(Store, <<"link_to_group/item">>) of
        {ok, V4} -> io:format("link_to_group/item = ~p~n", [V4]);
        E4 -> io:format("link_to_group/item = ERROR: ~p~n", [E4])
    end,
    
    % Check resolution
    io:format("~nChecking resolve:~n"),
    R1 = hyper_lmdb:resolve(Store, <<"link_to_simple">>),
    io:format("resolve(link_to_simple) = ~p~n", [R1]),
    
    R2 = hyper_lmdb:resolve(Store, <<"link_to_group">>),
    io:format("resolve(link_to_group) = ~p~n", [R2]),
    
    R3 = hyper_lmdb:resolve(Store, <<"link_to_group/item">>),
    io:format("resolve(link_to_group/item) = ~p~n", [R3]),
    
    ok.