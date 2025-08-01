#!/usr/bin/env escript

main(_) ->
    % Add code paths
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/ebin"),
    code:add_pathz("/Users/rakis/code/m3/hyper-lmdb/priv"),
    
    % Create store
    Store = #{<<"name">> => <<"test-sync-workaround">>},
    
    % Start the store
    case hyper_lmdb:start(Store) of
        ok -> ok;
        {ok, _} -> ok;
        Error -> 
            io:format("Failed to start: ~p~n", [Error]),
            halt(1)
    end,
    
    io:format("Testing if sync helps with multi-transaction visibility...~n~n"),
    
    % Simulate what hb_cache does - multiple transactions
    io:format("1. Simulating message write (multiple transactions)...~n"),
    
    % Transaction 1: Create group
    ok = hyper_lmdb:make_group(Store, <<"msg1">>),
    io:format("   - Created group 'msg1'~n"),
    
    % Transaction 2: Write field 1
    ok = hyper_lmdb:write(Store, <<"msg1/field1">>, <<"value1">>),
    io:format("   - Wrote field1~n"),
    
    % Transaction 3: Write field 2
    ok = hyper_lmdb:write(Store, <<"msg1/field2">>, <<"value2">>),
    io:format("   - Wrote field2~n"),
    
    % Transaction 4: Create link
    ok = hyper_lmdb:make_link(Store, <<"msg1">>, <<"link1">>),
    io:format("   - Created link~n"),
    
    % Without sync - immediate read
    io:format("~n2. Reading immediately (no sync)...~n"),
    check_all_fields(Store, "no sync"),
    
    % With sync
    io:format("~n3. Calling sync...~n"),
    ok = hyper_lmdb:sync(Store),
    
    io:format("~n4. Reading after sync...~n"),
    check_all_fields(Store, "after sync"),
    
    % Test with sleep instead
    io:format("~n5. Waiting 100ms...~n"),
    timer:sleep(100),
    
    io:format("~n6. Reading after sleep...~n"),
    check_all_fields(Store, "after sleep"),
    
    % Test batch of operations
    io:format("~n7. Testing rapid batch operations...~n"),
    lists:foreach(fun(N) ->
        Group = <<"group", (integer_to_binary(N))/binary>>,
        ok = hyper_lmdb:make_group(Store, Group),
        ok = hyper_lmdb:write(Store, <<Group/binary, "/data">>, <<"test">>),
        ok = hyper_lmdb:make_link(Store, Group, <<"link", (integer_to_binary(N))/binary>>)
    end, lists:seq(1, 10)),
    
    io:format("   - Created 10 groups with data and links~n"),
    
    % Check without sync
    NotFoundNoSync = check_batch(Store, 1, 10),
    io:format("~n8. Without sync: ~p items incomplete~n", [NotFoundNoSync]),
    
    % Sync and check again
    ok = hyper_lmdb:sync(Store),
    NotFoundAfterSync = check_batch(Store, 1, 10),
    io:format("9. After sync: ~p items incomplete~n", [NotFoundAfterSync]),
    
    io:format("~nConclusion: "),
    if
        NotFoundAfterSync < NotFoundNoSync ->
            io:format("Sync helps but doesn't guarantee atomicity~n");
        NotFoundAfterSync == 0 ->
            io:format("Sync appears to work as a workaround!~n");
        true ->
            io:format("Sync doesn't help with the atomicity issue~n")
    end,
    
    ok.

check_all_fields(Store, Label) ->
    Field1 = case hyper_lmdb:read(Store, <<"msg1/field1">>) of
        {ok, <<"value1">>} -> "ok";
        _ -> "FAIL"
    end,
    Field2 = case hyper_lmdb:read(Store, <<"msg1/field2">>) of
        {ok, <<"value2">>} -> "ok";
        _ -> "FAIL"
    end,
    Link = case hyper_lmdb:read(Store, <<"link1/field1">>) of
        {ok, <<"value1">>} -> "ok";
        _ -> "FAIL"
    end,
    io:format("   ~s: field1=~s, field2=~s, link_resolution=~s~n", 
              [Label, Field1, Field2, Link]).

check_batch(Store, Start, End) ->
    lists:foldl(fun(N, Acc) ->
        Group = <<"group", (integer_to_binary(N))/binary>>,
        Link = <<"link", (integer_to_binary(N))/binary>>,
        case {hyper_lmdb:read(Store, <<Group/binary, "/data">>),
              hyper_lmdb:read(Store, <<Link/binary, "/data">>)} of
            {{ok, <<"test">>}, {ok, <<"test">>}} -> Acc;
            _ -> Acc + 1
        end
    end, 0, lists:seq(Start, End)).