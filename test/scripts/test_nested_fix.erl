#!/usr/bin/env escript

main([]) ->
    % Load the NIF
    NIFPath = filename:join([filename:dirname(filename:dirname(escript:script_name())), "..", "priv", "hyper_lmdb"]),
    ok = erlang:load_nif(NIFPath, 0),
    
    % Start the store
    StoreOpts = #{
        name => <<"test_store">>,
        directory => <<"./test_data">>
    },
    Store = case hyper_lmdb_nif:start(StoreOpts) of
        {ok, S} ->
            io:format("Store started successfully~n"),
            S;
        Error ->
            io:format("Failed to start store: ~p~n", [Error]),
            halt(1)
    end,
    
    % Test the fix: Create a chain of links
    io:format("~nTesting link resolution fix...~n"),
    
    % 1. Write some actual data
    DataPath = <<"data/actual_content">>,
    TestData = <<"This is the actual content">>,
    case hyper_lmdb_nif:write(Store, DataPath, TestData) of
        ok -> io:format("1. Wrote actual data to ~s~n", [DataPath]);
        E1 -> io:format("   Error: ~p~n", [E1])
    end,
    
    % 2. Create a link to the data
    Link1Path = <<"links/link1">>,
    case hyper_lmdb_nif:make_link(Store, DataPath, Link1Path) of
        ok -> io:format("2. Created link1 -> data~n");
        E2 -> io:format("   Error: ~p~n", [E2])
    end,
    
    % 3. Create a link to the first link (this should resolve to data)
    Link2Path = <<"links/link2">>,
    case hyper_lmdb_nif:make_link(Store, Link1Path, Link2Path) of
        ok -> io:format("3. Created link2 -> link1 (should resolve to data)~n");
        E3 -> io:format("   Error: ~p~n", [E3])
    end,
    
    % 4. Check what link2 actually points to
    case hyper_lmdb_nif:read(Store, Link2Path) of
        {ok, LinkContent} ->
            io:format("4. Link2 content: ~p~n", [LinkContent]),
            % Should be @link:data/actual_content (resolved), not @link:links/link1
            case LinkContent of
                <<"@link:data/actual_content">> ->
                    io:format("   ✓ Link correctly resolved to final target!~n");
                <<"@link:links/link1">> ->
                    io:format("   ✗ Link not resolved (points to intermediate link)~n");
                Other ->
                    io:format("   ? Unexpected link content: ~p~n", [Other])
            end;
        E4 ->
            io:format("   Error reading link2: ~p~n", [E4])
    end,
    
    % 5. Test resolving through the chain
    case hyper_lmdb_nif:resolve(Store, Link2Path) of
        {ok, ResolvedPath} ->
            io:format("5. Link2 resolves to: ~s~n", [ResolvedPath]),
            case hyper_lmdb_nif:read(Store, ResolvedPath) of
                {ok, Data} ->
                    io:format("   Final data: ~p~n", [Data]),
                    if
                        Data == TestData ->
                            io:format("   ✓ Successfully read original data through link chain!~n");
                        true ->
                            io:format("   ✗ Data mismatch~n")
                    end;
                E5 ->
                    io:format("   Error reading resolved path: ~p~n", [E5])
            end;
        E6 ->
            io:format("   Error resolving link2: ~p~n", [E6])
    end,
    
    % Clean up
    hyper_lmdb_nif:stop(Store),
    io:format("~nTest completed.~n"),
    ok.