#!/usr/bin/env escript

main([]) ->
    % Load the NIF
    NIFPath = filename:join([filename:dirname(filename:dirname(escript:script_name())), "..", "priv", "libnative"]),
    ok = erlang:load_nif(NIFPath, 0),
    
    % Start the store
    StoreOpts = #{
        name => <<"test_store">>,
        directory => <<"./test_data">>
    },
    Store = case hyper_lmdb_nif:start(StoreOpts) of
        {ok, S} -> S;
        Error ->
            io:format("Failed to start store: ~p~n", [Error]),
            halt(1)
    end,
    
    % Test 1: Write some data and create a link
    io:format("Test 1: Basic link functionality~n"),
    DataPath = <<"data/test1">>,
    LinkPath = <<"links/test1">>,
    TestData = <<"Hello, World!">>,
    
    % Write data
    case hyper_lmdb_nif:write(Store, DataPath, TestData) of
        ok -> io:format("  Wrote data to ~s~n", [DataPath]);
        E1 -> io:format("  Error writing data: ~p~n", [E1])
    end,
    
    % Create link
    case hyper_lmdb_nif:make_link(Store, DataPath, LinkPath) of
        ok -> io:format("  Created link from ~s to ~s~n", [DataPath, LinkPath]);
        E2 -> io:format("  Error creating link: ~p~n", [E2])
    end,
    
    % Read through link
    case hyper_lmdb_nif:read(Store, LinkPath) of
        {ok, LinkData} ->
            io:format("  Read through link: ~p~n", [LinkData]),
            % Should be @link:data/test1
            case LinkData of
                <<"@link:", Target/binary>> ->
                    io:format("  Link points to: ~s~n", [Target]),
                    % Now resolve and read the target
                    case hyper_lmdb_nif:resolve(Store, LinkPath) of
                        {ok, ResolvedPath} ->
                            io:format("  Resolved to: ~s~n", [ResolvedPath]),
                            case hyper_lmdb_nif:read(Store, ResolvedPath) of
                                {ok, Data} ->
                                    io:format("  Final data: ~p~n", [Data]);
                                E3 ->
                                    io:format("  Error reading resolved path: ~p~n", [E3])
                            end;
                        E4 ->
                            io:format("  Error resolving: ~p~n", [E4])
                    end;
                _ ->
                    io:format("  Unexpected link format: ~p~n", [LinkData])
            end;
        E5 ->
            io:format("  Error reading link: ~p~n", [E5])
    end,
    
    % Test 2: Create a group and test composite structure
    io:format("~nTest 2: Group and composite structure~n"),
    GroupPath = <<"messages/msg1">>,
    
    % Make group
    case hyper_lmdb_nif:make_group(Store, GroupPath) of
        ok -> io:format("  Created group: ~s~n", [GroupPath]);
        E6 -> io:format("  Error creating group: ~p~n", [E6])
    end,
    
    % Write some keys under the group
    Key1Path = <<GroupPath/binary, "/key1">>,
    Key2Path = <<GroupPath/binary, "/key2">>,
    
    case hyper_lmdb_nif:write(Store, Key1Path, <<"value1">>) of
        ok -> io:format("  Wrote key1~n");
        E7 -> io:format("  Error writing key1: ~p~n", [E7])
    end,
    
    case hyper_lmdb_nif:write(Store, Key2Path, <<"value2">>) of
        ok -> io:format("  Wrote key2~n");
        E8 -> io:format("  Error writing key2: ~p~n", [E8])
    end,
    
    % List the group
    case hyper_lmdb_nif:list_prefix(Store, GroupPath, #{}) of
        {ok, Items} ->
            io:format("  Items in group: ~p~n", [Items]);
        E9 ->
            io:format("  Error listing group: ~p~n", [E9])
    end,
    
    % Check type of group
    case hyper_lmdb_nif:get_type(Store, GroupPath) of
        composite -> io:format("  Group type: composite~n");
        simple -> io:format("  Group type: simple~n");  
        not_found -> io:format("  Group type: not_found~n");
        Other -> io:format("  Group type: ~p~n", [Other])
    end,
    
    % Now create a link to the group
    GroupLinkPath = <<"links/group1">>,
    case hyper_lmdb_nif:make_link(Store, GroupPath, GroupLinkPath) of
        ok -> io:format("  Created link to group~n");
        E10 -> io:format("  Error creating group link: ~p~n", [E10])
    end,
    
    % Try to resolve the group link
    case hyper_lmdb_nif:resolve(Store, GroupLinkPath) of
        {ok, ResolvedGroup} ->
            io:format("  Group link resolved to: ~s~n", [ResolvedGroup]);
        E11 ->
            io:format("  Error resolving group link: ~p~n", [E11])
    end,
    
    hyper_lmdb_nif:stop(Store),
    ok.