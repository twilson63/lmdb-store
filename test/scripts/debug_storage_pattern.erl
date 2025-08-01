#!/usr/bin/env escript

main([]) ->
    % This script debugs what's actually stored when hb_cache writes a nested message
    
    % Load the NIF
    NIFPath = filename:join([filename:dirname(filename:dirname(escript:script_name())), "..", "priv", "hyper_lmdb"]),
    ok = erlang:load_nif(NIFPath, 0),
    
    % Start the store
    StoreOpts = #{
        name => <<"debug_store">>,
        directory => <<"./debug_storage_data">>
    },
    Store = case hyper_lmdb_nif:start(StoreOpts) of
        {ok, S} -> S;
        Error ->
            io:format("Failed to start store: ~p~n", [Error]),
            halt(1)
    end,
    
    % Simulate what hb_cache does when writing a nested message
    
    % 1. Create a message ID (this is what do_write_message returns for a map)
    MsgID = <<"TestMsg123">>,
    ok = hyper_lmdb_nif:make_group(Store, MsgID),
    io:format("1. Created group at: ~s~n", [MsgID]),
    
    % 2. Simulate write_key for a key with nested content
    Key = <<"layer1+link">>,
    NestedMsgID = <<"NestedMsg456">>,
    
    % Calculate a simplified hashpath (in reality uses hb_path:hashpath)
    % The hashpath function takes base, key, algorithm, and options
    % It probably generates something like base/hash(base+key)
    KeyData = <<MsgID/binary, Key/binary>>,
    Hash = crypto:hash(sha256, KeyData),
    HashB64 = base64:encode(Hash),
    KeyHashPath = <<MsgID/binary, "/", HashB64/binary>>,
    io:format("2. Calculated hashpath for key '~s': ~s~n", [Key, KeyHashPath]),
    
    % 3. The nested message would have been written and returned its ID
    ok = hyper_lmdb_nif:make_group(Store, NestedMsgID),
    io:format("3. Created nested message group at: ~s~n", [NestedMsgID]),
    
    % 4. Create link from nested message to the hashpath
    ok = hyper_lmdb_nif:make_link(Store, NestedMsgID, KeyHashPath),
    io:format("4. Created link from ~s to ~s~n", [NestedMsgID, KeyHashPath]),
    
    % Now let's see what happens when we try to list and read
    
    % 5. List the message
    io:format("~n5. Listing message ~s:~n", [MsgID]),
    case hyper_lmdb_nif:list(Store, MsgID) of
        {ok, Items} ->
            io:format("   Found items: ~p~n", [Items]);
        not_found ->
            io:format("   Message not found as group~n");
        Error1 ->
            io:format("   Error: ~p~n", [Error1])
    end,
    
    % 6. Try to read the path that hb_cache would construct
    TestPath = <<MsgID/binary, "/", Key/binary>>,
    io:format("~n6. Trying to read: ~s~n", [TestPath]),
    case hyper_lmdb_nif:read(Store, TestPath) of
        {ok, Data} ->
            io:format("   Found: ~p~n", [Data]);
        not_found ->
            io:format("   NOT FOUND~n")
    end,
    
    % 7. Try to resolve it
    case hyper_lmdb_nif:resolve(Store, TestPath) of
        {ok, Resolved} ->
            io:format("7. Resolved to: ~s~n", [Resolved]);
        not_found ->
            io:format("7. Resolve returned not_found~n")
    end,
    
    % 8. Check what's at the hashpath
    io:format("~n8. Checking hashpath ~s:~n", [KeyHashPath]),
    case hyper_lmdb_nif:read(Store, KeyHashPath) of
        {ok, LinkData} ->
            io:format("   Found: ~p~n", [LinkData]);
        not_found ->
            io:format("   NOT FOUND~n")
    end,
    
    % 9. List all keys with prefix to see what's actually stored
    io:format("~n9. Listing all keys with prefix '~s':~n", [MsgID]),
    case hyper_lmdb_nif:list_prefix(Store, MsgID, #{}) of
        {ok, AllItems} ->
            lists:foreach(fun({K, V}) ->
                io:format("   ~s = ~p~n", [K, V])
            end, AllItems);
        Error2 ->
            io:format("   Error: ~p~n", [Error2])
    end,
    
    % Clean up
    hyper_lmdb_nif:stop(Store),
    io:format("~nDebug completed.~n"),
    ok.