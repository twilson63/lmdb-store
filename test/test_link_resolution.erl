-module(test_link_resolution).
-export([test/0]).

test() ->
    % Start the store
    StoreOpts = #{
        name => <<"test_store_link">>,
        directory => <<"./test_data_link">>
    },
    {ok, Store} = hyper_lmdb:start(StoreOpts),
    io:format("Store started successfully~n"),
    
    % Test the fix: Create a chain of links
    io:format("~nTesting link resolution fix...~n"),
    
    % 1. Write some actual data
    DataPath = <<"data/actual_content">>,
    TestData = <<"This is the actual content">>,
    ok = hyper_lmdb:write(Store, DataPath, TestData),
    io:format("1. Wrote actual data to ~s~n", [DataPath]),
    
    % 2. Create a link to the data
    Link1Path = <<"links/link1">>,
    ok = hyper_lmdb:make_link(Store, DataPath, Link1Path),
    io:format("2. Created link1 -> data~n"),
    
    % 3. Create a link to the first link (this should resolve to data)
    Link2Path = <<"links/link2">>,
    ok = hyper_lmdb:make_link(Store, Link1Path, Link2Path),
    io:format("3. Created link2 -> link1 (should resolve to data)~n"),
    
    % 4. Check what link2 actually points to
    {ok, LinkContent} = hyper_lmdb:read(Store, Link2Path),
    io:format("4. Link2 content: ~p~n", [LinkContent]),
    % Should be @link:data/actual_content (resolved), not @link:links/link1
    case LinkContent of
        <<"@link:data/actual_content">> ->
            io:format("   ✓ Link correctly resolved to final target!~n");
        <<"@link:links/link1">> ->
            io:format("   ✗ Link not resolved (points to intermediate link)~n");
        Other ->
            io:format("   ? Unexpected link content: ~p~n", [Other])
    end,
    
    % 5. Test resolving through the chain
    {ok, ResolvedPath} = hyper_lmdb:resolve(Store, Link2Path),
    io:format("5. Link2 resolves to: ~s~n", [ResolvedPath]),
    {ok, Data} = hyper_lmdb:read(Store, ResolvedPath),
    io:format("   Final data: ~p~n", [Data]),
    case Data of
        TestData ->
            io:format("   ✓ Successfully read original data through link chain!~n");
        _ ->
            io:format("   ✗ Data mismatch~n")
    end,
    
    % Test with groups
    io:format("~nTesting link to group...~n"),
    GroupPath = <<"groups/test_group">>,
    ok = hyper_lmdb:make_group(Store, GroupPath),
    io:format("6. Created group at ~s~n", [GroupPath]),
    
    % Write data under the group
    Key1 = <<GroupPath/binary, "/key1">>,
    ok = hyper_lmdb:write(Store, Key1, <<"value1">>),
    io:format("7. Wrote key1 under group~n"),
    
    % Create a link to the group
    GroupLink = <<"links/group_link">>,
    ok = hyper_lmdb:make_link(Store, GroupPath, GroupLink),
    io:format("8. Created link to group~n"),
    
    % Check the link
    {ok, GroupLinkContent} = hyper_lmdb:read(Store, GroupLink),
    io:format("9. Group link content: ~p~n", [GroupLinkContent]),
    
    % Resolve the group link
    {ok, ResolvedGroup} = hyper_lmdb:resolve(Store, GroupLink),
    io:format("10. Group link resolves to: ~s~n", [ResolvedGroup]),
    
    % Check type of resolved group
    Type = hyper_lmdb:type(Store, ResolvedGroup),
    io:format("11. Type of resolved group: ~p~n", [Type]),
    case Type of
        composite ->
            io:format("    ✓ Correctly identified as composite~n");
        _ ->
            io:format("    ✗ Wrong type~n")
    end,
    
    % Clean up
    hyper_lmdb:stop(Store),
    io:format("~nTest completed.~n"),
    ok.