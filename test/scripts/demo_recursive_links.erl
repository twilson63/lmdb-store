#!/usr/bin/env escript

main(_) ->
    code:add_path("ebin"),
    
    io:format("=== hyper_lmdb Recursive Link Resolution Demo ===~n~n"),
    
    StoreOpts = #{
        <<"name">> => <<"/tmp/demo_recursive_lmdb">>
    },
    
    % Cleanup and start
    os:cmd("rm -rf /tmp/demo_recursive_lmdb"),
    {ok, _Env} = hyper_lmdb:start(StoreOpts),
    
    try
        demo_basic_path_resolution(StoreOpts),
        demo_multi_level_links(StoreOpts),
        demo_complex_organization(StoreOpts)
    after
        hyper_lmdb:stop(StoreOpts),
        os:cmd("rm -rf /tmp/demo_recursive_lmdb")
    end.

demo_basic_path_resolution(StoreOpts) ->
    io:format("1. Basic Path Resolution:~n"),
    io:format("   Setup: foo -> link:bar, bar -> 'directory', bar/baz -> 'boop'~n"),
    
    % Create the data - bar must exist as a value for path resolution to work
    ok = hyper_lmdb:write(StoreOpts, <<"bar">>, <<"directory">>),
    ok = hyper_lmdb:write(StoreOpts, <<"bar/baz">>, <<"boop">>),
    ok = hyper_lmdb:make_link(StoreOpts, <<"bar">>, <<"foo">>),
    
    % Demonstrate resolution
    {ok, Value0} = hyper_lmdb:read(StoreOpts, <<"foo">>),
    {ok, Value1} = hyper_lmdb:read(StoreOpts, <<"foo/baz">>),
    {ok, Value2} = hyper_lmdb:read(StoreOpts, <<"bar/baz">>),
    
    io:format("   Reading foo: ~s (resolves to bar's value)~n", [Value0]),
    io:format("   Reading foo/baz: ~s (resolved through link)~n", [Value1]),
    io:format("   Reading bar/baz: ~s (direct access)~n~n", [Value2]).

demo_multi_level_links(StoreOpts) ->
    io:format("2. Multi-Level Link Resolution:~n"),
    
    % Create a user profile structure
    ok = hyper_lmdb:write(StoreOpts, <<"users/alice/profile/name">>, <<"Alice Smith">>),
    ok = hyper_lmdb:write(StoreOpts, <<"users/alice/profile/role">>, <<"Developer">>),
    ok = hyper_lmdb:write(StoreOpts, <<"users/alice/settings/theme">>, <<"dark">>),
    
    % Create shortcuts
    ok = hyper_lmdb:make_link(StoreOpts, <<"users/alice">>, <<"current_user">>),
    ok = hyper_lmdb:make_link(StoreOpts, <<"current_user">>, <<"me">>),
    
    % Access through different paths
    {ok, Name1} = hyper_lmdb:read(StoreOpts, <<"users/alice/profile/name">>),
    {ok, Name2} = hyper_lmdb:read(StoreOpts, <<"current_user/profile/name">>),
    {ok, Name3} = hyper_lmdb:read(StoreOpts, <<"me/profile/name">>),
    
    io:format("   Direct path: users/alice/profile/name -> ~s~n", [Name1]),
    io:format("   Via link:    current_user/profile/name -> ~s~n", [Name2]),
    io:format("   Via chain:   me/profile/name -> ~s~n~n", [Name3]).

demo_complex_organization(StoreOpts) ->
    io:format("3. Complex Organization Structure:~n"),
    
    % Create shared resources
    ok = hyper_lmdb:write(StoreOpts, <<"resources/templates/welcome.html">>, <<"<h1>Welcome!</h1>">>),
    ok = hyper_lmdb:write(StoreOpts, <<"resources/templates/goodbye.html">>, <<"<h1>Goodbye!</h1>">>),
    ok = hyper_lmdb:write(StoreOpts, <<"resources/images/logo.png">>, <<"[PNG data]">>),
    
    % Create department structure with links to shared resources
    ok = hyper_lmdb:make_link(StoreOpts, <<"resources">>, <<"departments/marketing/shared">>),
    ok = hyper_lmdb:make_link(StoreOpts, <<"resources">>, <<"departments/sales/shared">>),
    
    % Create shortcuts to departments
    ok = hyper_lmdb:make_link(StoreOpts, <<"departments/marketing">>, <<"mkt">>),
    ok = hyper_lmdb:make_link(StoreOpts, <<"departments/sales">>, <<"sales">>),
    
    % Access the same resource through different paths
    Paths = [
        <<"resources/templates/welcome.html">>,
        <<"departments/marketing/shared/templates/welcome.html">>,
        <<"departments/sales/shared/templates/welcome.html">>,
        <<"mkt/shared/templates/welcome.html">>,
        <<"sales/shared/templates/welcome.html">>
    ],
    
    io:format("   All these paths resolve to the same template:~n"),
    lists:foreach(fun(Path) ->
        case hyper_lmdb:read(StoreOpts, Path) of
            {ok, _Value} -> io:format("   ✓ ~s~n", [Path]);
            _ -> io:format("   ✗ ~s (failed)~n", [Path])
        end
    end, Paths),
    
    io:format("~n   This demonstrates how links can create flexible~n"),
    io:format("   organizational structures while maintaining a single~n"),
    io:format("   source of truth for shared resources.~n").