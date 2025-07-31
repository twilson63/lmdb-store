#!/usr/bin/env escript

main(_) ->
    code:add_path("ebin"),
    
    io:format("=== hyper_lmdb Link Feature Demo ===~n~n"),
    
    StoreOpts = #{
        <<"name">> => <<"/tmp/demo_lmdb">>
    },
    
    % Cleanup and start
    os:cmd("rm -rf /tmp/demo_lmdb"),
    {ok, Env} = hyper_lmdb:start(StoreOpts),
    
    try
        demo_simple_link(StoreOpts),
        demo_link_chains(StoreOpts), 
        demo_link_updates(StoreOpts),
        demo_circular_links(StoreOpts)
    after
        hyper_lmdb:stop(StoreOpts),
        os:cmd("rm -rf /tmp/demo_lmdb")
    end.

demo_simple_link(StoreOpts) ->
    io:format("1. Simple Link:~n"),
    
    % Store a value
    ok = hyper_lmdb:write(StoreOpts, <<"config/database/host">>, <<"localhost">>),
    
    % Create a link to it
    ok = hyper_lmdb:make_link(StoreOpts, <<"config/database/host">>, <<"db_host">>),
    
    % Read through the link
    {ok, Value} = hyper_lmdb:read(StoreOpts, <<"db_host">>),
    io:format("   db_host -> ~s~n", [Value]),
    
    % Check types
    {ok, simple} = hyper_lmdb:type(StoreOpts, <<"config/database/host">>),
    {ok, link} = hyper_lmdb:type(StoreOpts, <<"db_host">>),
    io:format("   Types: config/database/host is 'simple', db_host is 'link'~n~n").

demo_link_chains(StoreOpts) ->
    io:format("2. Link Chains:~n"),
    
    % Create a chain of links
    ok = hyper_lmdb:write(StoreOpts, <<"users/admin/email">>, <<"admin@example.com">>),
    ok = hyper_lmdb:make_link(StoreOpts, <<"users/admin/email">>, <<"admin_email">>),
    ok = hyper_lmdb:make_link(StoreOpts, <<"admin_email">>, <<"contact_email">>),
    ok = hyper_lmdb:make_link(StoreOpts, <<"contact_email">>, <<"support_email">>),
    
    % All resolve to the same value
    {ok, Email} = hyper_lmdb:read(StoreOpts, <<"support_email">>),
    io:format("   support_email -> contact_email -> admin_email -> users/admin/email~n"),
    io:format("   Final value: ~s~n~n", [Email]).

demo_link_updates(StoreOpts) ->
    io:format("3. Updating Links:~n"),
    
    % Create two config values
    ok = hyper_lmdb:write(StoreOpts, <<"config/prod/api_url">>, <<"https://api.prod.example.com">>),
    ok = hyper_lmdb:write(StoreOpts, <<"config/dev/api_url">>, <<"http://localhost:8080">>),
    
    % Create a link that initially points to dev
    ok = hyper_lmdb:make_link(StoreOpts, <<"config/dev/api_url">>, <<"current_api_url">>),
    {ok, DevUrl} = hyper_lmdb:read(StoreOpts, <<"current_api_url">>),
    io:format("   current_api_url initially points to dev: ~s~n", [DevUrl]),
    
    % Update link to point to prod
    ok = hyper_lmdb:make_link(StoreOpts, <<"config/prod/api_url">>, <<"current_api_url">>),
    {ok, ProdUrl} = hyper_lmdb:read(StoreOpts, <<"current_api_url">>),
    io:format("   current_api_url now points to prod: ~s~n~n", [ProdUrl]).

demo_circular_links(StoreOpts) ->
    io:format("4. Circular Link Detection:~n"),
    
    % Create a circular reference
    ok = hyper_lmdb:make_link(StoreOpts, <<"link_b">>, <<"link_a">>),
    ok = hyper_lmdb:make_link(StoreOpts, <<"link_c">>, <<"link_b">>),
    ok = hyper_lmdb:make_link(StoreOpts, <<"link_a">>, <<"link_c">>),
    
    % Try to read - should return not_found
    Result = hyper_lmdb:read(StoreOpts, <<"link_a">>),
    io:format("   Circular link (link_a -> link_b -> link_c -> link_a)~n"),
    io:format("   Read result: ~p (safely handled)~n~n", [Result]).