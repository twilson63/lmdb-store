#!/usr/bin/env escript

main(_) ->
    code:add_path("ebin"),
    
    io:format("=== Testing @data: Prefix Implementation ===~n~n"),
    
    StoreOpts = #{
        <<"name">> => <<"test_prefix_store">>,
        <<"path">> => <<"/tmp/test_prefix_lmdb">>
    },
    
    % Cleanup and start
    os:cmd("rm -rf /tmp/test_prefix_lmdb"),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    try
        test_simple_write_read(StoreOpts),
        test_link_functionality(StoreOpts),
        test_security_check(StoreOpts)
    after
        hyper_lmdb:stop(StoreOpts),
        os:cmd("rm -rf /tmp/test_prefix_lmdb")
    end.

test_simple_write_read(StoreOpts) ->
    io:format("1. Testing simple write/read with @data: prefix:~n"),
    
    % Write a value
    ok = hyper_lmdb:write(StoreOpts, <<"test_key">>, <<"test_value">>),
    
    % Read it back - should get the value without @data: prefix
    {ok, Value} = hyper_lmdb:read(StoreOpts, <<"test_key">>),
    io:format("   Written: ~p~n", [<<"test_value">>]),
    io:format("   Read back: ~p~n", [Value]),
    io:format("   Match: ~p~n~n", [Value =:= <<"test_value">>]).

test_link_functionality(StoreOpts) ->
    io:format("2. Testing link functionality with prefixes:~n"),
    
    % Write a value
    ok = hyper_lmdb:write(StoreOpts, <<"target_key">>, <<"target_value">>),
    
    % Create a link
    ok = hyper_lmdb:make_link(StoreOpts, <<"target_key">>, <<"link_key">>),
    
    % Read through link
    {ok, Value} = hyper_lmdb:read(StoreOpts, <<"link_key">>),
    io:format("   Link resolves to: ~p~n", [Value]),
    io:format("   Expected: ~p~n", [<<"target_value">>]),
    io:format("   Match: ~p~n~n", [Value =:= <<"target_value">>]).

test_security_check(StoreOpts) ->
    io:format("3. Testing security - @link: in data should not be treated as link:~n"),
    
    % Write a value that looks like a link
    MaliciousValue = <<"@link:sensitive_data">>,
    ok = hyper_lmdb:write(StoreOpts, <<"malicious_key">>, MaliciousValue),
    
    % Read it back - should get the original value, not follow as link
    {ok, Value} = hyper_lmdb:read(StoreOpts, <<"malicious_key">>),
    io:format("   Written malicious value: ~p~n", [MaliciousValue]),
    io:format("   Read back: ~p~n", [Value]),
    io:format("   Security check passed: ~p~n", [Value =:= MaliciousValue]),
    
    % Also check that sensitive_data doesn't exist
    NotFound = hyper_lmdb:read(StoreOpts, <<"sensitive_data">>),
    io:format("   sensitive_data lookup: ~p~n", [NotFound]),
    io:format("   Confirmed not followed: ~p~n~n", [NotFound =:= not_found]).