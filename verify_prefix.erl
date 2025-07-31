#!/usr/bin/env escript

main(_) ->
    code:add_path("ebin"),
    
    StoreOpts = #{
        <<"name">> => <<"verify_store">>,
        <<"path">> => <<"/tmp/verify_lmdb">>
    },
    
    os:cmd("rm -rf /tmp/verify_lmdb"),
    {ok, _} = hyper_lmdb:start(StoreOpts),
    
    % Test 1: Simple write/read
    ok = hyper_lmdb:write(StoreOpts, <<"key1">>, <<"value1">>),
    case hyper_lmdb:read(StoreOpts, <<"key1">>) of
        {ok, <<"value1">>} -> io:format("✓ Simple write/read works~n");
        Other1 -> io:format("✗ Simple write/read failed: ~p~n", [Other1])
    end,
    
    % Test 2: Link functionality
    ok = hyper_lmdb:make_link(StoreOpts, <<"key1">>, <<"link1">>),
    case hyper_lmdb:read(StoreOpts, <<"link1">>) of
        {ok, <<"value1">>} -> io:format("✓ Link resolution works~n");
        Other2 -> io:format("✗ Link resolution failed: ~p~n", [Other2])
    end,
    
    % Test 3: Security check - write a value that looks like a link
    ok = hyper_lmdb:write(StoreOpts, <<"malicious">>, <<"@link:secret">>),
    case hyper_lmdb:read(StoreOpts, <<"malicious">>) of
        {ok, <<"@link:secret">>} -> io:format("✓ Security check passed - @link: in data not followed~n");
        Other3 -> io:format("✗ Security check failed: ~p~n", [Other3])
    end,
    
    hyper_lmdb:stop(StoreOpts),
    os:cmd("rm -rf /tmp/verify_lmdb").