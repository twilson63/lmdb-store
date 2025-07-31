#!/usr/bin/env escript

main(_) ->
    code:add_path("ebin"),
    
    S = #{<<"name">> => <<"qtest">>, <<"path">> => <<"/tmp/qtest">>},
    os:cmd("rm -rf /tmp/qtest"),
    
    {ok, _} = hyper_lmdb:start(S),
    
    % Test 1: Basic functionality
    ok = hyper_lmdb:write(S, <<"k1">>, <<"v1">>),
    R1 = hyper_lmdb:read(S, <<"k1">>),
    io:format("Test 1 - Basic: ~p (expect {ok,<<\"v1\">>})~n", [R1]),
    
    % Test 2: Security test
    ok = hyper_lmdb:write(S, <<"k2">>, <<"@link:target">>),
    R2 = hyper_lmdb:read(S, <<"k2">>),
    io:format("Test 2 - Security: ~p (expect {ok,<<\"@link:target\">>})~n", [R2]),
    
    % Test 3: Real link
    ok = hyper_lmdb:make_link(S, <<"k1">>, <<"k3">>),
    R3 = hyper_lmdb:read(S, <<"k3">>),
    io:format("Test 3 - Link: ~p (expect {ok,<<\"v1\">>})~n", [R3]),
    
    hyper_lmdb:stop(S),
    os:cmd("rm -rf /tmp/qtest").