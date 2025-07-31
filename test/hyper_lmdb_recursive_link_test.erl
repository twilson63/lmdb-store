-module(hyper_lmdb_recursive_link_test).
-include_lib("eunit/include/eunit.hrl").

%% Test fixture setup and teardown
setup() ->
    StoreOpts = #{
        <<"store-module">> => hyper_lmdb,
        <<"name">> => <<"./test-recursive-link-lmdb">>
    },
    case hyper_lmdb:start(StoreOpts) of
        {ok, _EnvRef} -> ok;
        ok -> ok
    end,
    StoreOpts.

cleanup(StoreOpts) ->
    hyper_lmdb:reset(StoreOpts),
    hyper_lmdb:stop(StoreOpts),
    os:cmd("rm -rf ./test-recursive-link-lmdb").

%% Test generators
recursive_link_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        fun test_simple_path_link_resolution/1,
        fun test_multi_level_path_resolution/1,
        fun test_link_chain_with_paths/1,
        fun test_partial_path_link_resolution/1,
        fun test_circular_path_links/1,
        fun test_complex_link_scenario/1,
        fun test_link_to_link_with_subpath/1
     ]
    }.

test_simple_path_link_resolution(StoreOpts) ->
    {"Simple path link resolution - foo -> link:bar, bar -> 'target', bar/baz -> 'boop'",
     fun() ->
        % Create the target values
        ok = hyper_lmdb:write(StoreOpts, <<"bar">>, <<"target">>),
        ok = hyper_lmdb:write(StoreOpts, <<"bar/baz">>, <<"boop">>),
        
        % Create link from foo to bar
        ok = hyper_lmdb:make_link(StoreOpts, <<"bar">>, <<"foo">>),
        
        % foo should resolve to "target"
        ?assertEqual({ok, <<"target">>}, hyper_lmdb:read(StoreOpts, <<"foo">>)),
        
        % foo/baz should resolve to bar/baz and return "boop"
        ?assertEqual({ok, <<"boop">>}, hyper_lmdb:read(StoreOpts, <<"foo/baz">>)),
        
        % bar/baz should still return "boop" directly
        ?assertEqual({ok, <<"boop">>}, hyper_lmdb:read(StoreOpts, <<"bar/baz">>))
     end}.

test_multi_level_path_resolution(StoreOpts) ->
    {"Multi-level path resolution with links at different levels",
     fun() ->
        % Create nested structure
        ok = hyper_lmdb:write(StoreOpts, <<"users/admin">>, <<"admin_user">>),
        ok = hyper_lmdb:write(StoreOpts, <<"users/admin/profile/name">>, <<"Administrator">>),
        ok = hyper_lmdb:write(StoreOpts, <<"users/admin/profile/email">>, <<"admin@example.com">>),
        
        % Create link: current -> users/admin
        ok = hyper_lmdb:make_link(StoreOpts, <<"users/admin">>, <<"current">>),
        
        % current should resolve to "admin_user"
        ?assertEqual({ok, <<"admin_user">>}, hyper_lmdb:read(StoreOpts, <<"current">>)),
        
        % current/profile/name should resolve to Administrator
        ?assertEqual({ok, <<"Administrator">>}, hyper_lmdb:read(StoreOpts, <<"current/profile/name">>)),
        ?assertEqual({ok, <<"admin@example.com">>}, hyper_lmdb:read(StoreOpts, <<"current/profile/email">>))
     end}.

test_link_chain_with_paths(StoreOpts) ->
    {"Link chain resolution with paths",
     fun() ->
        % Create data
        ok = hyper_lmdb:write(StoreOpts, <<"data">>, <<"data_root">>),
        ok = hyper_lmdb:write(StoreOpts, <<"data/config/database/host">>, <<"localhost">>),
        ok = hyper_lmdb:write(StoreOpts, <<"data/config/database/port">>, <<"5432">>),
        
        % Create staging that points to data
        ok = hyper_lmdb:write(StoreOpts, <<"staging">>, <<"staging_root">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"data">>, <<"staging">>),
        
        % Create prod that points to staging
        ok = hyper_lmdb:write(StoreOpts, <<"prod">>, <<"prod_root">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"staging">>, <<"prod">>),
        
        % prod should resolve through the chain to "data_root"
        ?assertEqual({ok, <<"data_root">>}, hyper_lmdb:read(StoreOpts, <<"prod">>)),
        
        % prod/config/database/host should resolve through the chain
        ?assertEqual({ok, <<"localhost">>}, hyper_lmdb:read(StoreOpts, <<"prod/config/database/host">>)),
        ?assertEqual({ok, <<"5432">>}, hyper_lmdb:read(StoreOpts, <<"prod/config/database/port">>))
     end}.

test_partial_path_link_resolution(StoreOpts) ->
    {"Link in the middle of a path",
     fun() ->
        % Create structure where link is in the middle
        ok = hyper_lmdb:write(StoreOpts, <<"base/real">>, <<"real_value">>),
        ok = hyper_lmdb:write(StoreOpts, <<"base/real/data/value">>, <<"found">>),
        
        % Create link: base/link -> base/real
        ok = hyper_lmdb:make_link(StoreOpts, <<"base/real">>, <<"base/link">>),
        
        % base/link should resolve to "real_value"
        ?assertEqual({ok, <<"real_value">>}, hyper_lmdb:read(StoreOpts, <<"base/link">>)),
        
        % base/link/data/value should resolve to base/real/data/value
        ?assertEqual({ok, <<"found">>}, hyper_lmdb:read(StoreOpts, <<"base/link/data/value">>))
     end}.

test_circular_path_links(StoreOpts) ->
    {"Circular links with paths should be handled gracefully",
     fun() ->
        % Create circular link: a/link -> b, b/link -> a
        ok = hyper_lmdb:make_link(StoreOpts, <<"b">>, <<"a/link">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"a">>, <<"b/link">>),
        
        % These should not crash but return not_found
        ?assertEqual(not_found, hyper_lmdb:read(StoreOpts, <<"a/link/data">>)),
        ?assertEqual(not_found, hyper_lmdb:read(StoreOpts, <<"b/link/data">>))
     end}.

test_complex_link_scenario(StoreOpts) ->
    {"Complex scenario with multiple links and paths",
     fun() ->
        % Set up a complex structure
        % Actual data
        ok = hyper_lmdb:write(StoreOpts, <<"storage">>, <<"storage_root">>),
        ok = hyper_lmdb:write(StoreOpts, <<"storage/files/document.txt">>, <<"content">>),
        ok = hyper_lmdb:write(StoreOpts, <<"storage/files/image.png">>, <<"binary_data">>),
        
        % Department structure
        ok = hyper_lmdb:write(StoreOpts, <<"departments/hr">>, <<"hr_dept">>),
        ok = hyper_lmdb:write(StoreOpts, <<"departments/it">>, <<"it_dept">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"storage">>, <<"departments/hr/storage">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"storage">>, <<"departments/it/storage">>),
        
        % Shortcuts
        ok = hyper_lmdb:make_link(StoreOpts, <<"departments/hr">>, <<"hr">>),
        ok = hyper_lmdb:make_link(StoreOpts, <<"departments/it">>, <<"it">>),
        
        % All these paths should resolve to the same value
        ?assertEqual({ok, <<"content">>}, hyper_lmdb:read(StoreOpts, <<"storage/files/document.txt">>)),
        ?assertEqual({ok, <<"content">>}, hyper_lmdb:read(StoreOpts, <<"departments/hr/storage/files/document.txt">>)),
        ?assertEqual({ok, <<"content">>}, hyper_lmdb:read(StoreOpts, <<"departments/it/storage/files/document.txt">>)),
        ?assertEqual({ok, <<"content">>}, hyper_lmdb:read(StoreOpts, <<"hr/storage/files/document.txt">>)),
        ?assertEqual({ok, <<"content">>}, hyper_lmdb:read(StoreOpts, <<"it/storage/files/document.txt">>))
     end}.

test_link_to_link_with_subpath(StoreOpts) ->
    {"Link pointing to another link with subpath",
     fun() ->
        % Create base data
        ok = hyper_lmdb:write(StoreOpts, <<"base/data/item">>, <<"value">>),
        
        % Create first link with subpath
        ok = hyper_lmdb:make_link(StoreOpts, <<"base/data">>, <<"link1">>),
        
        % Create second link to first link
        ok = hyper_lmdb:make_link(StoreOpts, <<"link1">>, <<"link2">>),
        
        % link2/item should resolve through both links
        ?assertEqual({ok, <<"value">>}, hyper_lmdb:read(StoreOpts, <<"link2/item">>))
     end}.