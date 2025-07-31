#!/usr/bin/env escript

main([]) ->
    % Load the NIF
    NIFPath = filename:join([filename:dirname(filename:dirname(escript:script_name())), "..", "priv", "libnative"]),
    ok = erlang:load_nif(NIFPath, 0),
    
    % Start the store
    StoreOpts = #{
        name => <<"test_store">>,
        <<"store-module">> => <<"hyper_lmdb">>,
        partition => test_partition,
        directory => <<"./test_data">>
    },
    Store = case hyper_lmdb_nif:start(StoreOpts) of
        {ok, S} ->
            io:format("Store started: ~p~n", [S]),
            S;
        Error ->
            io:format("Failed to start store: ~p~n", [Error]),
            halt(1)
    end,
    
    % Create nested message
    Item = #{
        <<"layer1">> => #{
            <<"layer2">> => #{
                <<"layer3">> => #{
                    <<"a">> => <<"b">>
                },
                <<"layer3b">> => #{
                    <<"c">> => <<"d">>
                },
                <<"layer3c">> => #{}
            }
        }
    },
    
    % Write layer3 first
    Layer3 = #{<<"a">> => <<"b">>},
    Layer3Path = write_and_trace(Store, <<"layer3">>, Layer3),
    
    % Write layer2 
    Layer2 = #{
        <<"layer3">> => #{<<"a">> => <<"b">>},
        <<"layer3b">> => #{<<"c">> => <<"d">>},
        <<"layer3c">> => #{}
    },
    Layer2Path = write_and_trace(Store, <<"layer2">>, Layer2),
    
    % Write layer1
    Layer1 = #{<<"layer2">> => Layer2},
    Layer1Path = write_and_trace(Store, <<"layer1">>, Layer1),
    
    % Now try to read back
    io:format("~nReading back layer1...~n"),
    case hyper_lmdb_nif:read(Store, Layer1Path) of
        {ok, Data} ->
            io:format("Layer1 data: ~p~n", [Data]);
        Error ->
            io:format("Error reading layer1: ~p~n", [Error])
    end,
    
    % List what's under layer1
    io:format("~nListing layer1...~n"),
    case hyper_lmdb_nif:list_prefix(Store, Layer1Path, #{}) of
        {ok, Items} ->
            io:format("Items under layer1: ~p~n", [Items]);
        Error2 ->
            io:format("Error listing: ~p~n", [Error2])
    end,
    
    % Check if layer2 is a link
    Layer2Key = <<"layer2">>,
    Layer2HP = hashpath(Layer1Path, Layer2Key),
    io:format("~nChecking layer2 hashpath: ~p~n", [Layer2HP]),
    
    case hyper_lmdb_nif:read(Store, Layer2HP) of
        {ok, LinkData} ->
            io:format("Layer2 hashpath data: ~p~n", [LinkData]),
            % Try to resolve it
            case hyper_lmdb_nif:resolve(Store, Layer2HP) of
                {ok, Resolved} ->
                    io:format("Resolved to: ~p~n", [Resolved]);
                ResolveError ->
                    io:format("Resolve error: ~p~n", [ResolveError])
            end;
        ReadError ->
            io:format("Error reading layer2 hashpath: ~p~n", [ReadError])
    end,
    
    hyper_lmdb_nif:stop(Store),
    ok.

write_and_trace(Store, Name, Data) ->
    io:format("~nWriting ~s...~n", [Name]),
    DataBin = term_to_binary(Data),
    HashBin = base64:encode(crypto:hash(sha256, DataBin)),
    Path = <<"data/", HashBin/binary>>,
    case hyper_lmdb_nif:write(Store, Path, DataBin) of
        ok ->
            io:format("Wrote ~s to path: ~p~n", [Name, Path]),
            Path;
        Error ->
            io:format("Error writing ~s: ~p~n", [Name, Error]),
            error
    end.

hashpath(Base, Key) ->
    KeyBin = if
        is_binary(Key) -> Key;
        true -> term_to_binary(Key)
    end,
    BaseKeyData = <<Base/binary, KeyBin/binary>>,
    HashBin = base64:encode(crypto:hash(sha256, BaseKeyData)),
    <<Base/binary, "/", HashBin/binary>>.