-module(room,
        [Id, RoomUuid, OwnerApiKey]
       ).
-compile(export_all).

before_create() ->
    io:format("in before hook: (~p, ~p, ~p)~n", [Id,RoomUuid,OwnerApiKey] ),
    ok.
