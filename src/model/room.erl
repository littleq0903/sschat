-module(room,
        [Id, RoomUuid, OwnerApiKey]
       ).
-compile(export_all).

before_create() ->
    ok.
