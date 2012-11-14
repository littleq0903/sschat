-module(subscription, [Id, RoomUuid :: string(), UserUuid :: string(), OwnerApiKey]).
-compile(export_all).


validation_tests() ->
    AuthConditions = [{owner_api_key, 'equals', OwnerApiKey}],
    DB_Options = [],
    %Rooms = boss_db:find(room, [{room_uuid, 'equals', RoomUuid}] ++ AuthConditions, DB_Options),
    Rooms = [1],
    Users = boss_db:find(users, [{user_uuid, 'equals', UserUuid}] ++ AuthConditions, DB_Options),
    Subscriptions = boss_db:find(subscription, [{room_uuid, 'equals', RoomUuid},{user_uuid, 'equals', UserUuid}]++AuthConditions, DB_Options),
    [
        {fun() -> length(Subscriptions) == 0 end, "Subscription duplicated."},
        {fun() -> (length(Rooms) > 0) and (length(Users) > 0) end, "Unauthorized operation."}
    ].
