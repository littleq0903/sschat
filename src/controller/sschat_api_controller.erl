-module(sschat_api_controller, [Req]).
-compile(export_all).

-record(auth_info, {apikey}).
-import(msg_lib, [get_uuid_id/1]).

get_auth_conditions(Conditions, Apikey) ->
    AuthConditions = [{owner_api_key, 'equals', Apikey}],
    Conditions ++ AuthConditions.

bossdb_auth_find_first(Apikey, Type, Conditions, Options) -> 
    NewConditions = get_auth_conditions(Conditions, Apikey),
    boss_db:find_first(Type, NewConditions, Options).

bossdb_auth_find(Apikey, Type, Conditions, Options) -> 
    NewConditions = get_auth_conditions(Conditions, Apikey),
    boss_db:find(Type, NewConditions, Options).

bossdb_auth_count(Apikey, Type, Conditions, Options) ->
    NewConditions = get_auth_conditions(Conditions, Apikey),
    boss_db:count(Type, NewConditions, Options).
        
bossdb_auth_delete(Apikey, Type, Conditions, Options) ->
    ToBeDeleted = bossdb_auth_find(Apikey, Type, Conditions, Options),
    lists:map(fun(Object) -> boss_db:delete(Object:id()) end, ToBeDeleted).

check_apikey(ApikeyFromReq) ->
    Count = boss_db:count(apikey, [api_key, 'equals', ApikeyFromReq]),
    case Count of
        0 -> false;
        _ -> true
    end.

% pre-processing
before_(Route) when Route == "register" ->
    {ok, []};
before_(_) ->
    io:format("Checking authorization...~n"),
    Apikey = Req:query_param("apikey"),
    case check_apikey(Apikey) of
        true ->
            AuthInfo = #auth_info{apikey=Apikey},
            {ok, AuthInfo};
        _ ->
            io:format("redirect to permission denied page..."),
            {redirect, "/404"}
    end.

% register apis (public, no need to be authorized)
register('GET', [ApiKey]) ->
    %% API to fetch the information of an api key.
    %% REQUIRE ARGUMENTS:
    %%      none
    Count = boss_db:count(apikey, [api_key, 'equals', ApiKey]),
    case Count of
        0 -> {json, [{status, "not_found"}]};
        _ -> {json, [
                    {status, "ok"},
                    {api_key, ApiKey}
                    ]}
    end;
register('POST', []) ->
    %% API to apply an api key for use this service.
    %% TODO: require more information for applying api key.
    %% REQUIRE ARGUMENTS:
    %%      none
    New_API_key = get_uuid_id(apikey),
    io:format("Generated a api key: ~p, storing to database...~n", [New_API_key]),
    New_Application = apikey:new(id, New_API_key), 
    case New_Application:save() of
        {ok, _} -> {json, [
                    {status, "ok"},
                    {api_key, New_API_key}
                    ]};
        {error, Msgs} -> {output, lists:nth(1, Msgs)}
    end;
register('DELETE', [ApiKey]) ->
    %% API to request permanent deleting of an api key.
    %% REQUIRE ARGUMENTS:
    %%      none
    Registration = boss_db:find_first(apikey, [api_key, 'equals', ApiKey]),
    Result = case Registration of
        undefined -> {error, "undefined"};
        Other -> 
            DataId = element(2, Other),
            io:format("Found data: ~p, deleting~n", [DataId]),
            boss_db:delete(DataId),
            io:format("Received deleting api key action, deleting api key: ~p~n", [ApiKey])
    end,
    case Result of
        ok -> {json, [
                    {status, "ok"},
                    {deleted_api_key, ApiKey}]};
        {error, Reason} -> {json, [
                    {status, "error"},
                    {reason, Reason}
                    ]}
    end.


% APIs need to be authorized wuth url parameter "apikey"
% rooms apis
room('GET', [RoomId], _AuthInfo) ->
    #auth_info{apikey=Apikey} = _AuthInfo,
    Rooms = bossdb_auth_find(Apikey, rooms, [{rooms_uuid, 'equals', RoomId}], []),
    Room = case length(Rooms) of
        1 -> lists:nth(1, Rooms);
        0 -> undefined
    end,
    case Room of
        undefined ->
            { json, [
                    {status, "error"},
                    {msg, "not_found"}
                    ] };
        RoomRecord -> 
            SubscriptionRecords = boss_db:find(subscription, [{'room_uuid', 'equals', RoomId}]),
            Subscripters = lists:map(fun(X) -> list_to_binary(X:user_uuid()) end, SubscriptionRecords),
            { json, [
                    {status, "ok"},
                    {data, [
                            %{members_count, bossdb_auth_count(Apikey, subscription, [{rooms_uuid, 'equals', RoomId}], [])},
                            {rooms_uuid, RoomRecord:rooms_uuid()},
                            {subscripters, Subscripters}
                            ]}
                    ] }
    end;
room('POST', [], _AuthInfo) ->
    #auth_info{apikey=Apikey} = _AuthInfo,
    RoomId = get_uuid_id(room),
    io:format("Received request to create a room, assigned room id: ~p~n", [RoomId]),
    Room = rooms:new(id, RoomId, Apikey),
    case Room:save() of
        {ok, _} -> {json, [
                    {status, "ok"},
                    {room_id, RoomId}
                    ]};
        {error, Msgs} -> {json, [
                    {status, "error"},
                    {msg, lists:nth(1, Msgs)}
                    ]}
    end;
room('DELETE', [RoomId], _AuthInfo) ->
    % TODO: delete related subcription after deleting.
    #auth_info{apikey=Apikey} = _AuthInfo,
    io:format("Received request to delete a room: ~p, deleting...", [RoomId]),
    ExecutiveResult = bossdb_auth_delete(Apikey, rooms, [{rooms_uuid, 'equals', RoomId}], []),
    
    case length(ExecutiveResult) of
        0 ->
            {json, [
                {status, "no_deleted"}
                    ]};
        Number ->
            {json, [
                {status, "ok"},
                {deleted_num, Number}
                    ]}
    end;

room('PUT', [RoomId, UserId, Action], _AuthInfo) ->
    #auth_info{apikey=Apikey} = _AuthInfo,
    io:format("room: ~p; user: ~p~n", [RoomId, UserId]),
    {Status, Message} = case Action of
        "add" ->
            Subscription = subscription:new(id, RoomId, UserId, Apikey),
            case Subscription:save() of
                {ok, _Record} -> {"ok", "Subscribing succeed."};
                {error, Msgs} -> {"error", lists:nth(1,Msgs)}
            end;
        "remove" ->
            ActionResult = bossdb_auth_delete(Apikey, subscription, [{rooms_uuid, 'equals', RoomId}, {user_uuid, 'equals', UserId}], []),
            case ActionResult of
                ok -> {"ok", "Desubscribing succeed."};
                {error, Msg} -> {"error", Msg}
            end
    end,
    {json, [
            {status, Status},
            {action, Action},
            {user, UserId},
            {room, RoomId},
            {msg, Message}
    ]}.


% users apis
user('GET', [UserId], _AuthInfo) ->
    #auth_info{apikey=Apikey} = _AuthInfo,
    Users = bossdb_auth_find(Apikey, users, [{user_uuid, 'equals', UserId}], []),
    User = case length(Users) of
        1 -> lists:nth(1, Users);
        0 -> undefined
    end,
    case User of
        undefined -> 
            { json, [
                    {status, "error"},
                    {msg, "not_found"}
                    ] };
        UserRecord ->
            SubRecords = boss_db:find(subscription, [{'user_uuid', 'equals', UserId}]),
            SubRooms = lists:map(fun(X) -> list_to_binary(X:room_uuid()) end, SubRecords),
            {json, [
                {user_uuid, UserId},
                {sub_rooms, SubRooms}
            ]}
    end;
user('POST', [], _AuthInfo) ->
    #auth_info{apikey=Apikey} = _AuthInfo,
    UserId = get_uuid_id(user),
    io:format("generate user id: ~p, creating user instance~n", [UserId]),
    NewUser = users:new(id, UserId, Apikey),
    case NewUser:save() of
        {ok, _UserRecord} -> {json, [
                    {status, "ok"},
                    {user_id, NewUser:user_uuid()}
                    ]};
        {error, Msgs} -> {json, [
                    {status, "error"},
                    {msg, lists:nth(1, Msgs)}
                    ]}
    end;
user('DELETE', [], _AuthInfo) ->
    % TODO: delete related subscription after deleting users.
    Apikey = Req:post_param("apikey"),
    case check_apikey(Apikey) of
        false -> {unauthorized, "api key not found."};
        true -> {output, "not implemented yet."}
    end.
