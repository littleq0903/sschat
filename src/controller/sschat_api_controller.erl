-module(sschat_api_controller, [Req]).
-compile(export_all).

-record(auth_info, {apikey}).

get_uuid_id(apikey) ->
    "apikey_" ++ get_uuid_id();
get_uuid_id(room) ->
    "room_" ++ get_uuid_id();
get_uuid_id(user) ->
    "user_" ++ get_uuid_id().
get_uuid_id() ->
    re:replace(uuid:to_string(uuid:uuid4()), "-", "", [global, {return, list}]).

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
        

check_apikey(ApikeyFromReq) ->
    Count = boss_db:count(apikey, [api_key, 'equals', ApikeyFromReq]),
    case Count of
        0 -> false;
        _ -> true
    end.

% pre-processing

before_(_) ->
    Apikey = Req:query_param("apikey"),
    case check_apikey(Apikey) of
        true ->
            AuthInfo = #auth_info{apikey=Apikey},
            {ok, AuthInfo};
        _ -> {redirect, "/api/permission_denied"}
    end.

permission_denied('GET', []) ->
    ok.

% register apis
register('GET', [ApiKey]) ->
    %% API to fetch the information of an api key.
    %% REQUIRE ARGUMENTS:
    %%      none
    Count = boss_db:count(apikey, [api_key, 'equals', ApiKey]),
    case Count of
        0 -> {json, [{status, "not_found"}]};
        Otherwise -> {json, [
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
        {ok, SavedBossRecord} -> {json, [
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


% rooms apis
room('GET', [RoomId], AuthInfo) ->
    #auth_info{apikey=Apikey} = AuthInfo,
    Room = boss_db:find_first(room, [
                {room_uuid, 'equalsf', RoomId},
                {api_key, 'equals', Apikey}]),
    case Room of
        undefined ->
            { json, [
                    {status, "error"},
                    {msg, "not_found"}
                    ] };
        RoomRecord -> 
            { json, [
                    {status, "ok"},
                    {data, [
                            {members, mochijson2:encode(["a", "b"])},
                            {room_uuid, RoomRecord:room_uuid()}
                            ]}
                    ] }
    end;
room('POST', [], AuthInfo) ->
    #auth_info{apikey=Apikey} = AuthInfo,
    RoomId = get_uuid_id(room),
    io:format("Received request to create a room, assigned room id: ~p~n", [RoomId]),
    Room = room:new(id, RoomId, ApiKey),
    case Room:save() of
        {ok, SavedBossRecord} -> {json, [
                    {status, "ok"}
                    ]};
        {error, Msgs} -> {json, [
                    {status, "error"},
                    {msg, lists:nth(1, Msgs)}
                    ]}
    end;
room('DELETE', [RoomId], AuthInfo) ->
    Apikey = Req:post_param("apikey"),
    case check_apikey(Apikey) of
        false -> {unauthorized, "api key not found."};
        true -> {output, "not implemented yet."}
    end;
room('PUT', [RoomId], AuthInfo) ->
    Apikey = Req:post_param("apikey"),
    case check_apikey(Apikey) of
        false -> {unauthorized, "api key not found."};
        true -> {output, "not implemented yet."}
    end.


% users apis
user('GET', []) ->
    Apikey = Req:query_param("apikey"),
    case check_apikey(Apikey) of
        false -> {unauthorized, "api key not found."};
        true -> {output, "not implemented yet."}
    end;
user('POST', []) ->
    Apikey = Req:post_param("apikey"),
    case check_apikey(Apikey) of
        false -> {unauthorized, "api key not found."};
        true -> {output, "not implemented yet."}
    end;
user('DELETE', []) ->
    Apikey = Req:post_param("apikey"),
    case check_apikey(Apikey) of
        false -> {unauthorized, "api key not found."};
        true -> {output, "not implemented yet."}
    end.
