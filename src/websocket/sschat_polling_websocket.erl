-module(sschat_polling_websocket).
-behaviour(boss_service_handler).

-import(msg_lib, [get_json/4]).
-compile(export_all).
-export([init/0, handle_join/4, handle_info/2, terminate/2]).

-record(state, {user_ids, users, last_msg}).
%
% user_ids: [{user_id, websocketid}]
% users: dict [{websocketid, user_id}]
%


get_room_subscription(RoomId) ->
    Records = boss_db:find(subscription, [{'room_uuid', equals, RoomId}]),
    lists:map(fun(X) -> X:user_uuid() end, Records).

send_to_matched(Users, FromUser, ToRoomId, Msg) ->
    MatchedUsers = get_room_subscription(ToRoomId),
    MatchedHandler = fun (K, V) ->
            io:format("K:~p, V:~p~n", [K, V]),
            IsMatch = lists:member(binary_to_list(V), MatchedUsers),
            io:format("IsMatch: ~p~n", [IsMatch]),
            if 
                IsMatch -> 
                    %io:format("DEBUG: ~p:~p:~p~n", [Msg, FromUser, ToRoomId]),
                    K ! {text, jsx:encode(get_json(message, Msg, FromUser, ToRoomId ))},
                    true;
                true -> false
            end
    end,
    dict:map(MatchedHandler, Users).


init() ->
    {ok, #state{users=dict:new()}}.


handle_join(ServiceName, WebSocketId, SessionId, State) ->
    MsgToResp = io_lib:format("ServiceName: ~p~nWebsocketId: ~p~nSessionId: ~p~n", [ServiceName, WebSocketId, SessionId]),
    #state{users=Users} = State,
    %WebSocketId ! { text, MsgToResp },
    FixedSessionId = case SessionId of
        undefined -> <<"user_not_logon">>;
        NotEmptySessionId -> SessionId
    end,
    io:format("[WS:JOIN] (ServiceName: ~p, WebSocketId:~p, SessionId:~p)~n", [ServiceName, WebSocketId, FixedSessionId]),
    {reply, ok, State#state{users=dict:store(WebSocketId, FixedSessionId, Users)}}.

handle_close(ServiceName, WebSocketId, SessionId, State) ->
    #state{users=Users} = State,
    io:format("[WS:CLOSE] (ServiceName: ~p, WebSocketId:~p, SessionId:~p)~n", [ServiceName, WebSocketId, SessionId]),
    {reply, ok, State#state{users=dict:erase(WebSocketId, Users)}}.

handle_incoming(_ServiceName, WebSocketId, SessionId, Message, State) ->
    io:format("[WS:INCOMING] msg: ~p~n (~p:~p:~p)~n", [Message, _ServiceName, WebSocketId, SessionId]),
    io:format("[WS:INCOMING] State: ~p~n", [State]),
    Accepted = jsx:is_json(Message),
    ResultState = if 
        Accepted -> 
            #state{users=Users} = State,
            ParsedMessage = jsx:decode(Message),
            Command = proplists:get_value(<<"command">>, ParsedMessage),
            case Command of
                <<"room">> ->
                    User_id = case dict:find(WebSocketId, Users) of
                        {ok, Result} -> Result;
                        {error, _} -> "error_id"
                    end,
                    Room_id = proplists:get_value(<<"room">>, ParsedMessage),
                    MessageToSend = proplists:get_value(<<"msg">>, ParsedMessage),

                    Ids = dict:erase(WebSocketId, Users),
                    send_to_matched(Ids, User_id, Room_id, MessageToSend),
                    State;
                <<"show-status">> ->
                    io:format("~p~n", [State]),
                    State;
                <<"login">> ->
                    LoginUser = proplists:get_value(<<"user">>, ParsedMessage),
                    State#state{users=dict:store(WebSocketId, LoginUser, Users)}
            end;
        true ->
            State
    end,
    io:format("[WS:INCOMING] ResultState: ~p~n", [ResultState]),
    {noreply, ResultState}.

handle_info(state, State) ->
	error_logger:info_msg("state: ~p~n", [State]),
	{noreply, State};

handle_info(ping, State) ->
	error_logger:info_msg("pong:~p~n", [now()]),
	{noreply, State};

handle_info(tic_tac, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
