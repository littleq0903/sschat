-module(sschat_polling_websocket).
-behaviour(boss_service_handler).


-compile(export_all).
%-export([init/0, handle_join/4, handle_info/2, terminate/2]).

-record(state, {user_ids, users, last_msg}).
%
% user_ids: [{user_id, websocketid}]
% users: dict [{websocketid, user_id}]
%

% utils
%

send_to_matched(Users, MatchedUsers, Msg) ->
    io:format("msg: ~p~n", [Msg]),
    MatchedHandler = fun (K, V) ->
            io:format("K:~p, V:~p~n", [K, V]),
            IsMatch = lists:member(binary_to_list(V), MatchedUsers),
            io:format("IsMatch: ~p~n", [IsMatch]),
            if 
                IsMatch -> 
                    K ! {text, Msg},
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
    io:format("[WS:JOIN](~p:~p:~p)~n", [ServiceName, WebSocketId, SessionId]),
    %WebSocketId ! { text, MsgToResp },
    {reply, ok, State#state{users=dict:store(WebSocketId, SessionId, Users)}}.

handle_close(_ServiceName, WebSocketId, _SessionId, State) ->
    #state{users=Users} = State,
    {reply, ok, State#state{users=dict:erase(WebSocketId, Users)}}.

handle_incoming(_ServiceName, WebSocketId, SessionId, Message, State) ->
    %io:format("[WS:INCOMING] msg: ~p~n (~p:~p:~p)~n", [Message, _ServiceName, WebSocketId, SessionId]),
    %io:format("[WS:INCOMING] State: ~p~n", [State]),
    Accepted = jsx:is_json(Message),
    if 
        Accepted -> 
            #state{users=Users} = State,
            ParsedMessage = jsx:decode(Message),
            Command = proplists:get_value(<<"command">>, ParsedMessage),
            case Command of
                <<"room">> ->
                    Room_id = proplists:get_value(<<"room">>, ParsedMessage),
                    MessageToSend = proplists:get_value(<<"msg">>, ParsedMessage),

                    Ids = dict:erase(WebSocketId, State#state.users),
                    Subscriptions = boss_db:find(subscription, [{'room_uuid', equals, Room_id}]),
                    MatchedUser = lists:map(fun(X) -> X:user_uuid() end, Subscriptions),
                    io:format("MatchedUser: ~p~n", [MatchedUser]),
                    send_to_matched(dict:erase(WebSocketId, Users), MatchedUser, MessageToSend),
                    CommandedState = State;
                <<"login">> ->
                    LoginUser = proplists:get_value(<<"user">>, ParsedMessage),
                    CommandedState = State#state{users=dict:store(WebSocketId, LoginUser, Users)}
            end,
            {noreply, CommandedState#state{last_msg=Message}};
        true ->
            {noreply, State}
    end.

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
