-module(sschat_polling_websocket).
-behaviour(boss_service_handler).


-compile(export_all).
%-export([init/0, handle_join/4, handle_info/2, terminate/2]).

-record(state, {users}).


init() ->
    {ok, #state{users=dict:new()}}.


handle_join(_ServiceName, WebSocketId, SessionId, State) ->
    #state{users=Users} = State,
    io:format("[WS:JOIN](~p:~p:~p)~n", [_ServiceName, WebSocketId, SessionId]),
    {reply, ok, #state{users=dict:store(WebSocketId, SessionId, Users)}}.

handle_close(ServiceName, WebSocketId, SessionId, State) ->
    #state{users=Users} = State,
    {reply, ok, #state{users=dict:erase(WebSocketId, Users)}}.

handle_incoming(_ServiceName, WebSocketId, SessionId, Message, State) ->
    io:format("[WS:INCOMING] msg: ~p~n (~p:~p:~p)~n", [Message, _ServiceName, WebSocketId, SessionId]),
    WebSocketId ! {text, <<"Response from server">>},
    {noreply, State}.


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
