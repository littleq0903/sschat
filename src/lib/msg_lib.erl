-module(msg_lib).
-compile(export_all).


% utils
%% get_timestamp(<type>, <timetuple>).
%% Usage: get_timestamp(sec, erlang:now()) -> <int>
%%   <type> := sec | microsec
%%   <timetuple> := {Mega, Sec, Micro} (generated by erlang:now())
%%
get_timestamp(sec, Timetuple) ->
    {Mega, Sec, Micro} = Timetuple,
    (Mega * 1000000) + Sec;
get_timestamp(microsec, Timetuple) ->
    {_, _, Micro} = Timetuple,
    (get_timestamp(sec, Timetuple) * 1000000) + Micro.

%% 
get_json(message, Data, From, To) ->
    Timestamp = get_timestamp(microsec, erlang:now()),
    MsgUuid = list_to_binary(get_uuid_id(msg)),
    get_json(message, Data, From, To, Timestamp, MsgUuid).
get_json(message, Data, From, To, Timestamp, MsgUuid) ->
    [
        {message, <<"Message">>},
        {data, Data},
        {from, From},
        {to, To},
        {timestamp, Timestamp},
        {msg_uuid, MsgUuid}
    ].

%% get_uuid_id(<prefix>)
%% Usage:
%%   <prefix> := <atom> 
get_uuid_id(Prefix) ->
    atom_to_list(Prefix) ++ "_" ++ get_uuid_id().
get_uuid_id() ->
    re:replace(uuid:to_string(uuid:uuid4()), "-", "", [global, {return, list}]).
