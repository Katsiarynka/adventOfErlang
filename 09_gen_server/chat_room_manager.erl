-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2, handle/2]).
-export([loop/1]). % internal function


start() ->
    io:format("starting ~p~n", [self()]),
    State = #{rooms => #{}, users => #{}, messages => #{}},
    spawn(?MODULE, loop, [State]).

call(Server, Query) ->
    Client = self(),
    Server ! {call, Client, Query},
    receive
        {ok, Reply} -> Reply;
        {error, ErrorMsg} -> {error, ErrorMsg};
        UnknownMsg -> 
            io:format("Error, UknownMsg ~p~n", [UnknownMsg]),
            {error, un_known_msg}
    after 
        500 -> {error, error}
    end.


create_room(Server, RoomName) ->
    call(Server, {create_room, RoomName}).

remove_room(Server, RoomId) ->
    call(Server, {remove_room, RoomId}).

get_rooms(Server) ->
    call(Server, get_rooms).

add_user(Server, RoomId, UserName) ->
    call(Server, {add_user, RoomId, UserName}).

remove_user(Server, RoomId, UserName) ->
    call(Server, {remove_user, RoomId, UserName}).

get_users_list(Server, RoomId) ->
    call(Server, {get_users_list, RoomId}).

send_message(Server, RoomId, UserName, Message) ->
    call(Server, {send_message, RoomId, UserName, Message}).

get_messages_history(Server, RoomId) ->
    call(Server, {get_messages_history, RoomId}).


handle(State, {get_users_list, RoomId}) ->
    {_, Users} = maps:find(users, State),
    case maps:find(RoomId, Users) of
        {ok, UsersInRoom} -> 
            {{ok, UsersInRoom}, State};
        error ->
            {error, room_not_found}
    end;

handle(State, {remove_user, RoomId, UserName}) ->
    {_, Users} = maps:find(users, State),
    case maps:find(RoomId, Users) of
        {ok, UsersInRoom} -> 
            case lists:filter(fun(User) -> User == UserName end, UsersInRoom) of
                [] -> {error, user_is_in_room};
                _ -> 
                    UsersInRoom2 = ists:filter(fun(User) -> User /= UserName end, UsersInRoom),
                    State2 = State#{users => Users#{RoomId => UsersInRoom2}},
                    {ok, State2}
            end;
        error ->
            {error, room_not_found}
    end;

handle(State, {add_user, RoomId, UserName}) -> 
    {_, Users} = maps:find(users, State),
    case maps:find(RoomId, Users) of
        {ok, UsersInRoom} -> 
            case lists:filter(fun(User) -> User == UserName end, UsersInRoom) of
                [] -> UsersInRoom2 = lists:append(UsersInRoom, [UserName]),
                    State2 = State#{users => Users#{RoomId => UsersInRoom2}},
                    {{ok, ok}, State2};
                _ -> {error, user_is_in_room}
            end;
        error ->
            {error, room_not_found}
    end;

handle(State, {create_room, RoomName}) ->
    {_, Rooms} = maps:find(rooms, State),
    {_, Users} = maps:find(users, State),
    case maps:size(Rooms) of
        S when S >= 5 -> {{error, room_limit}, State};
        _ ->
            RoomId = erlang:unique_integer(),
            Rooms2 = Rooms#{RoomId => RoomName}, 
            Users2 = Users#{RoomId => []}, 
            State2 = State#{rooms => Rooms2, users => Users2},
            {{ok, RoomId}, State2}
    end;


handle(State, {remove_room, RoomId}) ->
    {_, Rooms} = maps:find(rooms, State),
    {_, Users} = maps:find(users, State),
    case maps:find(RoomId, Rooms) of
        {ok, Value} -> 
            Rooms2 = maps:remove(RoomId, Rooms), 
            Users2 = maps:remove(RoomId, Users), 
            State2 = State#{rooms => Rooms2, users => Users2},
            {ok, State2};  
        error ->
            {error, room_not_found}
    end;

handle(State, get_rooms) ->
    {_, Rooms} = maps:find(rooms, State),
    {maps:fold(fun(RoomId, RoomName, Acc) -> [{RoomId, RoomName} | Acc] end, [], Rooms), State}.



loop(State) ->
    receive
        {call, Client, Query} ->
            case handle(State, Query) of
                {error, TextError} ->
                    Client ! {error, TextError},
                    ?MODULE:loop(State);
                {Reply, State2} ->
                    Client ! {ok, Reply},
                    io:format("got response on ~p - ~p ~n", [Query, Reply]),
                    ?MODULE:loop(State2);
                Msg -> io:format("Got from handle: ~p in ~p~n", [Msg, self()]),
                    ?MODULE:loop(State)
            end;
        stop ->
            io:format("~p is stopping~n", [self()]),
            ok;
        Msg -> io:format("Msg received: ~p in ~p~n", [Msg, self()]),
            ?MODULE:loop(State)
    end.


stop(Server) ->
    Server ! stop.
