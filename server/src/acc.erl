-module(acc).
-export([
         start/1,
         start_link/1,
         stop/0
        ]).

start(LPort) ->
    {ok, LSock} = gen_tcp:listen(LPort, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    Pid = spawn(fun() -> acc(LSock) end),
    register(?MODULE, Pid),
    {ok, Pid}.

start_link(LPort) ->
    {ok, LSock} = gen_tcp:listen(LPort, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    Pid = spawn_link(fun() -> acc(LSock) end),
    register(?MODULE, Pid),
    {ok, Pid}.

stop() ->
    srv:stop(?MODULE).

%%waits for accept on listen socket, if ok connection is established and creates a new client with that scoket.
%% change the process who receives message from the socket to client from acceptor (Default). If this is possible
%% tell LM there's a new client in preauth, else error and stop.
%%if 1s passes since the last connection, stop the acceptor so it can start receiving messages again.
acc(LSock) ->
    case gen_tcp:accept(LSock, 1000) of
        {ok, Socket} ->
            Cl = cl:new(Socket),
            case gen_tcp:controlling_process(Socket, Cl) of
                ok ->
                    cl:changed_controlling_process(Cl, Socket),
                    lm:new_client(Cl);
                {error, Reason} ->
                    cl:stop(Cl),
                    io:format("Error changing the controlling process: ~p\n", [Reason])
            end,
            handle_msgs(LSock);
        {error, timeout} -> handle_msgs(LSock);
        {error, _}=E -> E
    end.

%% if there're any messages received, process them, else go to acceptor.

handle_msgs(LSock) ->
    receive
        stop ->
            gen_tcp:close(LSock),
            ok;
        Msg ->
            io:format("acc:handle_msgs:unexpected: ~p\n", [Msg]),
            handle_msgs(LSock)
    after 0 ->
              acc(LSock)
    end.
