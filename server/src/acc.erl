-module(acc).
-export([
         start/0,
         stop/0
        ]).

-define(DEFAULT_PORT, 4242).

start() ->
    {ok, LSock} = gen_tcp:listen(?DEFAULT_PORT ,[binary,{packet,line},{reuseaddr,true}]),
    Pid = spawn(fun() -> acc(LSock) end),
    register(?MODULE, Pid),
    ok.

stop() ->
    srv:stop(?MODULE).

acc(LSock) ->
    case gen_tcp:accept(LSock, 1000) of
        {ok, Socket} ->
            Cl = cl:new(Socket),
            gen_tcp:controlling_process(Socket, Cl),
            lm:new_client(Cl),
            handle_msgs(LSock);
        {error, closed} ->
            handle_msgs(LSock);
        {error, timeout} ->
            handle_msgs(LSock)
    end.

handle_msgs(LSock) ->
    receive
        stop ->
            ok;
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            handle_msgs(LSock)
    after 0 ->
              acc(LSock)
    end.
