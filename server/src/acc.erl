-module(acc).
-export([
         start/0,
         stop/0
        ]).

-define(DEFAULT_PORT, 4242).
%%tells 
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
            case gen_tcp:controlling_process(Socket, Cl) of
                ok ->
                    lm:new_client(Cl);
                {error, Reason} ->
                    cl:stop(Cl),
                    io:format("Error changing the controlling process: ~p", [Reason])
            end,
            handle_msgs(LSock);
        {error, timeout} -> handle_msgs(LSock);
        {error, _}=E -> E
    end.

handle_msgs(LSock) ->
    receive
        stop ->
            gen_tcp:close(LSock),
            ok;
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            handle_msgs(LSock)
    after 0 ->              acc(LSock)

              acc(LSock)
    end.
