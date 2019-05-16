-module(lm).
-export([
         % start/stopping
         start/0,
         stop/0
        ]).

%%
%%
%%

start() ->
    Pid = spawn(fun() -> server(init()) end),
    register(?MODULE, Pid),
    ok.

stop() ->
    ?MODULE ! stop,
    ok.

init() ->
    dict:new().

server(Accs) ->
    receive
        stop -> ok;
        {call, {Pid, Ref}=From, Msg}
          when
              is_pid(Pid),
              is_reference(Ref) ->
            server(handle_call(Accs, From, Msg));
        {cast, Msg} ->
            server(handle_cast(Accs, Msg))
    end.

handle_call(Accs, From, _Msg) ->
    srv:reply(From, sup),
    Accs.

handle_cast(Accs, _Msg) ->
    Accs.
