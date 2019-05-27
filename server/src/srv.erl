-module(srv).
-export([
         call/2,
         cast/2,
         recv/1,
         reply/2,
         stop/1
        ]).

call(Srv, Msg) ->
    Ref = make_ref(),
    Srv ! {call, {self(), Ref}, Msg},
    Ref.

cast(Srv, Msg) ->
    Srv ! {cast, Msg},
    ok.

recv(Ref) ->
    receive
        {Ref, Reply} -> Reply
    end.

reply({From, Ref}, Reply) ->
    From ! {Ref, Reply}.

stop(Who) ->
    Who ! stop,
    ok.
