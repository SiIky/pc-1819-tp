-module(gs_app).
-behavior(application).
-export([
         start/2,
         stop/1
        ]).

start(normal, LPort) -> gs_sup:start(LPort).
stop(_State) -> ok.
