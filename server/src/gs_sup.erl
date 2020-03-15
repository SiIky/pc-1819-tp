-module(gs_sup).
-behaviour(supervisor).
-export([
         init/1,
         start/1
        ]).

start(LPort) -> supervisor:start_link(?MODULE, LPort).

init(LPort) ->
    SupFlags = {one_for_all, 2, 10},

    ACCSpec = #{
      id => acc,
      start => {acc, start_link, [LPort]},
      restart => transient,
      shutdown => 10000,
      type => worker,
      modules => [acc]
     },

    LMSpec = #{
      id => lm,
      start => {lm, start_link, []},
      restart => transient,
      shutdown => 10000,
      type => worker,
      modules => [lm]
     },

    MMSpec = #{
      id => mm,
      start => {mm, start_link, []},
      restart => transient,
      shutdown => 10000,
      type => worker,
      modules => [mm]
     },

    ChildSpecs = [ ACCSpec, LMSpec, MMSpec ],

    {ok, {SupFlags, ChildSpecs}}.
