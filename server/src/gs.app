% vim: ft=erlang
{application,
 gs,
 [
  {description, "The Game Server"},
  {vsn, "1.0.0"},
  {mod, {gs, wut}}, % wut will be the Args in gs:start/2
  {modules,
   [
    "acc",
    "cl",
    "gs",
    "lm",
    "map",
    "match",
    "mm",
    "srv",
    "ts"
   ]
  },
  {registered,
   [
    acc,
    gs,
    lm,
    mm,
    ts
   ]
  }
 ]
}.
