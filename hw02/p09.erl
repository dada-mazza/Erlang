-module(p09).

-import(p05,[reverse/1]).

-export([pack/1]).

pack([])     -> [];
pack([H|[]]) -> [[H]];
pack([H|T])  -> pack(T, [[H]]).


pack([H|T], [[H|_] = H1|T1]) -> pack(T, [[H|H1]|T1]);
pack([H|T], Acc)             -> pack(T, [[H]|Acc]);
pack([], Acc)                -> p05:reverse(Acc).



