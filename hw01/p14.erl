-module(p14).

-import(p05,[reverse/1]).

-export([duplicate/1]).

duplicate([])-> [];
duplicate(H) -> duplicate(H, []).

duplicate([H|T], Acc) -> duplicate(T, [H,H|Acc]);
duplicate([], Acc) -> p05:reverse(Acc).

duplicateR([])    -> [];
duplicateR([H|T]) -> [H,H|duplicateR(T)].
