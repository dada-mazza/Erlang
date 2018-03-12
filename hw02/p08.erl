-module(p08).

-import(p05,[reverse/1]).

-export([compress/1]).

compress([])     -> [];
compress([H|[]]) -> [H];
compress([H|T])  -> compress(T, [H]).


compress([H|T], [H|_] = Acc) -> compress(T, Acc);
compress([H|T], Acc)         -> compress(T, [H|Acc]);
compress([], Acc)            -> p05:reverse(Acc).



