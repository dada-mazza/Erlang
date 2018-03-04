-module(p07).

-import(p05,[reverse/1]).

-export([flatten/1]).

flatten([[]])   -> [];
flatten([H|[]]) -> [H];
flatten([H|T])  -> flatten(H, T, []).


flatten([], [H1|T], Acc) -> flatten(H1, T, Acc);
flatten([H|T], T1, Acc)  -> flatten(H, [T|T1], Acc);
flatten(H, [H1|T], Acc)  -> flatten(H1, T, [H|Acc]);
flatten([], [], Acc)     -> p05:reverse(Acc);
flatten(H, [], Acc)      -> p05:reverse([H|Acc]).



