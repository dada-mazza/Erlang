-module(p15).

-import(p05,[reverse/1]).

-export([replicate/2]).

replicate([], _) -> [];
replicate(L, Count) -> replicate(Count, L, Count, []).

replicate(R, [H|_], 1, Acc)     -> reverse(addElem(R, H, Acc));
replicate(R, [H|T], Count, Acc) -> replicate(R, T, Count-1, addElem(R, H, Acc)).


addElem(1, Var, Acc) -> [Var|Acc];
addElem(R, Var, Acc) -> addElem(R-1, Var, [Var|Acc]).
