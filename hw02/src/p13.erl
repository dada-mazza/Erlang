-module(p13).

-import(p05,[reverse/1]).

-export([decode/1]).

decode([])    -> [];
decode([H|T]) -> decode(H, T, []).


decode({Count,Var}, [H1|T1], Acc) -> decode(H1, T1, addElem(Count, Var, Acc));
decode({Count,Var}, [], Acc)      -> reverse(addElem(Count, Var, Acc)).


addElem(1, Var, Acc)     ->  [Var|Acc];
addElem(Count, Var, Acc) -> addElem(Count-1, Var, [Var|Acc]).

