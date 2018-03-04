-module(p12).

-import(p05,[reverse/1]).

-export([decode_modified/1]).

decode_modified([])     -> [];
decode_modified([H|T])  -> decode_modified(H, T, []).


decode_modified({Count,Var}, [H1|T1], Acc) -> decode_modified(H1, T1, addElem(Count, Var, Acc));
decode_modified({Count,Var}, [], Acc)      -> reverse(addElem(Count, Var, Acc));
decode_modified(H, [H1|T1], Acc)           -> decode_modified(H1, T1, [H|Acc]);
decode_modified(H, [], Acc)                -> reverse([H|Acc]).

addElem(1, Var, Acc) ->  [Var|Acc];
addElem(Count, Var, Acc) -> addElem(Count-1, Var, [Var|Acc]).

