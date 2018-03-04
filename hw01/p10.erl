-module(p10).

-import(p05,[reverse/1]).

-export([encode/1]).

encode([])     -> [{}];
encode([H|[]]) -> [{1,H}];
encode([H|T])  -> encode(T, [{1,H}]).


encode([H|T], [{Count,H}|T1]) -> encode(T, [{Count +1,H}|T1]);
encode([H|T], Acc)            -> encode(T, [{1,H}|Acc]);
encode([], Acc)               -> p05:reverse(Acc).



