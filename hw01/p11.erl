-module(p11).

-import(p05,[reverse/1]).

-export([encode_modified/1]).

encode_modified([])     -> [{}];
encode_modified([H|[]]) -> [H];
encode_modified([H|T])  -> encode_modified(T, [H]).


encode_modified([H|T], [{Count,H}|T1]) -> encode_modified(T, [{Count +1,H}|T1]);
encode_modified([H|T], [H|T1])         -> encode_modified(T, [{2,H}|T1]);
encode_modified([H|T], Acc)            -> encode_modified(T, [H|Acc]);
encode_modified([], Acc)               -> p05:reverse(Acc).



