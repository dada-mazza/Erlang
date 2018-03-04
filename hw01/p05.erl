-module(p05).

-export([reverse/1]).

reverse([H|T]) -> reverse(T, [H]). 

reverse([H|T], Acc) -> reverse(T, [H|Acc]);
reverse([], Acc) -> Acc.


