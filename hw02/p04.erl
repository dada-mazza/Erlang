-module(p04).

-export([len/1, rlen/1]).

len([]) -> 0;
len([_|T]) -> len(T,1).

len([_|T], Acc) -> len(T, Acc+1);
len([], Acc) -> Acc.


rlen([_|T]) -> 1 + rlen(T);
rlen([]) -> 0.






