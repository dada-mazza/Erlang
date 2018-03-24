-module(p01).

-export([last/1]).

last([H|[]]) -> H;
last([_|T])  -> last(T);
last([])     -> empty.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

last_test_() -> [
	?_assert(last([])      =:= empty),
	?_assert(last([0])     =:= 0),
	?_assert(last([0,1,2]) =:= 2)].

-endif.