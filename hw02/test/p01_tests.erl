-module(p01_tests).

-include_lib("eunit/include/eunit.hrl").

last_test() -> 
	p01:last([]) =:= empty.

% генератор тестів
last_test_() -> [
	?_assert(p01:last([])      =:= empty),
	?_assert(p01:last([0])     =:= 0),
	?_assert(p01:last([0,1,2]) =:= 2)].

