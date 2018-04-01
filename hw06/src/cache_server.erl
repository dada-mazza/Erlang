-module(cache_server).

-export([start_link/1, insert/3, lookup/1, lookup_by_date/2]).

-define(MODULE_HANDLER, cache_handler).

-callback start_cache({drop_interval, any()}) -> any().


start_link([{drop_interval, Drop_interval}]) when is_integer(Drop_interval)-> 
	case get(pid) of
		undefined ->
			Pid = spawn(?MODULE_HANDLER, start_cache, [{drop_interval, Drop_interval}]),
			Pid ! {self(), start},
			receive 
				{Pid, answer_start} -> 
					put(pid, Pid),
					erlang:display({?MODULE, {ok, get(pid)}}),
					{ok, get(pid)};
				Msg -> 
					{wrong_message, Msg}
			end;
		Pid -> 	
			{ok, Pid}		
			end;
start_link(Argument) ->
	{bad_argument, Argument}.



insert(Key, Value, Timeout) when is_integer(Timeout) -> 
	get(pid) ! {self(), insert, Key, Value, Timeout},
	receive
		{_, insert_answer, Result} ->
			Result;
		Msg -> 
			erlang:display({wrong_message, Msg})
	end;		
insert(_, _, Timeout) -> {bad_timeout, Timeout}.

			
lookup(Key) ->	
	get(pid) ! {self(), lookup, Key},
	receive
		{_, lookup_answer, Result} ->
			Result;
		Msg -> 
			erlang:display({wrong_message, Msg})
	end.		

lookup_by_date(DateFrom, DateTo) ->	
	get(pid) ! {self(), lookup_by_date, DateFrom, DateTo},
	receive
		{_, lookup_by_date_answer, Result} ->
			Result;
		Msg -> 
			erlang:display({wrong_message, Msg})
	end.			



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 10).
-define(DROP_INTERVAL, 20).

start_link_test_() -> [
	?_assertMatch({ok, _}, start_link([{drop_interval, 3600}]))
	].

insert_test_() -> [
	?_assert(ok =:= ?MODULE:insert(1, 1, ?TIMEOUT)),
	?_assert(ok =:= ?MODULE:insert(2, 2, ?TIMEOUT)),
	?_assert(ok =:= ?MODULE:insert(3, 3, ?TIMEOUT)),
	?_assert({bad_timeout, "600"} =:= ?MODULE:insert(1, 1, "600"))
].

lookup_test_() -> [
	?_assertEqual({ok, 1}, ?MODULE:lookup(1)),
	?_assertEqual({ok, 2}, ?MODULE:lookup(2)),
	?_assertEqual({ok, 3}, ?MODULE:lookup(3)),
	?_assertEqual({empty}, ?MODULE:lookup(4))
].

lookup_by_date_test_() -> [
	?_assertEqual({ok, [{1,1},{3,3},{2,2}]}, 
		?MODULE:lookup_by_date(
			calendar:local_time(),
			calendar:gregorian_seconds_to_datetime(
				calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 60*60)
		)
	)
].

slow_test_() ->
	{timeout, ?TIMEOUT*2 + 10,
	fun() ->
		timer:sleep(?TIMEOUT * 1000 + 1000)
	end}.

lookup_by_date_2_test_() -> [
	?_assertEqual({empty}, 
		?MODULE:lookup_by_date(
			calendar:local_time(),
			calendar:gregorian_seconds_to_datetime(
				calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 60*60)
		)
	)
].

lookup_1_test_() -> [   
	?_assertEqual({empty}, ?MODULE:lookup(1)),
	?_assertEqual({empty}, ?MODULE:lookup(2)),
	?_assertEqual({empty}, ?MODULE:lookup(3)),
	?_assertEqual({empty}, ?MODULE:lookup(4)),	
	?_assertEqual(ok, ?MODULE:insert(1, 1, ?TIMEOUT)),
	?_assertEqual({ok, 1}, ?MODULE:lookup(1))
].

lookup_by_date_3_test_() -> [
	?_assertEqual({ok, [{1,1}]}, 
		?MODULE:lookup_by_date(
			calendar:local_time(),
			calendar:gregorian_seconds_to_datetime(
				calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 60*60)
		)
	)
].

-endif.

% {ok, Pid} = cache_server:start_link([{drop_interval, 3600}]).
% ok = cache_server:insert(Key, Value, 600). %% Ключ, Значение, Время жизни записи
% {ok, Value} = cache_server:lookup(Key).
% DateFrom = {{2015,1,1},{00,00,00}}.
% DateTo = {{2015,1,10},{23,59,59}}.
% {ok, Value} = cache_server:lookup_by_date(DateFrom, DateTo).
