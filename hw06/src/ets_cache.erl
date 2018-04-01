-module(ets_cache).

-export([create/0]).
-export([insert/3]).
-export([lookup/1]).
-export([lookup_by_date/2]).
-export([delete_obsolete/0]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(CACHE_NAME, cache).

-behavior(cache_handler).


create() -> 
	case ets:info(?CACHE_NAME, name) of
		undefined ->
			?CACHE_NAME = ets:new(?CACHE_NAME, [private, named_table]),
			ok;
		?CACHE_NAME ->
			ok
	end.


insert(Key, Value, Timeout) when is_integer(Timeout) -> 
	Time = calendar:datetime_to_gregorian_seconds(calendar:local_time()) + Timeout,
	true = ets:insert(?CACHE_NAME, {Key, Value, Time}),
	ok;
insert(_, _, Timeout) -> {bad_timeout, Timeout}.


lookup(Key) ->	
	Time_Current = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	MS = ets:fun2ms(fun({Key1, Value, Time}) 
						when Key =:= Key1, Time >= Time_Current ->
					Value end),
	Res_List = ets:select(?CACHE_NAME, MS),
	case Res_List of
		[] -> {empty};
		[Res] -> {ok, Res}
	end.	

lookup_by_date(DateFrom, DateTo) ->	
	DateFromSec = calendar:datetime_to_gregorian_seconds(DateFrom),
	DateToSec = calendar:datetime_to_gregorian_seconds(DateTo),
	MS = ets:fun2ms(fun({Key, Value, Time}) 
						when Time >= DateFromSec, Time =< DateToSec ->
					{Key, Value}
					end),
	Res_List = ets:select(?CACHE_NAME, MS),
	%erlang:display(Res_List),
	case Res_List of
		[] -> {empty};
		Res -> {ok, Res}
	end.


lookup_old() ->	
	Time_Current = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	%% erlang:display(Time_Current),
	MS = ets:fun2ms(fun({Key, _, Time}) 
						when Time_Current >= Time ->
					Key end),
	ets:select(?CACHE_NAME, MS).


delete_obsolete() -> 
	ets:safe_fixtable(?CACHE_NAME, true),
	Res = delete_obsolete(lookup_old()),					
	ets:safe_fixtable(?CACHE_NAME, false),
	Res.

delete_obsolete([Key|T]) ->
	%% erlang:display(Key),
	ets:delete(?CACHE_NAME, Key),
	delete_obsolete(T);
delete_obsolete([]) -> ok.
	


% -ifdef(TEST).
% -include_lib("eunit/include/eunit.hrl").

% -define(TIMEOUT, 10).

% create_test_() -> [
% 	?_assert(ok =:= ?MODULE:create()),
% 	?_assert(ok =:= ?MODULE:create())
% ].

% insert_test_() -> [
% 	?_assert(ok =:= ?MODULE:insert(1, 1, ?TIMEOUT)),
% 	?_assert(ok =:= ?MODULE:insert(2, 2, ?TIMEOUT)),
% 	?_assert(ok =:= ?MODULE:insert(3, 3, ?TIMEOUT)),
% 	?_assert({bad_timeout, "600"} =:= ?MODULE:insert(1, 1, "600"))
% ].

% lookup_test_() -> [
% 	?_assertEqual({ok, 1}, ?MODULE:lookup(1)),
% 	?_assertEqual({ok, 2}, ?MODULE:lookup(2)),
% 	?_assertEqual({ok, 3}, ?MODULE:lookup(3)),
% 	?_assertEqual({empty}, ?MODULE:lookup(4))
% ].

% lookup_by_date_test_() -> [
% 	?_assertEqual({ok, [{1,1},{3,3},{2,2}]}, 
% 		?MODULE:lookup_by_date(
% 			calendar:local_time(),
% 			calendar:gregorian_seconds_to_datetime(
% 				calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 60*60)
% 		)
% 	)
% ].

% slow_test_() ->
% 	{timeout, ?TIMEOUT + 10,
% 	fun() ->
% 		timer:sleep(?TIMEOUT * 1000 + 1000)
% 	end}.

% lookup1_test_() -> [   
% 	?_assertEqual({empty}, ?MODULE:lookup(1)),
% 	?_assertEqual({empty}, ?MODULE:lookup(2)),
% 	?_assertEqual({empty}, ?MODULE:lookup(3)),
% 	?_assertEqual({empty}, ?MODULE:lookup(4)),	
% 	?_assertEqual(ok, ?MODULE:insert(1, 1, ?TIMEOUT)),
% 	?_assertEqual({ok, 1}, ?MODULE:lookup(1))
% ].

% delete_obsolete_test_() -> [
% 	?_assert(ok =:= ?MODULE:delete_obsolete()),
% 	?_assertEqual({ok, 1}, ?MODULE:lookup(1))
% ].

% -endif.
