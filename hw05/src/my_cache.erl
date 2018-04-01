-module(my_cache).

-export([create/0, insert/3, lookup/1, delete_obsolete/0]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(CACHE_NAME, cache).


create() -> ?CACHE_NAME = ets:new(?CACHE_NAME, [public, named_table]), ok.

insert(Key, Value, Timeout) when is_integer(Timeout) -> 
	Time_Insert = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
	true = ets:insert(?CACHE_NAME, {Key, Value, Timeout, Time_Insert}),
	ok;
insert(_, _, Timeout) -> {bad_timeout, Timeout}.

lookup(Key) ->	
	Time_Current = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
	%% erlang:display(Time_Current),
	
	MS = ets:fun2ms(fun({Key1, Value, Timeout, Time_Insert}) 
						when Key =:= Key1, Timeout >= (Time_Current - Time_Insert)->
					Value end),
	Res_List = ets:select(?CACHE_NAME, MS),
	case Res_List of
		[] -> {empty};
		[Res] -> {ok, Res}
	end.
	

lookup_old() ->	
	Time_Current = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
	%% erlang:display(Time_Current),
	
	MS = ets:fun2ms(fun({Key, _, Timeout, Time_Insert}) 
						when Timeout =< (Time_Current - Time_Insert)->
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
	


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 10).

create_test_() -> [
	?_assert(ok =:= my_cache:create())
	].

insert_test_() -> [
	?_assert(ok =:= my_cache:insert(1, 1, ?TIMEOUT)),
	?_assert(ok =:= my_cache:insert(2, 2, ?TIMEOUT)),
	?_assert(ok =:= my_cache:insert(3, 3, ?TIMEOUT)),
	?_assert({bad_timeout, "600"} =:= my_cache:insert(1, 1, "600"))
	].

lookup_test_() -> [
	?_assertEqual({ok, 1}, my_cache:lookup(1)),
	?_assertEqual({ok, 2}, my_cache:lookup(2)),
	?_assertEqual({ok, 3}, my_cache:lookup(3)),
	?_assertEqual({empty}, my_cache:lookup(4))
	].

slow_test_() ->
	{timeout, ?TIMEOUT + 10,
	fun() ->
		timer:sleep(?TIMEOUT * 1000 + 1000)
	end}.

lookup1_test_() -> [   
	?_assertEqual({empty}, my_cache:lookup(1)),
	?_assertEqual({empty}, my_cache:lookup(2)),
	?_assertEqual({empty}, my_cache:lookup(3)),
	?_assertEqual({empty}, my_cache:lookup(4)),	
	?_assertEqual(ok, my_cache:insert(1, 1, ?TIMEOUT)),
	?_assertEqual({ok, 1}, my_cache:lookup(1))
	].

delete_obsolete_test_() -> [
	?_assert(ok =:= my_cache:delete_obsolete()),
	?_assertEqual({ok, 1}, my_cache:lookup(1))
	].

%ets:delete(?CACHE_NAME).

-endif.

%% ok = my_cache:create().                %% Создание кеш-таблицы
%% ok = my_cache:insert(Key, Value, 600). %% Ключ, Значение, Время жизни записи
%% {ok, Value} = my_cache:lookup(Key).    %% Получить значение по ключу, функция должна доставать только НЕ устаревшие данные
%% ok = my_cache:delete_obsolete().       %% Очистка утстаревших данных