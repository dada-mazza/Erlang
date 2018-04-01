-module(cache_handler).

-export([start_cache/1]).
-export([start/0]).
-export([start_drop/2]).
-export([drop_loop/2]).
-export([cache_loop/1]).

-behavior(cache_server).

-define(MODULE_CACHE, ets_cache).

-callback create() -> 
	ok.
-callback insert(any(), any(), Timeout::integer()) -> 
	ok |
    {bad_timeout, any()}.
-callback lookup(any()) -> 
	{ok, any()} |
    {empty}.
-callback lookup_by_date(DateFrom::datetime, DateTo::datetime) -> 
	{ok, any()} |
    {empty}.


start_cache({drop_interval, Drop_interval}) ->
	start(),
	start_drop(self(), Drop_interval),
	cache_loop(Drop_interval);
start_cache(Arg) ->
	{bad_argument, Arg}.


start() -> 
	receive
		{Pid, start} ->
			ok = ?MODULE_CACHE:create(),
			Pid ! {self(), answer_start};
		{Pid, Msg} -> 
			Pid ! {self(), wrong_message, Msg};
		Msg -> 
			erlang:display({wrong_message, Msg})
		end.

start_drop(Pid, Drop_interval) -> 
	{Drop_Pid, Ref} = spawn_monitor(?MODULE, drop_loop, [Pid, Drop_interval]),
	register(drop_pid, Drop_Pid),
	{ok, Drop_Pid, Ref}.

drop_loop(Pid, Drop_interval) ->
	timer:sleep(Drop_interval * 1000),
	Pid ! {self(), drop},
	drop_loop(Pid, Drop_interval).


cache_loop(Drop_interval) -> 
	receive
		{_, drop} ->
			?MODULE_CACHE:delete_obsolete();
		{Pid, insert, Key, Value, Timeout} ->
			Pid ! {self(), insert_answer, ?MODULE_CACHE:insert(Key, Value, Timeout)};
		{Pid, lookup, Key} ->
			Pid ! {self(), lookup_answer, ?MODULE_CACHE:lookup(Key)};
		{Pid, lookup_by_date, DateFrom, DateTo} ->
			Pid ! {self(), lookup_by_date_answer, ?MODULE_CACHE:lookup_by_date(DateFrom, DateTo)};	
		{Pid, Msg} ->
			Pid ! {self(), wrong_message, Msg};
		{'DOWN', _Ref, process, _Pid, _Reason} ->
			start_drop(self(),Drop_interval);
		Msg -> 
			erlang:display({wrong_message, Msg})
	end,
	cache_loop(Drop_interval).


		







