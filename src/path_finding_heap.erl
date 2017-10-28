-module(path_finding_heap).

-export([heap/0, push/3, pop/1, update/3]).

heap() ->
	[].

push(Key, Value, Heap) ->
	lists:keysort(1, [{Key, Value} | Heap]).

pop([H | T]) ->
	{H, T}.

update(Key, Value, Heap) ->
	lists:keystore(Key, 1, Heap, {Key, Value}).