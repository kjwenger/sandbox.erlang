-module(ekvs).
-behaviour(application).
-export([start/0, stop/0]).
-export([put/2, get/1, delete/1]).

start() ->
    kvs_server:start_link(),
    ok.

stop() ->
    ok.

put(Key, Value) ->
    kvs_server:put(Key, Value).

get(Key) ->
    kvs_server:get(Key).

delete(Key) ->
    kvs_server:delete(Key).
