-module(ekvs_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

get_unknown_test() ->
    ekvs:start(),
    ?assertEqual(not_found, ekvs:get(unknown)),
    ekvs:stop().

get_known_test() ->
    ekvs:start(),
    ?assertEqual(true, ekvs:put(key, value)),
    ?assertEqual(value, ekvs:get(key)),
    ekvs:delete(key),
    ekvs:stop().

put_again_test() ->
    ekvs:start(),
    ?assertEqual(true, ekvs:put(key, eulav)),
    ?assertEqual(true, ekvs:put(key, value)),
    ?assertEqual(value, ekvs:get(key)),
    ekvs:delete(key),
    ekvs:stop().

delete_unknown_test() ->
    ekvs:start(),
    ?assertEqual(true, ekvs:delete(unknown)),
    ekvs:stop().

delete_known_test() ->
    ekvs:start(),
    ?assertEqual(true, ekvs:put(key, value)),
    ?assertEqual(true, ekvs:delete(key)),
    ekvs:stop().
