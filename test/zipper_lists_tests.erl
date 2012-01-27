-module(zipper_lists_tests).
-compile(export_all).
-import(zipper_lists, [from_list/1, to_list/1,
                 prev/1, next/1, current/1,
                 replace/2, insert/2, delete/1]).
-include_lib("eunit/include/eunit.hrl").

from_list_test_() -> % Dialyzer will warn about improper lists.
    [?_assertEqual({[], []}, from_list([])),
     ?_assertEqual({[], [a]}, from_list([a])),
     ?_assertEqual({[], [a,b,c]}, from_list([a,b,c]))].

to_list_test_() ->
    [?_assertEqual([], to_list({[], []})),
     ?_assertEqual([a], to_list({[a], []})),
     ?_assertEqual([a], to_list({[], [a]})),
     ?_assertEqual([a,b], to_list({[a], [b]})),
     ?_assertEqual([a,b,c,d], to_list({[b,a], [c,d]}))].

prev_test_() ->
    [?_assertEqual(undefined, prev({[],[1,2,3]})),
     ?_assertEqual({[1], [2,3,4]}, prev({[2,1], [3,4]}))].

next_test_() ->
    [?_assertEqual(undefined, next({[1,2,3],[]})),
     ?_assertEqual({[2,1], [3,4]}, next({[1], [2,3,4]}))].

current_test_() ->
    [?_assertEqual(undefined, current({[], []})),
     ?_assertEqual(undefined, current({[1], []})),
     ?_assertEqual({ok,2}, current({[1], [2,3]}))].

replace_test_() ->
    [?_assertError(function_clause, replace(1, {[a], []})),
     ?_assertEqual({[], [3,2]}, replace(3, {[], [1,2]}))].

insert_test_() ->
   [?_assertEqual({[],[1]}, insert(1, {[], []})),
    ?_assertEqual({[1], [2,3,4]}, insert(2, {[1], [3,4]}))].

delete_test_() ->
    [?_assertError(function_clause, delete({[], []})),
     ?_assertEqual({[1],[3]}, delete({[1], [2,3]}))].
