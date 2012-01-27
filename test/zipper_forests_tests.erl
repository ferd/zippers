-module(zipper_forests_tests).
-import(zipper_forests, [root/1, value/1,
                         replace/2, insert/2, delete/1,
                         prev/1, next/1, parent/1, rparent/1, children/1]).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_TREE, {[], % thread
                    {[], [{a, {[], [{d, {[], []}},
                                    {e, {[], []}}]}},
                          {b, {[], [{f, {[], []}},
                                    {g, {[], [{k, {[], []}}]}}]}},
                          {c, {[], [{h, {[], []}},
                                    {i, {[], []}},
                                    {j, {[], []}}]}}]}}).

root_test_() ->
    [?_assertEqual({[], {[], [{a,{[], []}}]}}, root(a))].

value_test_() ->
    [?_assertEqual(undefined, value(children(root(a)))),
     ?_assertEqual({ok,a}, value(root(a))),
     ?_assertEqual({ok,b}, value(next(?TEST_TREE)))].

replace_test_() ->
    [?_assertEqual({ok,r}, value(replace(r, root(a)))),
     ?_assertEqual({ok,r}, value(replace(r, next(?TEST_TREE)))),
     ?_assertMatch({[], {[], [{a, {_,_}}, {r, {_,_}}, {c, {_,_}}]}},
                   prev(replace(r, prev(next(next(?TEST_TREE))))))].

insert_test_() ->
    [?_assertEqual({[], {[], [{x, {[], []}}, {a, {[], []}}]}},
                   insert(x, root(a))),
     ?_assertMatch({[], {[], [{a,{_,_}}, {r,{_,_}}, {b,{_,_}}, {c,{_,_}}]}},
                   prev(insert(r, next(?TEST_TREE))))].

delete_test_() ->
    [?_assertError(function_clause, delete(delete(root(a)))),
     ?_assertEqual({[], {[], []}}, delete(root(a))),
     ?_assertEqual({[], {[], [{a, {[], []}}]}},
                   delete(insert(x, root(a)))),
     ?_assertMatch({[], {[], [{b, {[], [{f,{[], []}},
                                        {g,{[], []}}]}}]}},
                   parent(prev(parent(delete(children(next(children( % clear k
                    prev(delete(next(delete(?TEST_TREE)))) % clear a and c
                   ))))))))].

next_test() ->
    [?_assertEqual({[], {[{a, {[], []}}], []}}, next(root(a))),
     ?_assertEqual(undefined, next(next(root(a))))].

prev_test_() ->
    [?_assertEqual(undefined, prev(root(a))),
     ?_assertEqual(root(a), prev(next(root(a))))].

children_test_() ->
    [?_assertEqual({[{[], [a]}], {[], []}}, children(root(a))),
     ?_assertEqual(undefined, children(children(root(a)))),
     ?_assertEqual({ok,a}, value(parent(children(root(a))))),
     ?_assertEqual({ok,k}, value(children(next(children(next(?TEST_TREE))))))].

parent_test_() ->
    [?_assertEqual(undefined, parent(root(a))),
     ?_assertEqual(?TEST_TREE, parent(children(?TEST_TREE))),
     %% no rewind test
     ?_assertEqual({ok,e}, value(children(parent(next(children(?TEST_TREE))))))].

rparent_test_() ->
    [?_assertEqual(undefined, parent(root(a))),
     ?_assertEqual(?TEST_TREE, parent(children(?TEST_TREE))),
     %% rewind test
     ?_assertEqual({ok,d}, value(children(rparent(next(children(?TEST_TREE))))))].
