-module(zipper_bintrees_tests).
-import(zipper_bintrees, [root/1, current/1, replace/2, right/1, left/1, top/1,
                          set_left_branch/2, set_right_branch/2]).
-include_lib("eunit/include/eunit.hrl").
-define(TEST_TREE, {[],
                   {fork,
                    a,
                    {fork,b,undefined,undefined},
                    {fork,c,undefined,{fork,d,undefined,undefined}}}}).

root_test_() ->
    [?_assertEqual({[], {fork, a, undefined, undefined}}, root(a))].

get_test_() ->
    [?_assertEqual({ok,a}, current(root(a))),
     ?_assertEqual({ok,c}, current(right(?TEST_TREE)))].

replace_test_() ->
    [?_assertEqual({ok,c}, current(replace(c, root(a)))),
     ?_assertEqual({ok,f}, current(replace(f, right(?TEST_TREE)))),
     ?_assertEqual({ok,z}, current(replace(z, left(left(?TEST_TREE))))),
     ?_assertEqual(top(top(replace(z, left(left(?TEST_TREE))))),
                   {[],
                    {fork,
                     a,
                     {fork,b,{fork,z,undefined,undefined},undefined},
                     {fork,c,undefined,{fork,d,undefined,undefined}}}})].

right_test_() ->
    [?_assertEqual({[{right, a, {fork, b, undefined, undefined}}],
                    {fork, c, undefined, {fork,d,undefined,undefined}}},
                   right(?TEST_TREE)),
     ?_assertMatch({_, undefined},
                   right(right(right(?TEST_TREE)))),
     ?_assertEqual(undefined,
                   right(right(right(right(?TEST_TREE)))))].

left_test_() ->
    [?_assertEqual({[{left,a,{fork,c,undefined,{fork,d,undefined,undefined}}}],
                    {fork, b, undefined, undefined}},
                   left(?TEST_TREE)),
     ?_assertMatch({_, undefined},
                   left(left(?TEST_TREE))),
     ?_assertEqual(undefined,
                   left(left(left(?TEST_TREE))))].

top_test_() ->
    [?_assertEqual(undefined, top(?TEST_TREE)),
     ?_assertEqual(?TEST_TREE, top(left(?TEST_TREE))), 
     ?_assertEqual(?TEST_TREE, top(right(?TEST_TREE))), 
     ?_assertEqual(?TEST_TREE, top(top(right(left(?TEST_TREE))))),
     ?_assertEqual({[{left,a,{fork,c,undefined,{fork,d,undefined,undefined}}}],
                    {fork, b, undefined, undefined}},
                   left(top(right(?TEST_TREE))))].

