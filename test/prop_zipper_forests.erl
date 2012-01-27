-module(prop_zipper_forests).
-include_lib("proper/include/proper.hrl").
-compile(export_all).


%% When a given node has no value, it should return the
%% atom undefined.
prop_fail_value() ->
    ?FORALL(R, root(any()),
        equals(undefined,
               zipper_forests:value(zipper_forests:next(R)))).

%% Going to the next node too many times should return the atom
%% undefined to avoid useless try...catch to handle errors
%% in browsing.
prop_fail_next() ->
    ?FORALL(R, root(any()),
        equals(undefined,
               zipper_forests:next(zipper_forests:next(R)))).

%% Going to the prev node to many times should return the atom
%% undefined to avoid useless try...catch to handle errors
%% in browsing. This one fails on the first call to 'prev'
%% because the cursor is already at the firts possible point.
prop_fail_prev() ->
    ?FORALL(R, root(any()),
        equals(undefined,
               zipper_forests:prev(R))).

%% Two parent functions exist: parent and rparent.
%% They differ on behaviour on a child level where the first
%% one just goes back to the parent and the second one
%% 'rewinds' the cursor of the current level. However,
%% they should both return 'undefined' when already at
%% the topmost level of the tree.
prop_fail_parents() ->
    ?FORALL(R, root(any()),
        undefined =:= zipper_forests:parent(R)
        andalso
        undefined =:= zipper_forests:rparent(R)).

%% It should be possible to move to an empty child
%% in order to start populating it, but it should however
%% be impossible to go to the child of that child.
%% When that happens, the atom 'undefined' has to be returned
prop_fail_children() ->
    ?FORALL(R, root(any()),
        equals(undefined,
               zipper_forests:children(zipper_forests:children(R)))).

%% To test for the zipper properties, we:
%% - generate a random tree
%% - generate a random path
%% - explore the tree to make sure the path works
%%   or cut it when it can't go deeper
%% - make sure that going to the bottom of the tree
%%   and then back up should give the same tree as
%%   the original one.
prop_navigate() ->
    ?FORALL({Path, T}, {path(), forest(any())},
     begin
        Tree = eval(T),
        CutPath = find_cutoff(Path, Tree),
        DeepTree = follow_path(CutPath, Tree),
        TopPath = antipath(CutPath),
        TopTree = follow_path(TopPath, DeepTree),
        equals(Tree, TopTree)
     end).

%% To test for the zipper properties, we could:
%% - make sure that going halfway through the path
%%   is the same as going to the bottom of it and
%%   up and down a few times, by fetching the same value.
%% - generate a random tree
%% - generate a random path
%% - explore the tree to make sure the path works
%%   or cut it when it can't go deeper
prop_halfpath_value() ->
    ?FORALL({Path, T}, {path(), forest(any())},
     begin
        Tree = eval(T),
        CutPath = find_cutoff(Path, Tree),
        {HalfPath, Rest} = lists:split(round(length(CutPath)/2), CutPath),
        TopPath = antipath(Rest),
        DeepTree = follow_path(CutPath, Tree),
        HalfTree = follow_path(HalfPath, Tree),
        TopTree = follow_path(TopPath, DeepTree),
        equals(zipper_forests:value(HalfTree),
               zipper_forests:value(TopTree))
     end).

%% rparent is like parent, except it rewinds its child. After going
%% to the children of a tree, going a bit to the next
%% and then back up with rparent, if we go down the child again, it
%% should be impossible to then go to the prev (because we're at
%% the beginning of the node).
prop_rparent() ->
    ?FORALL(T, forest(int()),
     begin
        Tree = eval(T),
        Path = [children|lists:duplicate(10, next)],
        CutPath = find_cutoff(Path, Tree)++[rparent],
        NewTree = follow_path(CutPath, Tree),
        equals(undefined,
               zipper_forests:prev(zipper_forests:children(NewTree)))
     end).


%% When replacing an element, we should neither change its children
%% or its relatives. This test just replaces the value of a child
%% node and makes sure the node's level remains the same length and
%% the direct child is still there.
prop_replace() ->
    ?FORALL(T, forest(int()),
     begin
        Tree = eval(T),
        %% it is likely that we have less than 1000 siblings!
        Path = lists:duplicate(1000, next),
        LevelSize = length(find_cutoff(Path, Tree)),
        NewTree = zipper_forests:replace(a,Tree),
        Children1 = zipper_forests:children(Tree),
        Children2 = zipper_forests:children(NewTree),
        ?IMPLIES(
            Children1 =/= undefined,
            {ok,a} =:= zipper_forests:value(NewTree) andalso
            zipper_forests:value(Children1) =:= zipper_forests:value(Children2)
            andalso LevelSize =:= length(find_cutoff(Path, NewTree)))
     end).

%% When deleting an element, it should be removed from the list.
%% One way to see that this works is that deleting the child should
%% reduce the total number of movements to the next of it on a
%% nodes' level by one. This shall mean that the item's children
%% are also gone. Also the current value should not be the same.
%% To check that the value is different, we generate an integer
%% forest and then replace it with an atom to make sure it is unique.
prop_delete() ->
    ?FORALL(T, forest(int()),
     begin
        Tree = eval(T),
        Path = lists:duplicate(1000, next),
        LevelSize = length(find_cutoff(Path, Tree)),
        NewTree = zipper_forests:replace(a,Tree),
        DelTree = zipper_forests:delete(NewTree),
        zipper_forests:value(DelTree) =/= a andalso
        length(find_cutoff(Path, DelTree)) =/= LevelSize
     end).

%%% PRIVATE
%% Because there us an asymetry between going to the prev and next
%% node (you can go next on a root, but not prev), we need to split the
%% path on a per-level basis, where each levl is its own zipper.
%% By doing this, we can calculate the sum of movements on either
%% direction in time and reverse it.
antipath(L) ->
    NodePaths = lists:reverse(node_break(L,[[]])),
    lists:reverse(flatten([find_paths(Node) || Node <- NodePaths])).

%% Breaks the path into sublists in between nodes.
node_break([], Acc) -> Acc;
node_break([children|Rest],[Node|Nodes]) ->
    node_break(Rest, [[],lists:reverse(Node)|Nodes]);
node_break([PrevOrNext|Rest],[Node|Nodes]) ->
    node_break(Rest, [[PrevOrNext|Node]|Nodes]).

%% Finds the final sum of paths on a single level/node
find_paths(L) ->
    P = find_paths(L,0),
    if P >= 0 -> lists:duplicate(P, prev);
       P < 0  -> lists:duplicate(abs(P), next)
    end.
find_paths([],N) -> N;
find_paths([prev|Path], N) ->
    find_paths(Path, N-1);
find_paths([next|Path], N) ->
    find_paths(Path, N+1).

%% Rejoins all nodes into a single path after being reversed.
flatten([]) -> [];
flatten([H|[]]) -> H;
flatten([H|T]) -> H++[parent]++flatten(T).

%% explore the path until you can no longer go forward.
find_cutoff(Path, Tree) ->
    lists:reverse(find_cutoff(Path, Tree, [])).
find_cutoff([], _Tree, CutPath) -> CutPath;
find_cutoff([Direction|Path], Tree, CutPath) ->
    case zipper_forests:Direction(Tree) of
        undefined -> CutPath;
        NewTree -> find_cutoff(Path, NewTree, [Direction|CutPath])
    end.

%% Explore the forest following the path
follow_path([], Tree) -> Tree;
follow_path([Direction|Path], Tree) ->
    follow_path(Path, zipper_forests:Direction(Tree)).

%%% GENERATORS
%% A random forest of a given Type
forest(Type) ->
    ?SUCHTHAT(
        Tree,
        fill_tree(Type, {call, zipper_forests, root, [Type]}),
        zipper_forests:value(eval(Tree)) =/= undefined).

fill_tree(Type, Tree) ->
    ?LAZY(weighted_union([
    {4, {call, zipper_forests, insert, [Type, Tree]}},
    {3, begin
            Current = {call, zipper_forests, insert, [Type, Tree]},
            Next = {call, zipper_forests, prev, [
                fill_tree(Type, {call, zipper_forests, next, [Current]})
            ]},
            {call, zipper_forests, parent, [
                fill_tree(Type, {call, zipper_forests, children, [Next]})
            ]}
        end}
    ])).


path() ->
    ?SUCHTHAT(P, path1(), P=/=[]).
path1() ->
    ?LAZY(weighted_union([
        {1, []},
        {10, [prev|path1()]},
        {10, [next|path1()]},
        {10, [children|path1()]}
    ])).

%% A forest's root.
root(Type) ->
    zipper_forests:root(Type).
