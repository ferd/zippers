-module(prop_zipper_bintrees).
-include_lib("proper/include/proper.hrl").

%% Zipping to the top without any previous root to
%% do so should return undefined.
prop_top_failure() ->
    ?FORALL(Tree, root(any()),
        equals(undefined, zipper_bintrees:top(Tree))).

%% Zipping to the right without any right root branch
%% to do so should return undefined.
prop_right_failure() ->
    ?FORALL(Tree, root(any()),
        equals(undefined,
               zipper_bintrees:right(zipper_bintrees:right(Tree)))).

%% Zipping to the left without any left root branch
%% to do so should return undefined.
prop_left_failure() ->
    ?FORALL(Tree, root(any()),
        equals(undefined,
               zipper_bintrees:left(zipper_bintrees:left(Tree)))).

%% Trying to obtain a value from a branch without any value
%% (ex.: getting the value of an empty right branch) should
%% return the atom 'undefined' rather than {ok, Value}.
prop_get_failure() ->
    ?FORALL(Tree, root(any()),
        equals(undefined,
            zipper_bintrees:current(zipper_bintrees:right(Tree)))).

%% To test for the zipper properties, we:
%% - generate a random tree
%% - generate a random path
%% - explore the tree to make sure the path works
%%   or cut it when it can't go deeper
%% - make sure that going to the bottom of the tree
%%   and then back up should give the same tree as
%%   the original one.
prop_navigate() ->
    ?FORALL({Path, T}, {left_right_path(), tree(any())},
     begin
        Tree = eval(T),
        CutPath = find_cutoff(Path, Tree),
        DeepTree = follow_path(CutPath, Tree),
        TopPath = lists:duplicate(length(CutPath), top),
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
prop_halfpath_get() ->
    ?FORALL({Path, T}, {left_right_path(), tree(any())},
     begin
        Tree = eval(T),
        CutPath = find_cutoff(Path, Tree),
        {HalfPath, Rest} = lists:split(round(length(CutPath)/2), CutPath),
        TopPath = lists:duplicate(length(Rest), top),
        DeepTree = follow_path(CutPath, Tree),
        HalfTree = follow_path(HalfPath, Tree),
        TopTree = follow_path(TopPath, DeepTree),
        equals(zipper_bintrees:current(HalfTree),
               zipper_bintrees:current(TopTree))
     end).


find_cutoff(Path, Tree) ->
    lists:reverse(find_cutoff(Path, Tree, [])).

find_cutoff([], _Tree, CutPath) -> CutPath;
find_cutoff([Direction|Path], Tree, CutPath) ->
    case zipper_bintrees:Direction(Tree) of
        undefined -> CutPath;
        NewTree -> find_cutoff(Path, NewTree, [Direction|CutPath])
    end.

follow_path([], Tree) -> Tree;
follow_path([Direction|Path], Tree) ->
    follow_path(Path, zipper_bintrees:Direction(Tree)).

%%% GENERATORS
%% Generates a random zipper bintree.
%% Avoids all trees that consist only of
%% a root without children.
tree(Type) ->
    ?SUCHTHAT(
        Tree,
        fill_tree(Type,{call, zipper_bintrees, root, [Type]}),
        not zipper_bintrees:is_leaf(eval(Tree))
    ).

fill_tree(Type, Tree) ->
    ?LAZY(weighted_union([
    {4, {call, zipper_bintrees, replace, [Type, Tree]}},
    {3, begin
            Left = {call, zipper_bintrees, set_left_branch, [
                Type, Tree
            ]},
            Full = {call, zipper_bintrees, set_right_branch, [
                Type, Left
            ]},
            FillLeft = {call, zipper_bintrees, top, [
             fill_tree(Type, {call, zipper_bintrees, left, [Full]})
            ]},
            {call, zipper_bintrees, top, [
             fill_tree(Type, {call, zipper_bintrees, right, [FillLeft]})
            ]}
        end}])).

left_right_path() ->
    ?SUCHTHAT(P, left_right_path1(), P =/= []).
left_right_path1() ->
    ?LAZY(weighted_union([
        {1, []},
        {15, [left|left_right_path1()]},
        {15, [right|left_right_path1()]}
    ])).

%% Generates a root only.
root(Type) ->
    zipper_bintrees:root(Type).
