%% @author Fred Hebert <mononcqc@gmail.com> [http://ferd.ca/]
%% @doc Zipper binary trees are binary trees which can be browsed
%% up and down, and left and right, much like binary trees where
%% child nodes link up to their parents.
%% An example use case could be to represent a decision tree that can be
%% rolled back.
%% Inserting a child either on the left or the right is of amortized
%% constant time, and so is deleting them.
%% Note that zippers are not search data structures.
%% @reference See <a href="http://ferd.ca/yet-another-article-on-zippers.html">
%% the related blog post</a> for more basic details on the concept of zippers
-module(zipper_bintrees).
-export([root/1, current/1, replace/2, right/1, left/1, top/1,
         set_left_branch/2, set_right_branch/2, is_leaf/1]).
-export_type([zipper_bintree/0]).

-type node(A) :: undefined
               | {fork, A, Left::node(A), Right::node(A)}.

-type choice(A) :: {left, A, node(A)}
                 | {right, A, node(A)}.

-type thread(A) :: [choice(A)].

%% @type zipper_bintree(). A zipper binary tree.
-type zipper_bintree() :: {thread(any()), node(any())}.

%% @doc Creates a basic binary zipper tree. Should be called first when
%% declaring the data structure
-spec root(term()) -> zipper_bintree().
root(A) -> {[], {fork, A, undefined, undefined}}.

%% @doc if the node has no child (both undefined) it is
%% considered to be a leaf node.
-spec is_leaf(zipper_bintree()) -> boolean().
is_leaf({_Thread, {fork, _, undefined, undefined}}) ->
    true;
is_leaf({_Thread, {fork, _, _, _}}) ->
    false.

%% @doc Fetches the value of the current position
-spec current(zipper_bintree()) -> {ok, Val::term()} | undefined.
current({_Thread, {fork, Val, _Left, _Right}}) -> {ok,Val};
current({_Thread, undefined}) -> undefined.

%% @doc Either replaces or create a new node (if it was <code>undefined</code>)
%% at the current position in the zipper binary tree.
-spec replace(Val::term(), zipper_bintree()) -> zipper_bintree().
replace(Val, {Thread, undefined}) ->
    {Thread, {fork, Val, undefined, undefined}};
replace(Val, {Thread, {fork, _OldVal, L, R}}) ->
    {Thread, {fork, Val, L, R}}.

%% @doc Moves down the tree one level, picking the right child.
%% If there is no right child, the function returns <code>undefined</code>.
-spec right(zipper_bintree()) -> zipper_bintree() | undefined.
right({Thread, {fork, Val, L, R}}) -> {[{right, Val, L}|Thread], R};
right({_Thread, undefined}) -> undefined.

%% @doc Moves down the tree one level, picking the left child.
%% If there is no left child, the function returns <code>undefined</code>.
-spec left(zipper_bintree()) -> zipper_bintree() | undefined.
left({Thread, {fork, Val, L, R}}) -> {[{left, Val, R}|Thread], L};
left({_Thread, undefined}) -> undefined.

%% @doc Moves back up one level. When doing so, it reassembles the
%% Current and Past parts of the trees as a complete node.
%% If there is no parent, the function returns <code>undefined</code>.
-spec top(zipper_bintree()) -> zipper_bintree() | undefined.
top({[{left, Val, R}|Thread], L}) ->  {Thread, {fork, Val, L, R}};
top({[{right, Val, L}|Thread], R}) -> {Thread, {fork, Val, L, R}};
top({[], _Tree}) -> undefined.

%% @doc Shortcut function to add a left child
-spec set_left_branch(Val::term(), zipper_bintree()) -> zipper_bintree().
set_left_branch(A, Zipper) ->
    top(replace(A, left(Zipper))).

%% @doc Shortcut function to add a right child
-spec set_right_branch(Val::term(), zipper_bintree()) -> zipper_bintree().
set_right_branch(A, Zipper) ->
    top(replace(A, right(Zipper))).

