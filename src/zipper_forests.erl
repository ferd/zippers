%% @author Fred Hebert <mononcqc@gmail.com> [http://ferd.ca/]
%% @doc Zipper forests are the zipper equivalent of a tree where each
%% node is a list of subtrees. The term is chosen because a forest, in graph
%% theory, means an undirected acyclic graphs, which is precisely the shape
%% such a tree can represent. The advantage of the zipper forest is that it
%% allows the graph/tree to be navigated in an iterative manner.
%% Potential uses of such a forest can be to represent a minimum spanning tree,
%% a DOM document, an undo tree, etc.
%% Adding, replacing and deleting items is done in amortized constant time.
%% Note that zippers are not search data structures.
%% @reference See <a href="http://ferd.ca/yet-another-article-on-zippers.html">
%% the related blog post</a> for more basic details on the concept of zippers
-module(zipper_forests).
-export([root/1, value/1,
         replace/2, insert/2, delete/1,
         prev/1, next/1, children/1, parent/1, rparent/1]).

-export_type([zipper_forest/0]).

-type zlist(A) :: {Prev::list(A), Next::list(A)}.
-type znode()  :: zlist({term(), zlist(_)}). % znode is a zlist of nodes
-type thread() :: [znode()].
%% @type zipper_forest(). A zipper forest data structure.
-type zipper_forest() :: {thread(), znode()}.

%% @doc creates an empty zipper forest with <var>Val</var> as the first
%% element in it.
-spec root(Val::term()) -> zipper_forest().
root(Val) -> {[], {[], [{Val, {[], []}}]}}.

%% @doc Extracts the node's value from the current tree position.
%% If no item exists at the current position, the atom <code>undefined</code>
%% is returned.
-spec value(zipper_forest()) -> {ok, term()} | undefined.
value({_Thread, {_Prev, []}}) -> undefined;
value({_Thread, {_Prev, [{Val, _Children} | _Next]}}) -> {ok, Val}.

%% @doc Replaces the value from at the current tree position, without touching
%% the children nodes.
-spec replace(term(), zipper_forest()) -> zipper_forest().
replace(Val, {T, {L, [{_Val, Children}|R]}}) ->
    {T, {L, [{Val,Children}|R]}}.

%% @doc Add a new node at the current position with the value Val.
-spec insert(term(), zipper_forest()) -> zipper_forest().
insert(Val, {Thread, {L, R}}) ->
    {Thread, {L, [{Val, {[], []}} | R]}}.

%% @doc Deletes the node at the current position and its children.
%% The next one becomes the current position.
-spec delete(zipper_forest()) -> zipper_forest().
delete({Thread, {L, [_|R]}}) ->
    {Thread, {L, R}}.

%% @doc Moves to the previous node of the current level. If no
%% such node exists, the atom <code>undefined</code> is returned.
-spec prev(zipper_forest()) -> zipper_forest() | undefined.
prev({_Thread, {[], _Next}}) ->
    undefined;
prev({Thread, {[H|T], R}}) ->
    {Thread, {T, [H|R]}}.

%% @doc Moves to the next node of the current level. If no
%% such node exists, the atom <code>undefined</code> is returned.
-spec next(zipper_forest()) -> zipper_forest() | undefined.
next({_Thread, {_Prev, []}}) ->
    undefined;
next({Thread, {L, [H|T]}}) ->
    {Thread, {[H|L], T}}.

%% @doc Goes down one level to the children of the current node.
%% If the current node is undefined, the atom <code>undefined</code>
%% is returned.
%% @end
%% Note that in order for this to work, the {Val, Children} tuple
%% needs to be broken in two: the value goes in the Thread's zlist
%% while the Children become the current level.
-spec children(zipper_forest()) -> zipper_forest() | undefined.
children({_Thread, {_L,[]}}) ->
    undefined;
children({Thread, {L, [{Val, Children}|R]}}) ->
    {[{L,[Val|R]}|Thread], Children}.

%% @doc Moves up to the direct parent level. Doesn't rewind the current
%% level's child list. This means that if you have a tree, go to the
%% children, browse to the next element 2-3 times, then go back up and
%% down to the children again, you'll be at the same position you were
%% before. If no parent exists, the atom <code>undefined</code> is returned.
%% If you prefer the children to be <em>rewinded</em>, use
%% {@link rparent/1. <code>rparent/1</code>}
-spec parent(zipper_forest()) -> zipper_forest() | undefined.
parent({[], _Children}) ->
    undefined;
parent({[{L, [Val|R]}|Thread], Children}) ->
    {Thread, {L, [{Val, Children}|R]}}.

%% @doc Moves up to the direct parent level, much like
%% {@link parent/1. <code>parent/1</code>}, However,
%% it rewinds the current level's list before doing so. This allows
%% the programmer to access children as if it were the first time,
%% all the time.
%% If no parent exists, the atom <code>undefined</code> is returned.
-spec rparent(zipper_forest()) -> zipper_forest() | undefined.
rparent({[], _Children}) ->
    undefined;
rparent({[{ParentL, [Val|ParentR]}|Thread], {L, R}}) ->
    {Thread, {ParentL, [{Val, {[], lists:reverse(L)++R}}|ParentR]}}.

