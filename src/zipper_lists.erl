%% @author Fred Hebert <mononcqc@gmail.com> [http://ferd.ca/]
%% @doc Zipper lists allows to 'browse' a list in two directions, much like
%% an imperative doubly-linked list, or a list with iterators that can go
%% in both directions.
%% A zipper list can be used to represent items such as a browser's history.
%% Access to the next and previous elements are both
%% amortized constant time, and so are the costs of insertion and deletion
%% in the current position.
%% Looking up from the beginning or the end is in linear time.
%% Note that zippers are not search data structures.
%% @reference See <a href="http://ferd.ca/yet-another-article-on-zippers.html">
%% the related blog post</a> for more basic details on the concept of zippers
-module(zipper_lists).
-export([new/0, from_list/1, to_list/1,
         prev/1, next/1, current/1,
         replace/2, insert/2, delete/1]).
-export_type([zipper_list/0]).

%% {Previous, [Current | Next]}.
%% @type zipper_list(). The zipper type.
-type zipper_list() :: {list(), list()}.

%% @doc Creates an empty zipper
-spec new() -> zipper_list().
new() -> {[], []}.

%% Most zippers data structures wouldn't really need to convert between
%% types, but lists are kind of ubiquitous in usage, so it might be
%% proper to convert them.
%% @doc Convert from standard list to a zipper list
-spec from_list(list()) -> zipper_list().
from_list(L) when is_list(L) -> {[], L}.

%% @doc Convert from a zipper list to a standard list
-spec to_list(zipper_list()) -> list().
to_list({Pre, Post}) -> lists:reverse(Pre) ++ Post.

%% @doc Accesses the previous element of the zipper list. If there is no
%% previous element, the function returns <code>undefined</code>.
-spec prev(zipper_list()) -> zipper_list() | undefined.
prev({[], _Post}) -> undefined;
prev({[H|T], Post}) -> {T, [H|Post]}.

%% @doc Accesses the next element of the zipper list. If there is no
%% next element, the function returns <code>undefined</code>.
-spec next(zipper_list()) -> zipper_list() | undefined.
next({_Pre, []}) -> undefined;
next({Pre, [H|T]}) -> {[H|Pre], T}.

%% @doc Allows to read the value of the current list element. If the zipper
%% is empty, the atom <code>undefined</code> is returned.
-spec current(zipper_list()) -> {ok, term()} | undefined.
current({_, []}) -> undefined;
current({_, [Current|_]}) -> {ok,Current}.

%% @doc Changes the value of the current zipper list item.
-spec replace(term(), zipper_list()) -> zipper_list().
replace(Val, {Pre, [_|Post]}) -> {Pre, [Val|Post]}.

%% @doc Inserts a new element in the zipper. The element is added
%% at the current zipper position. Successively adding the numbers
%% <code>1..4</code> in a zipper without changing positions will result
%% in a zipper equivalent to <code>[4,3,2,1]</code>.
%% To avoid this and obtain a list that is in order, use the function
%% {@link next/1. <code>next/1</code>} after each insertion.
-spec insert(term(), zipper_list()) -> zipper_list().
insert(Val, {Pre, Post}) -> {Pre, [Val|Post]}.

%% @doc Deletes the element at the current position in the zipper.
-spec delete(zipper_list()) -> zipper_list().
delete({Pre, [_|Post]}) -> {Pre, Post}.
