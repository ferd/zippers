-module(prop_zipper_lists).
-include_lib("proper/include/proper.hrl").


%% Converting back and forth with a list should yield
%% the same result.
prop_convert() ->
    ?FORALL(L, list(),
        equals(L, zipper_lists:to_list(zipper_lists:from_list(L)))).

prop_current_failure() ->
    ?FORALL(L, zipper_lists:new(),
        equals(undefined, zipper_lists:current(L))).

prop_prev_failure() ->
    ?FORALL(L, zipper_lists:new(),
        equals(undefined, zipper_lists:prev(L))).

prop_next_failure() ->
    ?FORALL(L, zipper_lists:new(),
        equals(undefined, zipper_lists:next(L))).

%% when writing elements in a zipper in order,
%% and then converting to a list, we should get two
%% similar lists
prop_insert_to_list() ->
    ?FORALL({L,Z}, list_and_zip(any()),
       equals(L, zipper_lists:to_list(eval(Z)))).

%% Moving to the next element N times should give
%% the Nth+1 element of the list.
prop_next() ->
    ?FORALL({L,SymbZip}, list_and_zip(any()),
     ?IMPLIES(length(L) >= 2,
      begin
        Z = eval(SymbZip),
        Nth = round(length(L) / 2),
        {ok,C} = zipper_lists:current(do_times(Nth,fun zipper_lists:next/1,Z)),
        equals(lists:nth(Nth+1,L), C)
      end)).

%% Moving to the Nth element of a list and replacing it
%% should not change the length of list but still change
%% the element.
prop_replace() ->
    ?FORALL({L,SymbZip}, list_and_zip(int()),
     ?IMPLIES(length(L) >= 2,
      begin
        Z = eval(SymbZip),
        N = round(length(L) / 2),
        {Prev,[_|Next]} = lists:split(N, L),
        RepList = Prev++[a|Next],
        C = zipper_lists:replace(a, do_times(N, fun zipper_lists:next/1, Z)),
        equals(RepList, zipper_lists:to_list(C))
      end)).

%% Deleting the Nth element of a list
%% should remove it from there.
prop_delete() ->
    ?FORALL({L,SymbZip}, list_and_zip(int()),
     ?IMPLIES(length(L) >= 2,
      begin
        Z = eval(SymbZip),
        N = round(length(L) / 2),
        {Prev,[_|Next]} = lists:split(N, L),
        RepList = Prev++Next,
        C = zipper_lists:delete(do_times(N, fun zipper_lists:next/1, Z)),
        equals(RepList, zipper_lists:to_list(C))
      end)).

%% Starting from the end of the list and going back half the
%% elements should give the same middle element as if you went
%% halfway forward.
prop_prev() ->
    ?FORALL({L,SymbZip}, list_and_zip(any()),
     ?IMPLIES(length(L) >= 2,
      begin
        Right = Length = length(L),
        Pos = round(Length/2),
        Left = if Length rem 2 =:= 0 -> round(Length/2)+1;
                  Length rem 2 =/= 0 -> round((Length+1)/2)
               end,
        %% get to the end
        Z1 = do_times(Right, fun zipper_lists:next/1, eval(SymbZip)),
        %% then rewind to the position. +1 because the 'cursor' can
        %% go past the last element in a zipper.
        Z2 = do_times(Left, fun zipper_lists:prev/1, Z1),
        equals(lists:nth(Pos, L), element(2,zipper_lists:current(Z2)))
      end)).


%% Generators
%% Gives a list and a zipper with the same ones terms
list_and_zip(Type) ->
    ?LAZY(weighted_union([
        {1,{[], {call, zipper_lists, new, []}}},
        {5, ?LET({A,{L,Z}},
                {Type, list_and_zip(Type)},
                {[A|L], {call, zipper_lists, insert, [A,Z]}})}
    ])).

%% Private funs
do_times(0, _, Res) -> Res;
do_times(N, F, Args) ->
    do_times(N-1, F, F(Args)).
