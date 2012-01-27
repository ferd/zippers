# Zippers #

## Build Instructions ##

simply call `erl -make Emakefile` or `agner build zippers` if you have agner installed.


## What are zippers? ##

In Erlang (and in many other programming languages which include a functional subset), operations on purely functional data structures are frequently limited to `O(log n)` time complexity: there is no such thing as a true array with constant time access. No destructive updates. Nothing like pointers either.

Because of this, you end up with a bunch of modules like `array`, `dict`, `gb_trees` and whatnot, which are all built either on lists or trees with varying branching factors. None of them does better than `O(log n)`, and list-based solutions don't do better than `O(n)` (although they can average `n/2`.)

There is, however, a class of data-structures that allow for amortized constant time for some operations: finger trees and zippers are examples of this. We only care about zippers in this case.

*Zipper* is a generic term used to qualify a data structure that you can arbitrarily modify and navigate through. This is generally done by deconstructing the data structure as you go through it so that you have, on one hand, the data structure yet unexplored, and on the other hand, the exploded set of previously seen items. The idea is that as you move forward, you keep deconstructing the structure and adding the past items to the set, and that as you move backwards, you reconstruct the set of previously seen data structures, wrapping them around the current one.

This lets you update and insert content inside the data structure in amortized constant time while letting you freely move through it as you would in a doubly linked lists or in bidirectional graphs. Note that zippers are not search data structures. 

For more explanations, please consult the following resources:

- [Yet Another Article on Zippers](http://ferd.ca/yet-another-article-on-zippers.html) (explains the implementation of this library)
- [Zippers with trees on Haskell wiki](http://www.haskell.org/haskellwiki/Zipper)
- [You could have invented zippers](http://blog.ezyang.com/2010/04/you-could-have-invented-zippers/)
- [Theseus and the Zipper](http://en.wikibooks.org/wiki/Haskell/Zippers)


## Data Structures in the Library  and Basic usage ##

This library contains 3 different Zipper data structures: lists, binary trees and forests.

### Zipper Lists ###

Zipper lists allows to 'browse' a list in two directions, much like an imperative doubly-linked list, or a list with iterators that can go in both directions. A zipper list can be used to represent items such as a browser's history. Access to the next and previous elements are both amortized constant time, and so are the costs of insertion and deletion in the current position. Looking up from the beginning or the end is in linear time.

Example:

    1> Z = zipper_lists:from_list([1,2,3,4,5]).
    {[],[1,2,3,4,5]}
    2> zipper_lists:next(zipper_lists:next(zipper_lists:next(Z))).
    {[3,2,1],[4,5]}
    3> Z2 = zipper_lists:prev(zipper_lists:next(zipper_lists:insert(a, zipper_lists:next(zipper_lists:next(Z))))).
    {[2,1],[a,3,4,5]}
    4> zipper_lists:current(Z2).
    {ok,a}


### Zipper Bintrees ###

Zipper binary trees are binary trees which can be browsed up and down, and left and right, much like binary trees where child nodes link up to their parents. An example use case could be to represent a decision tree that can be rolled back. Inserting a child either on the left or the right is of amortized constant time, and so is deleting them. Note that zippers are not search data structures.

Example:

    1> Root = zipper_bintrees:root(a).
    {[],{fork,a,undefined,undefined}}
    2> T = zipper_bintrees:set_right_branch(c, zipper_bintrees:set_left_branch(b, Root)).
    {[],
     {fork,a,
           {fork,b,undefined,undefined},
           {fork,c,undefined,undefined}}}
    3> zipper_bintrees:current(zipper_bintrees:left(zipper_bintrees:top(zipper_bintrees:right(T)))).
    {ok,b}

### Zipper Forests ###

Zipper forests are the zipper equivalent of a tree where each node is a list of subtrees. The term is chosen because a forest, in graph theory, means an undirected acyclic graphs, which is precisely the shape such a tree can represent. The advantage of the zipper forest is that it allows the graph/tree to be navigated in an iterative manner. Potential uses of such a forest can be to represent a minimum spanning tree, a DOM document, an undo tree, etc. Adding, replacing and deleting items is done in amortized constant time.

Example with the following tree:

        [a,  b,  c]
       /     |     \
    [d,e]  [f,g]  [h,i,j]
               \
               [k]

Using a pre-built tree:

    1> PreBuiltTree = {[],
    1>                 {[], [{a, {[], [{d, {[], []}},
    1>                                 {e, {[], []}}]}},
    1>                       {b, {[], [{f, {[], []}},
    1>                                 {g, {[], [{k, {[], []}}]}}]}},
    1>                       {c, {[], [{h, {[], []}},
    1>                                 {i, {[], []}},
    1>                                 {j, {[], []}}]}}]}}.
    {[],
     {[],
      [{a,{[],[{d,{[],[]}},{e,{[],[]}}]}},
       {b,{[],[{f,{[],[]}},{g,{[],[{k,{[],[]}}]}}]}},
       {c,{[],[{h,{[],[]}},{i,{[],[]}},{j,{[],[]}}]}}]}}
    2> Low = zipper_forests:children(zipper_forests:next(zipper_forests:children(zipper_forests:next(PreBuiltTree)))).
    {[{[{f,{[],[]}}],[g]},
      {[{a,{[],[{d,{[],[]}},{e,{[],[]}}]}}],
       [b,{c,{[],[{h,{[],[]}},{i,{[],[]}},{j,{[],[]}}]}}]}],
     {[],[{k,{[],[]}}]}}
    3> zipper_forests:value(Low).
    {ok,k}
    4> zipper_forests:value(zipper_forests:rparent(zipper_forests:parent(Low))).
    {ok,b}

Nevermind the hard to read internal representation. The advantage here is to be able to navigate the tree and modify it as we wish.


## Authors ##

- [Fred Hebert](http://ferd.ca)
