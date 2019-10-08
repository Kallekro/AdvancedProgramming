-module(warmup).
-export([move/2, insert/3, lookup/2]).

% direction is one of the atoms north, south, east or west

move(north, {X, Y}) -> {X, Y+1};
move(south, {X, Y}) -> {X, Y-1};
move(east,  {X, Y}) -> {X+1, Y};
move(west,  {X, Y}) -> {X-1, Y}.

% A binary search tree is either
%      the atom leaf
%   or a tuple {node, Key, Value, Left, Right}
%      where Left and Right are binary search trees, and all the keys
%      in Left are smaller than Key and all the keys in Right are
%      larger than Key


% insert inserts a key and a value into a binary search tree. If the
% key is already there the value is updated.

insert(Key, Val, leaf) -> {node, {Key, Val}, leaf, leaf};
insert(Key, Val, {node, {K, V}, Left, Right}) ->
    if Key < K  -> {node, {K, V}, insert(Key, Val, Left), Right};
       Key > K  -> {node, {K, V}, Left, insert(Key, Val, Right)};
       Key == K -> {node, {Key, Val}, Left, Right}
    end.


% lookup find the value associated to a key in a binary search
% tree. Returns {ok, Value} if the key is in the tree; or none if the
% key is not in the tree.

lookup(_, leaf) -> none;
lookup(Key, {node, {K, V}, Left, Right}) ->
    if Key == K -> {ok, V};
       Key < K  -> lookup(Key, Left);
       Key > K  -> lookup(Key, Right)
    end.