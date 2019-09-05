module Warmup where

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move South (x, y) = (x, y-1)
move East  (x, y) = (x+1, y)

moves :: [Direction] -> Pos -> Pos
moves [] (x,y) = (x,y)
moves (l:ls) p = moves ls (move l p)

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

addNat :: Nat -> Nat -> Nat
addNat Zero a = a
addNat (Succ a) b = addNat a (Succ b)

mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat (Succ a) b = addNat (mulNat a b) b

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ a) = 1 + nat2int a

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ (int2nat (x-1))
  
data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert n Leaf = Node n Leaf Leaf
insert n (Node m t1 t2) 
    | n < m = Node m (insert n t1) t2 
    | n > m = Node m t1 (insert n t2)
insert _ t = t
