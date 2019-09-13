-- move and moves
type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x, y) = (x, y+1)
move West  (x, y) = (x-1, y)
move South (x, y) = (x, y-1)
move East  (x, y) = (x+1, y)

moves :: [Direction] -> Pos -> Pos
moves []     (x,y) = (x,y)
moves [l]    (x,y) = move l (x, y)
moves (l:ls) (x,y) = moves ls (move l (x, y))

-- natural numbers
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

-- binary search tree
data Tree = Leaf | Node Int Tree Tree
    deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert n Leaf = Node n Leaf Leaf
insert n (Node m t1 t2) 
    | n < m  = Node m (insert n t1) t2 
    | n > m = Node m t1 (insert n t2)
insert _ t = t

-- morse code
letters = [ ("A", ".-"), ("B", "-..."), ("C", "-.-."),
            ("D", "-.."), ("E", "."), ("F", "..-."),
            ("G", "--."), ("H", "...."), ("I", ".."),
            ("J", ".---"), ("K", "-.-"), ("L", ".-.."),
            ("M", "--"), ("N", "-."), ("O", "---"),
            ("P", ".--."), ("Q", "--.-"), ("R", ".-."),
            ("S", "..."), ("T", "-"), ("U", "..-"),
            ("V", "...-"), ("W", ".--"), ("X", "-..-"),
            ("Y", "-.--"), ("Z", "--..") ]

matchLetters :: [(String, String)] -> String -> Bool -> String
matchLetters [] _ _ = ""
matchLetters ((alph, morse):_) x alph2morse
    | alph2morse  && x == alph  = morse
    | not alph2morse && x == morse = alph
matchLetters (_:xs) x alph2morse = matchLetters xs x alph2morse

encode :: String -> String
encode "" = ""
encode [x] = matchLetters letters [x] True
encode (x:xs) = (encode [x]) ++ (encode xs)

decode :: String -> [String]
decode "" = [""]

-- type classes
class Sizeable t where
    size::t -> Int

instance Sizeable Int where
    size _ = 1

instance Sizeable [a] where
    size [] = 0
    size (_:xs) = 1 + (size xs)

instance Sizeable String where
    size "" = 0
    size (_:xs) = 1 + size xs

instance Sizeable Tree where
    size Leaf = 0
    size (Node _ t1 t2) = 1 + size t1 + size t2