{-
  Code for tic-tac-toe games

  Date: Sep 2, 2016
  Author: Ken Friis Larsen <kflarsen@diku.dk>, Maya Saietz <mayasaietz@gmail.com>
-}

data Player = Cross | Nought
            deriving Show
                     
data Cell = Move Player | Empty
          deriving Show

type Row = [Cell]
type Board = [Row]

emptyBoard :: Board
emptyBoard = take 3 emptyRows
  where emptyRows = repeat emptyRow
        emptyRow = take 3 (repeat Empty)

type Position = (Int, Int)

-- You should probably take a good look at this function until you
-- understand it. Keep in mind that it does not check whether the move
-- is valid.
move :: Position -> Board -> Player -> Board
move (x, y) board player = rowsBefore ++ (changed : rowsAfter)
  where (rowsBefore, toBeChanged : rowsAfter) = splitAt x board
        changed = cellsBefore ++ (newCell : cellsAfter)
        (cellsBefore, old : cellsAfter) = splitAt y toBeChanged
        newCell = Move player
        
-- Type for representing the state of a game. That is, which should
-- take the next move, and what does the board look like.
type GameState = (Player, Board)

startState :: GameState
startState = (Cross, emptyBoard)

-- Hint: You already have the move function, defined above, to do most
-- of the legwork.
makeMove :: Position -> GameState -> GameState
makeMove pos (Cross, board) = (Nought, move pos board Cross)
makeMove pos (Nought, board) = (Cross, move pos board Nought)

validMove :: Position -> GameState -> Bool
validMove _ _ = undefined

allMoves :: [Position]
allMoves = [ (x, y) | x <- [0 .. 2], y <- [0 .. 2] ]

allValidMoves :: GameState -> [Position]
allValidMoves _ = undefined

-- The type for representing game trees.  Each node have a game state
-- and a list of *valid* moves leading to (sub-) game trees
data GameTree = Node GameState [(Position, GameTree)]

-- Grow the game tree starting in a given state
makeTree :: GameState -> GameTree
makeTree _ = undefined

-- Return all game states in a game tree, duplicates are allowed
allNodes :: GameTree -> [GameState]
allNodes (Node gs subs) = gs : concatMap (allNodes . snd) subs

