{-# LANGUAGE ScopedTypeVariables #-}

import qualified System.Random as R
import Control.Monad.State.Lazy

type Problem = (Expr, Int)
data Expr = Const Int
          | Plus Expr Expr
          | Mult Expr Expr
          deriving (Show, Read, Eq)

eval :: Expr -> Int
eval (Const a) = a
eval (Plus a b) = eval a + eval b
eval (Mult a b) = eval a * eval b

genRandom :: (R.StdGen -> (a, R.StdGen)) -> State R.StdGen a
genRandom f = do
  g <- get
  let (rnd, g') = f g
  put g'
  return rnd

generate_const :: State R.StdGen Expr
generate_const = do
  a <- genRandom $ R.randomR (-1000, 1000)
  return $ Const a

generate_other :: Int -> (Expr -> Expr -> Expr) -> State R.StdGen Expr
generate_other max_depth op = do
  expr1 <- generate_expr (max_depth-1)
  expr2 <- generate_expr (max_depth-1)
  return $ op expr1 expr2

generate_expr :: Int -> State R.StdGen Expr
generate_expr max_depth = do
  idx <- genRandom $ R.randomR (1, 3)
  case (idx, max_depth) of
    (n :: Int, m) | (n == 1 || m == 0) -> generate_const
                  | (n == 2) -> generate_other max_depth Plus
                  | (n == 3) -> generate_other max_depth Mult
    _ -> error $ "R.randomR return unexpected value: " ++ show idx

generate_problem :: R.StdGen -> Int -> (Problem, R.StdGen)
generate_problem g max_depth =
  let (expr, g') = runState (generate_expr max_depth) g
  in ((expr, eval expr), g')

generate_sheet :: R.StdGen -> Int -> [Problem]
generate_sheet g n =
    if n > 0 then let (p, g') = generate_problem g 3
                  in p : (generate_sheet g' (n-1))
    else []
