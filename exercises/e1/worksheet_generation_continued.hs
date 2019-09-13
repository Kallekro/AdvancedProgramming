
type Problem = (Equality, Maybe (Int, Int))

data Equality = Equal QuadExpr QuadExpr
              deriving (Show, Read, Eq)

data QuadExpr = Const Int
              | X
              | Plus QuadExpr QuadExpr
              | Mult QuadExpr QuadExpr
              | Squared QuadExpr
          deriving (Show, Read, Eq)

-- 5 + x + (3^2)*x^2
-- Plus (Plus (Const 5) (X)) (Mult (Squared (Const 3)) (Squared X))

evalSubQ :: QuadExpr -> Int
evalSubQ (Const a) = a
evalSubQ (Plus e1 e2) = evalSubQ e1 + evalSubQ e2
evalSubQ (Mult e1 e2) = evalSubQ e1 * evalSubQ e2
evalSubQ (Squared e1) = evalSubQ e1 ^ 2
evalSubQ qe = error ("evalSubQ did not understand: " ++ show qe)

evalQ :: QuadExpr -> (Int, Int, Int)
evalQ (Plus (Plus q1 q2) (q3)) = undefined
evalQ _ = error "Polynomial must be on form: x^2 + x + c"

evalEquality :: Equality -> Maybe (Int, Int)
evalEquality (Equal qexp1 (Const 0)) = undefined
