-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\_ -> (Right a, []))
  (Comp c) >>= f = Comp (\env ->
    let (a, l) = c env
      in case a of
        Right x -> let (b, l2) = runComp (f x) env
                   in (b, l++l2)
        Left e ->  (Left e, l))


-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort e = Comp (\_ -> (Left e, [] ))

look :: VName -> Comp Value
look vn = Comp ( \env ->
          case env of
            ((x1, x2):xs)
              | x1==vn -> (Right x2, mempty)
              | x1/=vn -> runComp (look vn) xs
            [] -> runComp (abort (EBadVar vn)) []
            _ -> error "impossible case")

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding vn val c = Comp (\env -> let new_env = (vn, val):env
                                     in runComp c new_env)

output :: String -> Comp ()
output s = Comp (\_ -> (Right (), [s] ) )

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy NoneVal = False
truthy TrueVal = True
truthy FalseVal = False
truthy (IntVal n) = n /= 0
truthy (StringVal s) = s /= ""
truthy (ListVal l) = (length l) /= 0

operate :: Op -> Value -> Value -> Either String Value
--Plus
operate Plus (IntVal v1) (IntVal v2) = Right (IntVal (v1 + v2))
operate Plus (StringVal s1) (StringVal s2) = Right (StringVal (s1 ++ s2))
operate Plus (StringVal s) (IntVal i) = Right (StringVal (s ++ (show i)))
operate Plus (IntVal i) (StringVal s) = Right (StringVal ((show i) ++ s))
operate Plus (ListVal l1) (ListVal l2) = Right (ListVal (l1 ++ l2))
--Minus
operate Minus (IntVal v1) (IntVal v2) = Right (IntVal (v1 - v2))
--Times
operate Times (IntVal v1) (IntVal v2) = Right (IntVal (v1 * v2))
--Div
operate Div (IntVal v1) (IntVal v2) =
  if v2 /= 0 then Right (IntVal (v1 `div` v2))
  else Left "Division by zero."
--Mod
operate Mod (IntVal v1) (IntVal v2) =
  if v2 /= 0 then Right (IntVal (v1 `mod` v2))
  else Left "Modulo by zero."
--Eq
operate Eq v1 v2 =
  if v1 == v2 then Right TrueVal
  else Right FalseVal
--Less
operate Less (IntVal v1) (IntVal v2) =
  if v1 < v2 then Right TrueVal
  else Right FalseVal
--Greater
operate Greater (IntVal v1) (IntVal v2) =
  if v1 > v2 then Right TrueVal
  else Right FalseVal
--In
operate In v (ListVal l)  =
  if v `elem` l then Right TrueVal
  else Right FalseVal
operate op _ _ = Left (show op ++ ": Operand mismatch.")

range :: Int -> Int -> Int -> [Value]
range n1 n2 n3 =
  if n3 > 0 && n1 < n2 then (IntVal n1) : (range (n1+n3) n2 n3)
  else if n3 < 0 && n1 > n2 then (IntVal n1) : (range (n1+n3) n2 n3)
  else []

apply :: FName -> [Value] -> Comp Value
apply "range" [(IntVal n2)] =
  Comp (\env -> (Right (ListVal (range 0 n2 1)), []))
apply fn _= Comp( \env -> (Left (EBadFun fn), [] ))

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval = undefined

exec :: Program -> Comp ()
exec (x:xs) = case x of
  SDef name exp -> do
    a <- eval exp
    withBinding name a (exec xs)
  SExp exp -> do
    eval exp
    exec xs
exec [] = do return ()

execute :: Program -> ([String], Maybe RunError)
execute = undefined
