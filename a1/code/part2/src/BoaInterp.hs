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

-- Built-in functions
-- built-in: range
range :: Int -> Int -> Int -> [Value]
range n1 n2 n3 =
  if (n3 > 0 && n1 < n2) || (n3 < 0 && n1 > n2) then
    (IntVal n1) : (range (n1+n3) n2 n3)
  else []

-- built-in: print
-- The main function is 'printVal' which is named so because
-- the 'print' name is defined in Prelude.
printListElements :: [Value] -> String
printListElements [] = ""
printListElements [x] = (printVal x)
printListElements (x:xs) = (printVal x) ++ ", " ++ (printListElements xs)

-- aka. print
printVal :: Value -> String
printVal NoneVal = "None"
printVal TrueVal = "True"
printVal FalseVal = "False"
printVal (IntVal a) = show a
printVal (StringVal s) = s
printVal (ListVal l) = "[" ++ printListElements l ++ "]"

-- apply built-in functions
apply :: FName -> [Value] -> Comp Value
apply "range" [(IntVal n2)] =
  Comp (\_ -> (Right (ListVal (range 0 n2 1)), []))
apply "range" [(IntVal n1), (IntVal n2)] =
  Comp (\_ -> (Right (ListVal (range n1 n2 1)), []))
apply "range" [(IntVal n1), (IntVal n2), (IntVal n3)] =
  Comp (\_ -> (Right (ListVal (range n1 n2 n3)), []))
apply "range" _ =
  Comp (\_ -> (Left (EBadArg "invalid arguments for range."), []))
apply "print" l = case l of
  (x:xs) -> do
    output (printVal x)
    apply "print" xs
  [] ->
    Comp (\_ -> (Right NoneVal, []))
apply fn _ = abort (EBadFun fn)

-- helper function that evaluates a list of expressions
evalExpList :: [Exp] -> Comp [Value]
evalExpList (x:xs) = do
  a <- eval x
  rest <- evalExpList xs
  return $ a : rest
evalExpList [] = do return []

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var vn)  = look vn

eval (Oper op e1 e2) =
  do v1 <- eval e1
     v2 <- eval e2
     case operate op v1 v2 of
       Left err -> Comp(\_ -> (Left (EBadArg err), []))
       Right v  -> return v

eval (Not e) = do
  v <- eval e
  case v of
    NoneVal -> return TrueVal
    FalseVal -> return TrueVal
    ListVal l   | length l == 0  -> return TrueVal
    StringVal s | length s == 0  -> return TrueVal
    _ -> return FalseVal

eval (Call f args) = do
  valList <- evalExpList args
  apply f valList

eval (List el) = do
  valList <- evalExpList el
  return (ListVal valList)

eval (Compr e0 []) = eval e0
eval (Compr e0 (q:qs) ) =
  case q of
    QFor _ (List []) ->
      do val <- eval e0
         return val
    QFor vn (List [x]) ->
      do x_val <- eval x
         e0_val <- withBinding vn x_val (eval (Compr e0 qs))
         return (ListVal [e0_val])
    QFor vn (List (x:xs)) ->
      do x_val <- eval x
         e0_val <- withBinding vn x_val (eval (Compr e0 qs) )
         rest_val <- eval (Compr e0 ((QFor vn (List xs)) : qs))
         case e0_val of
           NoneVal -> case rest_val of
                        ListVal ls -> return (ListVal (ls))
                        _ -> return (ListVal [])
           _ -> case rest_val of
                  ListVal ls -> return (ListVal (e0_val : ls))
                  _ -> return (ListVal [e0_val])
    QFor _ _ -> abort (EBadArg "Not a list.")
    QIf e ->
      do tasty <- eval e
         if truthy tasty then
           do et <- eval e0
              return et
         else
            do return (NoneVal)



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
execute p = let (a, output) = runComp (exec p) []
            in case a of
              Left err -> (output, Just err)
              Right _ -> (output, Nothing)
