-- This is a skeleton file for you to edit

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showOp :: Exp -> Exp -> String -> String
showOp e1 e2 op = "(" ++ showExp e1 ++ op ++ showExp e2 ++ ")"

showExp :: Exp -> String
showExp (Cst exp)
  | exp >= 0 = show exp
  | otherwise = "(" ++ show exp ++ ")"
showExp (Add exp1 exp2) = showOp exp1 exp2 "+"
showExp (Sub exp1 exp2) = showOp exp1 exp2 "-"
showExp (Mul exp1 exp2) = showOp exp1 exp2 "*"
showExp (Div exp1 exp2) = showOp exp1 exp2 "/"
showExp (Pow exp1 exp2) = showOp exp1 exp2 "^"
showExp _ = error "Expression is not supported by showExp."

evalSimple :: Exp -> Integer
evalSimple (Cst exp) = exp
evalSimple (Add exp1 exp2) = evalSimple exp1 + evalSimple exp2
evalSimple (Sub exp1 exp2) = evalSimple exp1 - evalSimple exp2
evalSimple (Mul exp1 exp2) = evalSimple exp1 * evalSimple exp2
evalSimple (Div exp1 exp2) =
  case evalSimple exp2 of
    0 -> error "Division by zero."
    n -> evalSimple exp1 `div` n
evalSimple (Pow exp1 exp2) =
  let n1 = evalSimple exp1
    in case evalSimple exp2 of
      -- We must force evaluation of n1 to crash on errors like (2/0)^0
      -- instead of just returning 0 which is what Haskells (^) operator does.
      -- We force the evaluation by testing for n1's self-equality
      n2 | n2 >= 0 && n1 == n1 -> n1 ^ n2
         | otherwise -> error "Exponent must be positive."
evalSimple _ = error "Expression is not supported by evalSimple."

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \_v -> if (v == _v) then Just n else r _v

evalFull :: Exp -> Env -> Integer
evalFull (Cst exp) _ = exp
evalFull (Add exp1 exp2) r = evalFull exp1 r + evalFull exp2 r
evalFull (Sub exp1 exp2) r = evalFull exp1 r - evalFull exp2 r
evalFull (Mul exp1 exp2) r = evalFull exp1 r * evalFull exp2 r
evalFull (Div exp1 exp2) r =
  case evalFull exp2 r of
    0 -> error "Division by zero."
    n -> evalFull exp1 r `div` n
evalFull (Pow exp1 exp2) r =
  let n1 = evalFull exp1 r
    in case evalFull exp2 r of
      -- As evalSimple we force the evaluation by testing for n1's self-equality
      n2 | n2 >= 0 && n1 == n1 -> n1 ^ n2
         | otherwise -> error "Exponent must be positive."
evalFull (If e1 e2 e3) r =
  case evalFull e1 r of
    0 -> evalFull e3 r
    _ -> evalFull e2 r
evalFull (Var v) r =
  case r v of
    Just n -> n
    Nothing -> error ("Undefined variable " ++ v)
evalFull (Let v e1 e2) r = evalFull e2 (extendEnv v (evalFull e1 r) r)
evalFull (Sum v e1 e2 e3) r =
  case (evalFull e1 r, evalFull e2 r) of
    (n1, n2) | n1 <= n2 ->
      (evalFull e3 (extendEnv v n1 r)) +
      (evalFull (Sum v (Add e1 (Cst 1)) e2 e3) r)
    _ -> 0

evalErrHelper1 :: Exp -> Env -> Either ArithError Integer
evalErrHelper1 exp r = case evalErr exp r of
  Left err -> Left err
  Right n -> Right n

evalErrHelper2 :: Exp -> Exp -> Env -> Either ArithError (Integer, Integer)
evalErrHelper2 exp1 exp2 r =
  case evalErr exp1 r of
    Left err1 -> Left err1
    Right n1 -> case evalErr exp2 r of
      Left err2 -> Left err2
      Right n2 -> Right (n1, n2)

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst exp) _ = Right exp
evalErr (Add exp1 exp2) r =
  case evalErrHelper2 exp1 exp2 r of
    Left err -> Left err
    Right (n1, n2) -> Right (n1 + n2)
evalErr (Sub exp1 exp2) r =
  case evalErrHelper2 exp1 exp2 r of
    Left err -> Left err
    Right (n1, n2) -> Right (n1 - n2)
evalErr (Mul exp1 exp2) r =
  case evalErrHelper2 exp1 exp2 r of
    Left err -> Left err
    Right (n1, n2) -> Right (n1 * n2)
evalErr (Div exp1 exp2) r =
  case evalErrHelper2 exp1 exp2 r of
    Left err -> Left err
    Right (n1, n2)
      | n2 == 0 -> Left EDivZero
      | otherwise -> Right (n1 `div` n2)
evalErr (Pow exp1 exp2) r =
  case evalErrHelper2 exp1 exp2 r of
    Left err -> Left err
    Right (n1, n2)
      | n2 < 0 -> Left ENegPower
      | otherwise -> Right (n1 ^ n2)
evalErr (If e1 e2 e3) r =
  case evalErr e1 r of
    Left err -> Left err
    Right b | b == 0 -> evalErrHelper1 e3 r
            | otherwise -> evalErrHelper1 e2 r
evalErr (Var v) r =
  case r v of
    Just n -> Right n
    Nothing -> Left (EBadVar v)
evalErr (Let v e1 e2) r =
  case evalErr e1 r of
    Left err1 ->
      case evalErr e2 r of
        Left (EBadVar vE) | vE == v -> Left err1
        Left err2 -> Left err2
        Right n2 -> Right n2
    Right n1 -> evalErrHelper1 e2 (extendEnv v n1 r)
evalErr (Sum v e1 e2 e3) r =
  case evalErrHelper2 e1 e2 r of
    Left err1 -> Left err1
    Right (n1, n2)
      | n1 <= n2 ->
        case evalErr e3 (extendEnv v n1 r) of
          Left err2 -> Left err2
          Right n3 ->
            case evalErr (Sum v (Add e1 (Cst 1)) e2 e3) r of
              Left err3 -> Left err3
              Right n4 -> Right (n3 + n4)
      | otherwise -> Right 0



-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
