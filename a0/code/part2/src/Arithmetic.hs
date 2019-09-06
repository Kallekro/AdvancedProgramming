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

-- parenthesize :: String -> String
-- parenthesize s = "(" ++ s ++ ")"

showOp :: Exp -> Exp -> String -> String
showOp e1 e2 op = "(" ++ showExp e1 ++ op ++ showExp e2 ++ ")"

showExp :: Exp -> String
showExp (Cst exp) = show exp
showExp (Add exp1 exp2) = showOp exp1 exp2 "+"-- parenthesize (showExp exp1 ++ " + " ++ showExp exp2)
showExp (Sub exp1 exp2) = showOp exp1 exp2 "-"-- parenthesize (showExp exp1 ++ " - " ++ showExp exp2)
showExp (Mul exp1 exp2) = showOp exp1 exp2 "*"-- parenthesize (showExp exp1 ++ " * " ++ showExp exp2)
showExp (Div exp1 exp2) = showOp exp1 exp2 "/"-- parenthesize (showExp exp1 ++ " / " ++ showExp exp2)
showExp (Pow exp1 exp2) = showOp exp1 exp2 "^"-- parenthesize (showExp exp1 ++ " ^ " ++ showExp exp2)
showExp _ = error "Expression is not supported by showExp."

evalSimple :: Exp -> Integer
evalSimple (Cst exp) = exp
evalSimple (Add exp1 exp2) = evalSimple exp1 + evalSimple exp2
evalSimple (Sub exp1 exp2) = evalSimple exp1 - evalSimple exp2
evalSimple (Mul exp1 exp2) = evalSimple exp1 * evalSimple exp2
evalSimple (Div exp1 exp2) =
  case evalSimple exp2 of 0 -> error "Division by zero."
                          n -> evalSimple exp1 `div` n
evalSimple (Pow exp1 exp2) =
  case evalSimple exp2 of 0 -> 1
                          n | n >= 0 -> evalSimple exp1 ^ n
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
  case evalFull exp2 r of 0 -> error "Division by zero."
                          n -> evalFull exp1 r `div` n
evalFull (Pow exp1 exp2) r =
  case evalFull exp2 r of 0 -> 1
                          n | n >= 0 -> (evalFull exp1 r) ^ n
                            | otherwise -> error "Exponent must be positive."
evalFull (If e1 e2 e3) r =
  case evalFull e1 r of 0 -> evalFull e3 r
                        _ -> evalFull e2 r
evalFull (Var v) r =
  case r v of Just n -> n
              Nothing -> error ("Undefined variable " ++ v)
evalFull (Let v e1 e2) r = evalFull e2 (extendEnv v (evalFull e1 r) r)
evalFull (Sum _ _ _ _) _ = 0

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = undefined

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
