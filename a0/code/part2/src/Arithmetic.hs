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

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

showExp :: Exp -> String
showExp (Cst exp) = show exp
showExp (Add exp1 exp2) = parenthesize (showExp exp1 ++ " + " ++ showExp exp2)
showExp (Sub exp1 exp2) = parenthesize (showExp exp1 ++ " - " ++ showExp exp2)
showExp (Mul exp1 exp2) = parenthesize (showExp exp1 ++ " * " ++ showExp exp2)
showExp (Div exp1 exp2) = parenthesize (showExp exp1 ++ " / " ++ showExp exp2)
showExp (Pow exp1 exp2) = parenthesize (showExp exp1 ++ " ^ " ++ showExp exp2)
showExp _ = error "Expression is not supported by showExp."

evalSimple :: Exp -> Integer
evalSimple (Cst exp) = exp
evalSimple (Add exp1 exp2) = evalSimple exp1 + evalSimple exp2
evalSimple (Sub exp1 exp2) = evalSimple exp1 - evalSimple exp2
evalSimple (Mul exp1 exp2) = evalSimple exp1 * evalSimple exp2
evalSimple (Div exp1 exp2) = evalSimple exp1 `div` evalSimple exp2
evalSimple (Pow exp1 exp2) = evalSimple exp1 ^ evalSimple exp2
evalSimple _ = error "Expression is not supported by evalSimple."

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \_v -> if (v == _v) then Just n else r _v

evalFull :: Exp -> Env -> Integer
evalFull = undefined

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = undefined

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
