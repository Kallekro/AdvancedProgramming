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

showExp :: Exp -> String
showExp (Cst exp) = show exp
showExp (Add exp1 exp2) = showExp exp1 ++ " + " ++ showExp exp2
showExp (Sub exp1 exp2) = showExp exp1 ++ " - " ++ showExp exp2
showExp (Mul exp1 exp2) = "(" ++ showExp exp1 ++ ") * (" ++ showExp exp2 ++ ")"

evalSimple :: Exp -> Integer
evalSimple = undefined

extendEnv :: VName -> Integer -> Env -> Env
extendEnv = undefined

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
