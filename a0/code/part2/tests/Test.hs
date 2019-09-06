-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone
import Control.Exception

-- environments for testing extendEnv
testEnv1 = extendEnv "c" 100 (extendEnv "b" 10 (extendEnv "a" 5 initEnv))
testEnv2 = extendEnv "d" 25 (extendEnv "b" 99 (extendEnv "c" (-2) testEnv1))

tests :: [(String, Bool)]
tests = [
  -- evalSimple tests
  ("test_ES_0", evalSimple (Cst 13) == 13),
  ("test_ES_1", evalSimple (Add (Cst 2) (Cst 2)) == 4),
  ("test_ES_2", evalSimple (Sub (Cst 3) (Cst 5)) == -2),
  ("test_ES_2", evalSimple (Mul (Cst 5) (Cst 10)) == 50),
  ("test_ES_3", evalSimple (Div (Cst 15) (Cst 3)) == 5),
  ("test_ES_4", evalSimple (Pow (Cst 2) (Cst 4)) == 16),
  ("test_ES_5", evalSimple (Mul (Sub (Add (Cst 100)
    (Div (Pow (Cst 2) (Cst 4)) (Cst 4))) (Cst 60)) (Cst 2)) == 88),

  -- extendEnv tests
  ("test_EX_0", testEnv1 "b" == Just 10),
  ("test_EX_1", testEnv2 "b" == Just 99),
  ("test_EX_2", testEnv1 "d" == Nothing),
  ("test_EX_3", testEnv2 "d" == Just 25),
  ("test_EX_4", testEnv1 "a" == Just 5),
  ("test_EX_5", testEnv2 "a" == Just 5),

  -- evalFull tests
  ("test_EF_0", evalFull (Cst 13) initEnv == 13),
  ("test_EF_1", evalFull (Add (Cst 2) (Cst 2)) initEnv == 4),
  ("test_EF_2", evalFull (Sub (Cst 3) (Cst 5)) initEnv == -2),
  ("test_EF_2", evalFull (Mul (Cst 5) (Cst 10)) initEnv == 50),
  ("test_EF_3", evalFull (Div (Cst 15) (Cst 3)) initEnv == 5),
  ("test_EF_4a", evalFull (Pow (Cst 2) (Cst 4)) initEnv == 16),
  ("test_EF_4b", evalFull (Pow (Cst 2) (Cst 0)) initEnv == 1),
  ("test_EF_5", evalFull (Mul (Sub (Add (Cst 100)
    (Div (Pow (Cst 2) (Cst 4)) (Cst 4))) (Cst 60)) (Cst 2)) initEnv == 88),
  ("test_EF_6", evalFull (Add (Var "a") (Var "b")) testEnv1 == 15),
  ("test_EF_7", evalFull (Mul (Var "d") (Cst 4)) testEnv2 == 100),
  ("test_EF_8", evalFull (If (Cst 1) (Cst 10) (Cst 20)) initEnv == 10),
  ("test_EF_9", evalFull (If (Cst 0) (Pow (Cst 10) (Cst (-2))) (Cst 20)) initEnv == 20),
  ("test_EF_10", evalFull (Var "c") testEnv2 == (-2)),
  ("test_EF_11", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42),
  ("test_EF_12", evalFull (Let "a" (Div (Cst 2) (Cst 0)) (Cst 10)) initEnv == 10),
  ("test_EF_13", evalFull (Sum "i" (Cst 1) (Cst 10)
    (Mul (Var "i") (Cst 2))) initEnv == 110),
  ("test_EF_14", evalFull (Sum "x" (Cst 1) (Add (Cst 2) (Cst 2))
    (Mul (Var "x") (Var "x"))) initEnv == 30),

  -- evalErr tests
  ("test_EE_0", evalErr (Cst 13) initEnv == Right 13),
  ("test_EE_1", evalErr (Add (Cst 2) (Cst 2)) initEnv == Right 4),
  ("test_EE_2", evalErr (Sub (Cst 3) (Cst 5)) initEnv == Right (-2)),
  ("test_EE_2", evalErr (Mul (Cst 5) (Cst 10)) initEnv == Right 50),
  ("test_EE_3", evalErr (Div (Cst 15) (Cst 3)) initEnv == Right 5),
  ("test_EE_4a", evalErr (Pow (Cst 2) (Cst 4)) initEnv == Right 16),
  ("test_EE_4b", evalErr (Pow (Cst 2) (Cst 0)) initEnv == Right 1),
  ("test_EE_5", evalErr (Mul (Sub (Add (Cst 100)
    (Div (Pow (Cst 2) (Cst 4)) (Cst 4))) (Cst 60)) (Cst 2)) initEnv == Right 88),
  ("test_EE_6", evalErr (Add (Var "a") (Var "b")) testEnv1 == Right 15),
  ("test_EE_7", evalErr (Mul (Var "d") (Cst 4)) testEnv2 == Right 100),
  ("test_EE_8", evalErr (If (Cst 1) (Cst 10) (Cst 20)) initEnv == Right 10),
  ("test_EE_9", evalErr (If (Cst 0) (Pow (Cst 10) (Cst (-2))) (Cst 20)) initEnv == Right 20),
  ("test_EE_10", evalErr (Var "c") testEnv2 == Right (-2)),
  ("test_EE_11", evalErr (Let "a" (Cst 42) (Var "a")) initEnv == Right 42),
  ("test_EE_12", evalErr (Let "a" (Div (Cst 2) (Cst 0)) (Cst 10)) initEnv == Right 10),
  ("test_EE_12", evalErr (Let "a" (Cst 50) (Add (Var "a") (Var "d"))) testEnv2 == Right 75),
  ("test_EE_13", evalErr (Sum "i" (Cst 1) (Cst 10)
    (Mul (Var "i") (Cst 2))) initEnv == Right 110),
  ("test_EE_14", evalErr (Sum "x" (Cst 1) (Add (Cst 2) (Cst 2))
    (Mul (Var "x") (Var "x"))) initEnv == Right 30),
  ("test_EE_15", evalErr (Div (Cst 2) (Cst 0)) initEnv == Left EDivZero),
  ("test_EE_16", evalErr (Pow (Cst 2) (Cst (-2))) initEnv == Left ENegPower),
  ("test_EE_17", evalErr (Var "x") initEnv == Left (EBadVar "x")),
  ("test_EE_18", evalErr (Let "x" (Div (Cst 2) (Cst 0)) (Var "x")) initEnv == Left EDivZero),
  ("test_EE_19", evalErr (Let "x" (Div (Cst 2) (Cst 0)) (Pow (Cst 2) (Cst (-1)))) initEnv == Left ENegPower),
  -- TODO: test errors in different expressions in If and Sum
  ("dummy_test", True)]

main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn $ "\nAll " ++ (show (length tests)) ++ " tests passed!"
                exitSuccess
       _ -> do putStrLn $ "\n" ++ (show (length failed)) ++ "/" ++ (show (length tests)) ++ " failed tests: " ++ intercalate ", " failed
               exitFailure
