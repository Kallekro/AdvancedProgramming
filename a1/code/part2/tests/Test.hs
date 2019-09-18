-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests =
  testGroup "All tests"
  [testGroup "Monad operator tests"
  [ -- Comp return and bind
    testCase "testCompBase1" $
      (runComp (return ()) [])
      @?= (Right (), []),
    testCase "testCompBase2" $
      (runComp (Comp (\_ -> (Right (), ["some output"]))) [])
      @?= (Right (), ["some output"]),
    testCase "testCompBase3" $
      (runComp (do { x <- return (); return x}) [])
      @?= (Right (), []),
    testCase "testCompBase4" $
      (runComp (do { x <- return "Hello"; return x}) [])
      @?= (Right "Hello", []),
    -- output
     testCase "testOutput1" $
      (runComp (do { output "test output"; return () }) [])
      @?= (Right (), ["test output"]),
    testCase "testOutput2" $
      (runComp (do { output "test1"; output "test2"; return () }) [])
      @?= (Right (), ["test1", "test2"]),
    -- abort
    testCase "testAbort1" $
      ((runComp (abort (EBadVar "x")) [])
        :: (Either RunError String, [String]))
      @?= (Left (EBadVar "x"), []),
    testCase "testAbort2" $
      ((runComp (do { abort (EBadFun "f");
                      output "no errors?";
                      return ()}) [])
        :: (Either RunError (), [String]))
      @?= (Left (EBadFun "f"), []),
    testCase "testAbort3" $
      ((runComp (do { output "before crash";
                      abort (EBadArg "a");
                      output "after crash";
                      return ()}) [])
        :: (Either RunError (), [String]))
      @?= (Left (EBadArg "a"), ["before crash"]),
    -- look
    testCase "testLook1" $
      (runComp (look "a") [("a", IntVal 1)])
      @?= (Right (IntVal 1), []),
    testCase "testLook2" $
      (runComp (look "a") [("b", IntVal 2), ("a", IntVal 1)])
      @?= (Right (IntVal 1), []),
    testCase "testLook3" $
      (runComp (look "a") [("a", IntVal 3), ("b", IntVal 2), ("a", IntVal 1)])
      @?= (Right (IntVal 3), []),
    testCase "testLook4" $
      (runComp (look "a") [])
      @?= (Left (EBadVar "a"), []),
    testCase "testLook5" $
      (runComp (do { a <- look "a"; output (show a); return () })
        [("a", IntVal 1)])
      @?= (Right (), ["IntVal 1"]),
    -- withBinding
    testCase "testWithBinding1" $
      (runComp (withBinding "a" (IntVal 1) (look "a")) [])
      @?= (Right (IntVal 1), []),
    testCase "testWithBinding2" $
      (runComp (withBinding "a" (IntVal 2) (look "a")) [("a", IntVal 1)])
      @?= (Right (IntVal 2), []),
    testCase "testWithBinding3" $
      (runComp (withBinding "a" (IntVal 2) (look "b")) [])
      @?= (Left (EBadVar "b"), []),
    testCase "testWithBinding4" $
      (runComp (withBinding "a" (IntVal 2) (look "b"))
        [("b", StringVal "Maxwell")])
      @?= (Right (StringVal "Maxwell"), []),
    testCase "testWithBinding5" $
      (runComp (withBinding "a" (StringVal "Oh, ") ( do
        a <- look "a"
        b <- look "b"
        case (a, b) of
          (StringVal s1, StringVal s2) -> return $ s1 ++ s2
          _ -> return ""
        )) [("b", StringVal "Darlin'")])
      @?= (Right "Oh, Darlin'", [])],

  testGroup "Helper function tests"
    [
    -- truthy
    testCase "testTruthy1" $ truthy NoneVal @?= False,
    testCase "testTruthy2" $ truthy TrueVal @?= True,
    testCase "testTruthy3" $ truthy FalseVal @?= False,
    testCase "testTruthy4" $ truthy (IntVal 0) @?= False,
    testCase "testTruthy5" $ truthy (IntVal 1) @?= True,
    testCase "testTruthy6" $ truthy (IntVal (-1)) @?= True,
    testCase "testTruthy7" $ truthy (StringVal "") @?= False,
    testCase "testTruthy7" $ truthy (StringVal "hey") @?= True,
    testCase "testTruthy7" $ truthy (ListVal []) @?= False,
    testCase "testTruthy7" $ truthy (ListVal [IntVal 1]) @?= True,
    -- operate
    -- operate Plus
    testCase "testOperatePlus1" $
      operate Plus (IntVal 1) (IntVal 2)
      @?= Right (IntVal 3),
    testCase "testOperatePlus2" $
      operate Plus NoneVal (IntVal 2)
      @?= Left ("Plus: Operand mismatch."),
    testCase "testOperatePlus3" $
      operate Plus (FalseVal) (TrueVal)
      @?= Left ("Plus: Operand mismatch."),
    -- operate Minus
    testCase "testOperateMinus1" $
      operate Minus (IntVal 2) (IntVal 1)
      @?= Right (IntVal 1),
    testCase "testOperateMinus2" $
      operate Minus (IntVal (-99)) (IntVal (-100))
      @?= Right (IntVal 1),
    testCase "testOperateMinus3" $
      operate Minus (NoneVal) (IntVal 0)
      @?= Left ("Minus: Operand mismatch."),
    testCase "testOperateTimes1" $
      operate Times (IntVal 2) (IntVal 5)
      @?= Right (IntVal 10)
    ],
  testGroup "Other tests"
    [testCase "execute crash.boa" $
      execute crashAST @?= crashOut]]
  where
    crashAST = [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                              (Const (IntVal 2))]),
                SExp (Var "hello")]
    crashOut = (["4"], Just (EBadVar "hello"))
