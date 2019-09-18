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
  [testGroup "Monad operators"
  [ -- Comp return and bind
    testCase "compBase1" $
      (runComp (return ()) [])
      @?= (Right (), []),
    testCase "compBase2" $
      (runComp (Comp (\_ -> (Right (), ["some output"]))) [])
      @?= (Right (), ["some output"]),
    testCase "compBase3" $
      (runComp (do { x <- return (); return x}) [])
      @?= (Right (), []),
    testCase "compBase4" $
      (runComp (do { x <- return "Hello"; return x}) [])
      @?= (Right "Hello", []),
    -- output
     testCase "output1" $
      (runComp (do { output "test output"; return () }) [])
      @?= (Right (), ["test output"]),
    testCase "output2" $
      (runComp (do { output "test1"; output "test2"; return () }) [])
      @?= (Right (), ["test1", "test2"]),
    -- abort
    testCase "abort1" $
      ((runComp (abort (EBadVar "x")) [])
        :: (Either RunError String, [String]))
      @?= (Left (EBadVar "x"), []),
    testCase "abort2" $
      ((runComp (do { abort (EBadFun "f");
                      output "no errors?";
                      return ()}) [])
        :: (Either RunError (), [String]))
      @?= (Left (EBadFun "f"), []),
    testCase "abort3" $
      ((runComp (do { output "before crash";
                      abort (EBadArg "a");
                      output "after crash";
                      return ()}) [])
        :: (Either RunError (), [String]))
      @?= (Left (EBadArg "a"), ["before crash"]),
    -- look
    testCase "look1" $
      (runComp (look "a") [("a", IntVal 1)])
      @?= (Right (IntVal 1), []),
    testCase "look2" $
      (runComp (look "a") [("b", IntVal 2), ("a", IntVal 1)])
      @?= (Right (IntVal 1), []),
    testCase "look3" $
      (runComp (look "a") [("a", IntVal 3), ("b", IntVal 2), ("a", IntVal 1)])
      @?= (Right (IntVal 3), []),
    testCase "look4" $
      (runComp (look "a") [])
      @?= (Left (EBadVar "a"), []),
    testCase "look5" $
      (runComp (do { a <- look "a"; output (show a); return () })
        [("a", IntVal 1)])
      @?= (Right (), ["IntVal 1"]),
    -- withBinding
    testCase "withBinding1" $
      (runComp (withBinding "a" (IntVal 1) (look "a")) [])
      @?= (Right (IntVal 1), []),
    testCase "withBinding2" $
      (runComp (withBinding "a" (IntVal 2) (look "a")) [("a", IntVal 1)])
      @?= (Right (IntVal 2), []),
    testCase "withBinding3" $
      (runComp (withBinding "a" (IntVal 2) (look "b")) [])
      @?= (Left (EBadVar "b"), []),
    testCase "withBinding4" $
      (runComp (withBinding "a" (IntVal 2) (look "b"))
        [("b", StringVal "Maxwell")])
      @?= (Right (StringVal "Maxwell"), []),
    testCase "withBinding5" $
      (runComp (withBinding "a" (StringVal "Oh, ") ( do
        a <- look "a"
        b <- look "b"
        case (a, b) of
          (StringVal s1, StringVal s2) -> return $ s1 ++ s2
          _ -> return ""
        )) [("b", StringVal "Darlin'")])
      @?= (Right "Oh, Darlin'", [])],

  testGroup "Helper functions truthy and operate"
    [
    -- truthy
    testCase "truthy1" $ truthy NoneVal @?= False,
    testCase "truthy2" $ truthy TrueVal @?= True,
    testCase "truthy3" $ truthy FalseVal @?= False,
    testCase "truthy4" $ truthy (IntVal 0) @?= False,
    testCase "truthy5" $ truthy (IntVal 1) @?= True,
    testCase "truthy6" $ truthy (IntVal (-1)) @?= True,
    testCase "truthy7" $ truthy (StringVal "") @?= False,
    testCase "truthy7" $ truthy (StringVal "hey") @?= True,
    testCase "truthy7" $ truthy (ListVal []) @?= False,
    testCase "truthy7" $ truthy (ListVal [IntVal 1]) @?= True,
    -- operate
    -- operate Plus
    testCase "operatePlus1" $
      operate Plus (IntVal 1) (IntVal 2)
      @?= Right (IntVal 3),
    testCase "operatePlus2" $
      operate Plus NoneVal (IntVal 2)
      @?= Left ("Plus: Operand mismatch."),
    testCase "operatePlus3" $
      operate Plus (FalseVal) (TrueVal)
      @?= Left ("Plus: Operand mismatch."),
    -- operate Minus
    testCase "operateMinus1" $
      operate Minus (IntVal 2) (IntVal 1)
      @?= Right (IntVal 1),
    testCase "operateMinus2" $
      operate Minus (IntVal (-99)) (IntVal (-100))
      @?= Right (IntVal 1),
    testCase "operateMinus3" $
      operate Minus (NoneVal) (IntVal 0)
      @?= Left ("Minus: Operand mismatch."),
    -- operate Times
    testCase "operateTimes1" $
      operate Times (IntVal 2) (IntVal 5)
      @?= Right (IntVal 10),
    testCase "operateTimes2" $
      operate Times (IntVal 0) (IntVal 5)
      @?= Right (IntVal 0),
    testCase "operateTimes3" $
      operate Times (IntVal (-1)) (IntVal 10)
      @?= Right (IntVal (-10)),
    testCase "operateTimes4" $
      operate Times (IntVal (-2)) (IntVal (-2))
      @?= Right (IntVal (4)),
    testCase "operateTimes5" $
      operate Times NoneVal (IntVal (-2))
      @?= Left ("Times: Operand mismatch."),
    -- operate Div
    testCase "operateDiv1" $
      operate Div (IntVal 4) (IntVal 2)
      @?= Right (IntVal 2),
    testCase "operateDiv2" $
      operate Div (IntVal 5) (IntVal 2)
      @?= Right (IntVal 2),
    testCase "operateDiv3" $
      operate Div (IntVal 0) (IntVal 2)
      @?= Right (IntVal 0),
    testCase "operateDiv4" $
      operate Div (IntVal 2) (IntVal 0)
      @?= Left ("Division by zero."),
    -- operate Mod
    testCase "operateMod1" $
      operate Mod (IntVal 4) (IntVal 2)
      @?= Right (IntVal 0),
    testCase "operateMod2" $
      operate Mod (IntVal 5) (IntVal 2)
      @?= Right (IntVal 1),
    testCase "operateMod3" $
      operate Mod (IntVal 0) (IntVal 2)
      @?= Right (IntVal 0),
    testCase "operateMod4" $
      operate Mod (IntVal 2) (IntVal 0)
      @?= Left ("Modulo by zero."),
    -- operate Eq
    testCase "operateEq1" $
      operate Eq (IntVal 2) (IntVal 2)
      @?= Right (TrueVal),
    testCase "operateEq2" $
      operate Eq (IntVal 1) (IntVal 2)
      @?= Right (FalseVal),
    testCase "operateEq3" $
      operate Eq (IntVal 1) (StringVal "one")
      @?= Right (FalseVal),
    testCase "operateEq4" $
      operate Eq (StringVal "one") (StringVal "one")
      @?= Right (TrueVal),
    testCase "operateEq5" $
      operate Eq (StringVal "one") (StringVal "two")
      @?= Right (FalseVal),
    testCase "operateEq6" $
      operate Eq (ListVal [IntVal 2]) (IntVal 2)
      @?= Right (FalseVal),
    testCase "operateEq7" $
      operate Eq (ListVal [IntVal 1, IntVal 2])
        (ListVal [IntVal 1, IntVal 2])
      @?= Right (TrueVal),
    testCase "operateEq8" $
      operate Eq NoneVal NoneVal
      @?= Right (TrueVal),
    testCase "operateEq9" $
      operate Eq TrueVal FalseVal
      @?= Right (FalseVal),
    testCase "operateEq9" $
      operate Eq TrueVal TrueVal
      @?= Right (TrueVal),
    -- operate Less
    testCase "operateLess1" $
      operate Less (IntVal 2) (IntVal 2)
      @?= Right (FalseVal),
    testCase "operateLess2" $
      operate Less (IntVal 1) (IntVal 2)
      @?= Right (TrueVal),
    testCase "operateLess3" $
      operate Less (IntVal 2) (IntVal 1)
      @?= Right (FalseVal),
    testCase "operateLess4" $
      operate Less (IntVal (-10)) (IntVal 1)
      @?= Right (TrueVal),
    testCase "operateLess5" $
      operate Less (IntVal 1) (StringVal "1")
      @?= Left ("Less: Operand mismatch."),
    -- operate Greater
    testCase "operateGreater1" $
      operate Greater (IntVal 2) (IntVal 2)
      @?= Right (FalseVal),
    testCase "operateGreater2" $
      operate Greater (IntVal 1) (IntVal 2)
      @?= Right (FalseVal),
    testCase "operateGreater3" $
      operate Greater (IntVal 2) (IntVal 1)
      @?= Right (TrueVal),
    testCase "operateGreater4" $
      operate Greater (IntVal (-10)) (IntVal 1)
      @?= Right (FalseVal),
    testCase "operateGreater5" $
      operate Greater (IntVal 1) (StringVal "1")
      @?= Left ("Greater: Operand mismatch."),
    -- operate In
    testCase "operateIn1" $
      operate In (IntVal 2) (ListVal [IntVal 1, IntVal 2, IntVal 3])
      @?= Right (TrueVal),
    testCase "operateIn2" $
      operate In (IntVal 4) (ListVal [IntVal 1, IntVal 2, IntVal 3])
      @?= Right (FalseVal),
    testCase "operateIn3" $
      operate In (StringVal "beans")
        (ListVal [IntVal 1, IntVal 2, StringVal "beans", IntVal 3])
      @?= Right (TrueVal),
    testCase "operateIn4" $
      operate In (StringVal "beans")
        (ListVal [StringVal "cool beans"])
      @?= Right (FalseVal),
    testCase "operateIn5" $
      operate In (TrueVal)
        (ListVal [NoneVal, FalseVal, IntVal 2, TrueVal, NoneVal])
      @?= Right (TrueVal),
    testCase "operateIn6" $
      operate In (ListVal [TrueVal])
        (ListVal [NoneVal, FalseVal, IntVal 2, TrueVal, NoneVal])
      @?= Right (FalseVal),
    testCase "operateIn7" $
      operate In (ListVal [TrueVal])
        (ListVal [NoneVal, FalseVal, IntVal 2, ListVal [TrueVal]])
      @?= Right (TrueVal),
    testCase "operateIn8" $
      operate In TrueVal FalseVal
      @?= Left ("In: Operand mismatch.")
    ],
  testGroup "apply (and built-ins)"
    [ -- apply range
      testCase "range1" $
        runComp (apply "range" [IntVal 3]) []
        @?= (Right (ListVal [IntVal 0, IntVal 1, IntVal 2]), []),
      testCase "range2" $
        runComp (apply "range" [IntVal 0, IntVal 3]) []
        @?= (Right (ListVal [IntVal 0, IntVal 1, IntVal 2]), []),
      testCase "range3" $
        runComp (apply "range" [IntVal 0, IntVal 3, IntVal 1]) []
        @?= (Right (ListVal [IntVal 0, IntVal 1, IntVal 2]), []),
      testCase "range4" $
        runComp (apply "range" [IntVal 0, IntVal 3, IntVal 2]) []
        @?= (Right (ListVal [IntVal 0, IntVal 2]), []),
      testCase "range5" $
        runComp (apply "range" [IntVal (-2), IntVal 3, IntVal 2]) []
        @?= (Right (ListVal [IntVal (-2), IntVal 0, IntVal 2]), []),
      testCase "range6" $
        runComp (apply "range" [IntVal 0]) []
        @?= (Right (ListVal []), []),
      testCase "range7" $
        runComp (apply "range" [IntVal (-1)]) []
        @?= (Right (ListVal []), []),
      testCase "range8" $
        runComp (apply "range" [IntVal 3, IntVal 0]) []
        @?= (Right (ListVal []), []),
      testCase "range9" $
        runComp (apply "range" [IntVal 3, IntVal 0, IntVal (-1)]) []
        @?= (Right (ListVal [IntVal 3, IntVal 2, IntVal 1]), []),
      testCase "range10" $
        runComp (apply "range" [IntVal 3, IntVal 0, IntVal 1]) []
        @?= (Right (ListVal []), []),
      testCase "range11" $
        runComp (apply "range" [StringVal "hello"]) []
        @?= (Left (EBadArg "invalid arguments for range."), []),
      testCase "range12" $
        runComp (apply "range" [IntVal 3, StringVal "hello"]) []
        @?= (Left (EBadArg "invalid arguments for range."), []),
      -- apply print
      testCase "print1" $
        runComp (apply "print" [StringVal "Hello world!"]) []
        @?= (Right NoneVal, ["Hello world!"]),
      testCase "print2" $
        runComp (apply "print" [StringVal "Hello", StringVal "world!"]) []
        @?= (Right NoneVal, ["Hello world!"]),
      testCase "print3" $
        runComp (apply "print" [StringVal "Power: >", IntVal 9000]) []
        @?= (Right NoneVal, ["Power: > 9000"]),
      testCase "print4" $
        runComp (apply "print" [IntVal 42, StringVal "foo",
          ListVal [TrueVal, ListVal []], IntVal (-1)]) []
        @?= (Right NoneVal, ["42 foo [True, []] -1"]),
      testCase "print5" $
        runComp (apply "print" [ListVal [IntVal 1, IntVal 2]]) []
        @?= (Right NoneVal, ["[1, 2]"])
    ],
  testGroup "eval"
    [

    ],
  testGroup "exec and execute"
    [

    ],
  testGroup "Other tests"
    [testCase "execute crash.boa" $
      execute crashAST @?= crashOut]]
  where
    crashAST = [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                              (Const (IntVal 2))]),
                SExp (Var "hello")]
    crashOut = (["4"], Just (EBadVar "hello"))
