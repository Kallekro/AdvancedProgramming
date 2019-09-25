-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests =
  testGroup "All tests" [

  testGroup "Statements tests" [
    testCase "Single statement1" $
      parseString "1" @?= 
        Right [SExp (Const (IntVal 1))],
    testCase "Single statement2" $
      parseString "fourtytwo" @?= 
        Right [SExp (Var "fourtytwo")],
    testCase "Two statements" $
      parseString "4;2" @?= 
        Right [SExp (Const (IntVal 4)), SExp (Const (IntVal 2))],
    testCase "Multiple statements" $
        parseString "var1 = 5; var1; var1-5" @?=
          Right [SDef "var1" (Const (IntVal 5)), SExp (Var "var1"), 
                 SExp (Oper Minus (Var "var1") (Const (IntVal 5)))]
  ],

  testGroup "Statement tests" [
    testCase "Identifier simple" $
      parseString "f1 = 42" @?=
        Right [SDef "f1" (Const (IntVal 42))],
    testCase "Identifier complex" $
      parseString "id = (42 + 42) // 2" @?=
        Right [SDef "id" (Oper Div 
                           (Oper Plus (Const (IntVal 42)) (Const (IntVal 42)))
                           (Const (IntVal 2)))],
    testCase "Expression simple" $
      parseString "42" @?=
        Right [SExp (Const (IntVal 42))],
    testCase "Expression complex" $
      parseString "var = 'jimbo'" @?=
        Right [SDef "var" (Const (StringVal "jimbo"))]
  ],

  testGroup "Expression tests" [
    testCase "Numconst" $
      parseString "42" @?=
        Right [SExp (Const (IntVal 42))],
    testCase "StringConst" $
      parseString "'string'" @?=
        Right [SExp (Const (StringVal "string"))],
    testCase "Keyword None" $
      parseString "None" @?=
        Right [SExp (Const NoneVal)],
    testCase "Keyword True" $
      parseString "True" @?=
        Right [SExp (Const TrueVal)],
    testCase "Keyword False" $
      parseString "False" @?=
        Right [SExp (Const FalseVal)],
    testCase "Identifier" $
      parseString "variable" @?=
        Right [SExp (Var "variable")],
    testCase "Operation 1" $
      parseString "2 + 2" @?=
        Right [SExp (Oper Plus (Const (IntVal 2)) (Const (IntVal 2)))],
    testCase "Operation 2" $
      parseString "var1 // 42" @?=
        Right [SExp (Oper Div (Var "var1") (Const (IntVal 42)))],
    testCase "Not 1" $
      parseString "not 1" @?=
        Right [SExp (Not (Const (IntVal 1)))],
    testCase "Not 2" $
      parseString "not 'jimbo'" @?=
        Right [SExp (Not (Const (StringVal "jimbo")))],
    testCase "Parenthesis" $
      parseString "2 * (42 + 2)" @?=
        Right [SExp (Oper Times (Const (IntVal 2)) 
                                (Oper Plus (Const (IntVal 42)) 
                                           (Const (IntVal 2))))],
    testCase "Function call one arg" $
      parseString "print(42)" @?=
        Right [SExp (Call "print" [ Const (IntVal 42) ]) ],
    testCase "Function call two args" $
      parseString "print(42,156)" @?=
        Right [SExp (Call "print" [Const (IntVal 42), 
                                   Const (IntVal 156)])],
    testCase "List 1" $
      parseString "[1,5,6]" @?=
        Right [SExp (List [Const (IntVal 1), Const (IntVal 5),
                           Const (IntVal 6)])]
  ]
  
  -- end of all tests
  ]