{-# LANGUAGE UnicodeSyntax #-}

module TestsParser (tests) where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.HUnit

import LinearProgramming.Problem
import LinearProgramming.Parser

isRight ∷ Either a b → Bool
isRight (Right _) = True
isRight (Left _) = False

fromLeft ∷ Either a b → a
fromLeft (Left x) = x

tests ∷ TestTree
tests = testGroup "Parser"
          [ simpleMax
          , simpleMin
          , greaterThanOneCoefficients
          , greaterThanOneCoefficientsWithoutMul
          , minimizationWithInequalities
          , negativeCoefficients
          , newlinesBeforeObjective
          , newlinesAfterObjective
          , newlinesBeforeEof
          , newlinesBetweenConstraints
          ]

simpleMax =
  testCase "Simple maximization" $
    let text = "max x1 + x2\n" ⧺
               "x1 + x2 + x3 = 6\n" ⧺
               "x2 + x4 = 8\n" ⧺
               ""

        problemExpected = (Maximize, [(1,1), (2,1)], constraintsExpected)
        constraintsExpected = [ ([(1,1), (2,1), (3,1)], Equal, 6)
                              , ([(2,1), (4,1)], Equal, 8)
                              ]

        result = parseProblem text
        Right tableau = result

    in do
      isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
      tableau @?= problemExpected

simpleMin =
  testCase "Simple minimization" $
    let text = "min x1 + x2\n" ⧺
               "x1 + x2 + x3 = 6\n" ⧺
               "x2 + x4 = 8\n" ⧺
               ""

        problemExpected = (Minimize, [(1,1), (2,1)], constraintsExpected)
        constraintsExpected = [ ([(1,1), (2,1), (3,1)], Equal, 6)
                              , ([(2,1), (4,1)], Equal, 8)
                              ]

        result = parseProblem text
        Right tableau = result

    in do
      isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
      tableau @?= problemExpected

greaterThanOneCoefficients =
  testCase "Greater than one coefficients" $
    let text = "max x1 + 3*x2\n" ⧺
               "x1 + x2 + x3 = 6\n" ⧺
               "9*x2 + x4 = 8\n" ⧺
               ""

        problemExpected = (Maximize, [(1,1), (2,3)], constraintsExpected)
        constraintsExpected = [ ([(1,1), (2,1), (3,1)], Equal, 6)
                              , ([(2,9), (4,1)], Equal, 8)
                              ]

        result = parseProblem text
        Right tableau = result

    in do
      isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
      tableau @?= problemExpected

greaterThanOneCoefficientsWithoutMul =
  testCase "Greater than one coefficients (no * symbol)" $
    let text = "max x1 + 3 x2\n" ⧺
               "x1 + x2 + x3 = 6\n" ⧺
               "9 x2 + x4 = 8\n" ⧺
               ""

        problemExpected = (Maximize, [(1,1), (2,3)], constraintsExpected)
        constraintsExpected = [ ([(1,1), (2,1), (3,1)], Equal, 6)
                              , ([(2,9), (4,1)], Equal, 8)
                              ]

        result = parseProblem text
        Right tableau = result

    in do
      isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
      tableau @?= problemExpected

minimizationWithInequalities =
  testCase "Minimization with inequalities" $
    let text = "min x1 + 3 x2\n" ⧺
               "x1 + x2 + x3 >= 6\n" ⧺
               "2 x2 + x4 = 8\n" ⧺
               "9 x2 + x4 <= 8\n" ⧺
               ""

        problemExpected = (Minimize, [(1,1), (2,3)], constraintsExpected)
        constraintsExpected = [ ([(1,1), (2,1), (3,1)], GreaterEqual, 6)
                              , ([(2,2), (4,1)], Equal, 8)
                              , ([(2,9), (4,1)], LesserEqual, 8)
                              ]

        result = parseProblem text
        Right tableau = result

    in do
      isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
      tableau @?= problemExpected

negativeCoefficients =
  testCase "Negative coefficients" $
    let text = "min -1 x1 + 3 x2\n" ⧺
               "x1 + x2 + x3 >= 6\n" ⧺
               "-2 x2 + x4 = -8\n" ⧺
               "9 x2 + -7 x4 <= 8\n" ⧺
               ""

        problemExpected = (Minimize, [(1,-1), (2,3)], constraintsExpected)
        constraintsExpected = [ ([(1,1), (2,1), (3,1)], GreaterEqual, 6)
                              , ([(2,-2), (4,1)], Equal, -8)
                              , ([(2,9), (4,-7)], LesserEqual, 8)
                              ]

        result = parseProblem text
        Right tableau = result

    in do
      isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
      tableau @?= problemExpected

newlinesBeforeObjective =
  testCase "Newlines before objective function" $
    let text = "\n" ⧺
               "\n" ⧺
               "min -1 x1 + 3 x2\n" ⧺
               "\n" ⧺
               "\n" ⧺
               "x1 + x2 + x3 >= 6\n" ⧺
               "-2 x2 + x4 = -8\n" ⧺
               "9 x2 + -7 x4 <= 8\n" ⧺
               ""

        problemExpected = (Minimize, [(1,-1), (2,3)], constraintsExpected)
        constraintsExpected = [ ([(1,1), (2,1), (3,1)], GreaterEqual, 6)
                              , ([(2,-2), (4,1)], Equal, -8)
                              , ([(2,9), (4,-7)], LesserEqual, 8)
                              ]

        result = parseProblem text
        Right tableau = result

    in do
      isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
      tableau @?= problemExpected

newlinesAfterObjective =
  testCase "Newlines after objective function" $
    let text = "min -1 x1 + 3 x2\n" ⧺
               "\n" ⧺
               "\n" ⧺
               "x1 + x2 + x3 >= 6\n" ⧺
               "-2 x2 + x4 = -8\n" ⧺
               "9 x2 + -7 x4 <= 8\n" ⧺
               ""

        problemExpected = (Minimize, [(1,-1), (2,3)], constraintsExpected)
        constraintsExpected = [ ([(1,1), (2,1), (3,1)], GreaterEqual, 6)
                              , ([(2,-2), (4,1)], Equal, -8)
                              , ([(2,9), (4,-7)], LesserEqual, 8)
                              ]

        result = parseProblem text
        Right tableau = result

    in do
      isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
      tableau @?= problemExpected

newlinesBeforeEof =
  testCase "Newlines before end of file" $
    let text = "min -1 x1 + 3 x2\n" ⧺
               "x1 + x2 + x3 >= 6\n" ⧺
               "-2 x2 + x4 = -8\n" ⧺
               "9 x2 + -7 x4 <= 8\n" ⧺
               "\n" ⧺
               "\n" ⧺
               ""

        problemExpected = (Minimize, [(1,-1), (2,3)], constraintsExpected)
        constraintsExpected = [ ([(1,1), (2,1), (3,1)], GreaterEqual, 6)
                              , ([(2,-2), (4,1)], Equal, -8)
                              , ([(2,9), (4,-7)], LesserEqual, 8)
                              ]

        result = parseProblem text
        Right tableau = result

    in do
      isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
      tableau @?= problemExpected

newlinesBetweenConstraints =
  testCase "Newlines between constraints" $
    let text = "min -1 x1 + 3 x2\n" ⧺
               "x1 + x2 + x3 >= 6\n" ⧺
               "\n" ⧺
               "-2 x2 + x4 = -8\n" ⧺
               "\n" ⧺
               "\n" ⧺
               "9 x2 + -7 x4 <= 8\n" ⧺
               ""

        problemExpected = (Minimize, [(1,-1), (2,3)], constraintsExpected)
        constraintsExpected = [ ([(1,1), (2,1), (3,1)], GreaterEqual, 6)
                              , ([(2,-2), (4,1)], Equal, -8)
                              , ([(2,9), (4,-7)], LesserEqual, 8)
                              ]

        result = parseProblem text
        Right tableau = result

    in do
      isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
      tableau @?= problemExpected
