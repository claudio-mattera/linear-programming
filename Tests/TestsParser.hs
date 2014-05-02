{-# LANGUAGE UnicodeSyntax #-}

module TestsParser (tests) where

import Prelude.Unicode

import Data.Ratio

import Test.Tasty
import Test.Tasty.HUnit

import LinearProgramming.Problem
import LinearProgramming.Tableau
import LinearProgramming.Parser

isRight ∷ Either a b → Bool
isRight (Right _) = True
isRight (Left _) = False

fromLeft ∷ Either a b → a
fromLeft (Left x) = x
fromLeft _ = error "Not Left"

tests ∷ TestTree
tests = testGroup "Parser"
          [ testsProblem
          , testsDirectTableau
          ]

testsProblem ∷ TestTree
testsProblem = testGroup "Problem"
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

  where

  simpleMax ∷ TestTree
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


  simpleMin ∷ TestTree
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


  greaterThanOneCoefficients ∷ TestTree
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


  greaterThanOneCoefficientsWithoutMul ∷ TestTree
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


  minimizationWithInequalities ∷ TestTree
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


  negativeCoefficients ∷ TestTree
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


  newlinesBeforeObjective ∷ TestTree
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


  newlinesAfterObjective ∷ TestTree
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


  newlinesBeforeEof ∷ TestTree
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


  newlinesBetweenConstraints ∷ TestTree
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


testsDirectTableau ∷ TestTree
testsDirectTableau = testGroup "Direct tableau"
          [ integerCoefficients
          , rationalCoefficients
          , newlinesAtBeginning
          , newlinesBeforeEof
          , trailingSpaces
          , largeTableau
          ]

  where

  integerCoefficients ∷ TestTree
  integerCoefficients =
    testCase "Integer coefficients" $
      let text = "3 4\n" ⧺
                 "1 3 6\n" ⧺
                 "2 4 5 7\n" ⧺
                 "1 3 0\n" ⧺
                 "0 0 -1 -2\n" ⧺
                 "1 -1 0 -1\n" ⧺
                 "-1 0 -2 0\n" ⧺
                 "1 -1  2 3 1"

          tableauExpected = makeTableau
            4
            3
            [ [ 0,  0, -1, -2]
            , [ 1, -1,  0, -1]
            , [-1,  0, -2,  0]
            ]
            [1, 3, 0]
            [-1, 2, 3, 1]
            1
            [1,3,6]
            [2,4,5,7]

          result = parseDirectTableau text
          Right tableau = result

      in do
        isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
        tableau @?= tableauExpected


  rationalCoefficients ∷ TestTree
  rationalCoefficients =
    testCase "Rational coefficients" $
      let text = "3 4\n" ⧺
                 "1 3 6\n" ⧺
                 "2 4 5 7\n" ⧺
                 "1.5 3.0 0.0\n" ⧺
                 "0.0 0.0 -1.0 -2.0\n" ⧺
                 "1.0 -1.0 0.0 -1.0\n" ⧺
                 "-1.0 0.9 -2.0 0.0\n" ⧺
                 "1.0 -1.0  2.0 3.0 1.0"

          tableauExpected = makeTableau
            4
            3
            [ [ 0,  0, -1, -2]
            , [ 1, -1,  0, -1]
            , [-1,  9%10, -2,  0]
            ]
            [3%2, 3, 0]
            [-1, 2, 3, 1]
            1
            [1,3,6]
            [2,4,5,7]

          result = parseDirectTableau text
          Right tableau = result

      in do
        isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
        tableau @?= tableauExpected


  newlinesAtBeginning ∷ TestTree
  newlinesAtBeginning =
    testCase "Newlines at beginning" $
      let text = "\n\n3 4\n" ⧺
                 "1 3 6\n" ⧺
                 "2 4 5 7\n" ⧺
                 "1.5 3.0 0.0\n" ⧺
                 "0.0 0.0 -1.0 -2.0\n" ⧺
                 "1.0 -1.0 0.0 -1.0\n" ⧺
                 "-1.0 0.9 -2.0 0.0\n" ⧺
                 "1.0 -1.0  2.0 3.0 1.0"

          tableauExpected = makeTableau
            4
            3
            [ [ 0,  0, -1, -2]
            , [ 1, -1,  0, -1]
            , [-1,  9%10, -2,  0]
            ]
            [3%2, 3, 0]
            [-1, 2, 3, 1]
            1
            [1,3,6]
            [2,4,5,7]

          result = parseDirectTableau text
          Right tableau = result

      in do
        isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
        tableau @?= tableauExpected


  newlinesBeforeEof ∷ TestTree
  newlinesBeforeEof =
    testCase "Newlines before EOF" $
      let text = "\n\n3 4\n" ⧺
                 "1 3 6\n" ⧺
                 "2 4 5 7\n" ⧺
                 "1.5 3.0 0.0\n" ⧺
                 "0.0 0.0 -1.0 -2.0\n" ⧺
                 "1.0 -1.0 0.0 -1.0\n" ⧺
                 "-1.0 0.9 -2.0 0.0\n" ⧺
                 "1.0 -1.0  2.0 3.0 1.0\n\n"

          tableauExpected = makeTableau
            4
            3
            [ [ 0,  0, -1, -2]
            , [ 1, -1,  0, -1]
            , [-1,  9%10, -2,  0]
            ]
            [3%2, 3, 0]
            [-1, 2, 3, 1]
            1
            [1,3,6]
            [2,4,5,7]

          result = parseDirectTableau text
          Right tableau = result

      in do
        isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
        tableau @?= tableauExpected


  trailingSpaces ∷ TestTree
  trailingSpaces =
    testCase "Trailing spaces" $
      let text = "3 4\n" ⧺
                 "1 3 6 \n" ⧺
                 "2 4 5 7\n" ⧺
                 "1.5 3.0 0.0\n" ⧺
                 "0.0 0.0 -1.0 -2.0 \n" ⧺
                 "1.0 -1.0 0.0 -1.0 \n" ⧺
                 "-1.0 0.9 -2.0 0.0\n" ⧺
                 "1.0 -1.0  2.0 3.0 1.0"

          tableauExpected = makeTableau
            4
            3
            [ [ 0,  0, -1, -2]
            , [ 1, -1,  0, -1]
            , [-1,  9%10, -2,  0]
            ]
            [3%2, 3, 0]
            [-1, 2, 3, 1]
            1
            [1,3,6]
            [2,4,5,7]

          result = parseDirectTableau text
          Right tableau = result

      in do
        isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
        tableau @?= tableauExpected


  largeTableau ∷ TestTree
  largeTableau =
    testCase "Large tableau" $
      let text = "19 12\n" ⧺
                 "13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31\n" ⧺
                 "1 2 3 4 5 6 7 8 9 10 11 12\n" ⧺
                 "0.2 0.7 6.3 8.3 5.6 7.9 6.1 1.3 3.9 2.6 8.2 9.0 0.1 6.8 7.6 9.8 3.0 9.6 9.7\n" ⧺
                 "0.3 -7.0 7.6 4.1 -0.5 3.4 1.3 5.2 1.5 -7.9 -2.7 2.4\n" ⧺
                 "-4.9 -5.5 0.6 1.8 -8.0 2.9 -5.8 -2.3 1.2 0.6 -7.8 6.8\n" ⧺
                 "-2.3 6.8 7.6 -6.1 -3.1 -1.2 -1.7 -6.9 1.5 -6.7 7.2 -0.9\n" ⧺
                 "-7.1 1.0 -3.6 0.7 3.7 2.9 -6.5 6.6 -2.9 4.7 -2.0 -2.3\n" ⧺
                 "5.7 6.9 -2.8 5.1 4.2 4.3 -7.4 -2.1 7.0 6.9 5.4 -5.0\n" ⧺
                 "0.5 -7.5 5.3 4.4 -0.9 -1.0 0.7 3.1 4.6 2.8 6.0 3.0\n" ⧺
                 "4.4 2.1 -4.6 7.7 -5.0 2.7 1.9 -4.1 -0.6 -1.8 -3.6 -3.7\n" ⧺
                 "-5.9 -4.7 0.9 -3.1 1.3 -5.6 -3.6 -5.4 -6.9 4.8 -3.8 5.6\n" ⧺
                 "-4.4 4.3 1.1 3.6 -4.0 -5.0 1.2 7.0 2.4 -7.6 -1.1 -4.3\n" ⧺
                 "7.4 -6.3 7.8 -3.8 -6.8 4.0 -7.8 -4.0 4.0 -0.0 -1.8 6.0\n" ⧺
                 "7.9 1.0 -1.7 6.5 7.6 -2.9 -6.4 4.5 -5.6 -1.0 -1.4 -7.5\n" ⧺
                 "-6.9 -5.2 -7.7 0.6 1.6 1.0 3.1 7.9 -0.6 5.5 -4.9 -6.5\n" ⧺
                 "-5.9 2.9 6.6 0.5 2.1 -0.6 3.3 -0.9 6.3 -6.2 0.5 -0.3\n" ⧺
                 "-3.7 2.0 6.5 -2.6 4.9 -0.0 -7.2 -5.1 -6.2 4.2 -1.8 -2.6\n" ⧺
                 "-4.2 1.7 4.2 6.3 0.9 2.7 0.9 0.1 0.3 -3.0 0.3 -6.4\n" ⧺
                 "-0.7 -6.5 4.7 -1.5 -0.8 1.8 -7.7 -4.8 2.8 -7.1 1.2 -3.4\n" ⧺
                 "8.0 -1.5 -7.6 4.1 -5.9 4.1 4.7 -0.7 -7.3 1.9 4.8 -0.5\n" ⧺
                 "-0.9 -0.7 -7.3 -1.3 7.7 -0.0 -1.7 7.2 1.6 -7.0 1.5 5.6\n" ⧺
                 "-7.3 -7.3 4.9 6.9 7.7 -6.3 6.2 -6.1 -6.9 -6.8 5.2 5.9\n" ⧺
                 "0.0 -2.3 2.9 2.3 4.6 -2.7 -1.8 0.9 3.4 -1.6 -4.1 2.9 -4.1\n"

          tableauExpected = makeTableau
            12
            19
            [ [  3 % 10, -70 % 10,  76 % 10,  41 % 10, - 5 % 10,  34 % 10,  13 % 10,  52 % 10,  15 % 10, -79 % 10, -27 % 10,  24 % 10]
            , [-49 % 10, -55 % 10,   6 % 10,  18 % 10, -80 % 10,  29 % 10, -58 % 10, -23 % 10,  12 % 10,   6 % 10, -78 % 10,  68 % 10]
            , [-23 % 10,  68 % 10,  76 % 10, -61 % 10, -31 % 10, -12 % 10, -17 % 10, -69 % 10,  15 % 10, -67 % 10,  72 % 10, - 9 % 10]
            , [-71 % 10,  10 % 10, -36 % 10,   7 % 10,  37 % 10,  29 % 10, -65 % 10,  66 % 10, -29 % 10,  47 % 10, -20 % 10, -23 % 10]
            , [ 57 % 10,  69 % 10, -28 % 10,  51 % 10,  42 % 10,  43 % 10, -74 % 10, -21 % 10,  70 % 10,  69 % 10,  54 % 10, -50 % 10]
            , [  5 % 10, -75 % 10,  53 % 10,  44 % 10, - 9 % 10, -10 % 10,   7 % 10,  31 % 10,  46 % 10,  28 % 10,  60 % 10,  30 % 10]
            , [ 44 % 10,  21 % 10, -46 % 10,  77 % 10, -50 % 10,  27 % 10,  19 % 10, -41 % 10, - 6 % 10, -18 % 10, -36 % 10, -37 % 10]
            , [-59 % 10, -47 % 10,   9 % 10, -31 % 10,  13 % 10, -56 % 10, -36 % 10, -54 % 10, -69 % 10,  48 % 10, -38 % 10,  56 % 10]
            , [-44 % 10,  43 % 10,  11 % 10,  36 % 10, -40 % 10, -50 % 10,  12 % 10,  70 % 10,  24 % 10, -76 % 10, -11 % 10, -43 % 10]
            , [ 74 % 10, -63 % 10,  78 % 10, -38 % 10, -68 % 10,  40 % 10, -78 % 10, -40 % 10,  40 % 10, - 0 % 10, -18 % 10,  60 % 10]
            , [ 79 % 10,  10 % 10, -17 % 10,  65 % 10,  76 % 10, -29 % 10, -64 % 10,  45 % 10, -56 % 10, -10 % 10, -14 % 10, -75 % 10]
            , [-69 % 10, -52 % 10, -77 % 10,   6 % 10,  16 % 10,  10 % 10,  31 % 10,  79 % 10, - 6 % 10,  55 % 10, -49 % 10, -65 % 10]
            , [-59 % 10,  29 % 10,  66 % 10,   5 % 10,  21 % 10, - 6 % 10,  33 % 10, - 9 % 10,  63 % 10, -62 % 10,   5 % 10, - 3 % 10]
            , [-37 % 10,  20 % 10,  65 % 10, -26 % 10,  49 % 10, - 0 % 10, -72 % 10, -51 % 10, -62 % 10,  42 % 10, -18 % 10, -26 % 10]
            , [-42 % 10,  17 % 10,  42 % 10,  63 % 10,   9 % 10,  27 % 10,   9 % 10,   1 % 10,   3 % 10, -30 % 10,   3 % 10, -64 % 10]
            , [- 7 % 10, -65 % 10,  47 % 10, -15 % 10, - 8 % 10,  18 % 10, -77 % 10, -48 % 10,  28 % 10, -71 % 10,  12 % 10, -34 % 10]
            , [ 80 % 10, -15 % 10, -76 % 10,  41 % 10, -59 % 10,  41 % 10,  47 % 10, - 7 % 10, -73 % 10,  19 % 10,  48 % 10, - 5 % 10]
            , [- 9 % 10, - 7 % 10, -73 % 10, -13 % 10,  77 % 10, - 0 % 10, -17 % 10,  72 % 10,  16 % 10, -70 % 10,  15 % 10,  56 % 10]
            , [-73 % 10, -73 % 10,  49 % 10,  69 % 10,  77 % 10, -63 % 10,  62 % 10, -61 % 10, -69 % 10, -68 % 10,  52 % 10,  59 % 10]
            ]
            [ 2 % 10, 7 % 10,63 % 10,83 % 10,56 % 10,79 % 10,61 % 10,13 % 10,39 % 10,26 % 10,82 % 10,90 % 10, 1 % 10,68 % 10,76 % 10,98 % 10,30 % 10,96 % 10,97 % 10]
            [-23 % 10,29 % 10,23 % 10,46 % 10,-27 % 10,-18 % 10, 9 % 10,34 % 10,-16 % 10,-41 % 10,29 % 10,-41 % 10]
            0
            [13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
            [1,2,3,4,5,6,7,8,9,10,11,12]

          result = parseDirectTableau text
          Right tableau = result

      in do
        isRight result @? ("Parsing failed: " ⧺ (show ∘ fromLeft $ result))
        tableau @?= tableauExpected
