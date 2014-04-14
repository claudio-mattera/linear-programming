{-# LANGUAGE UnicodeSyntax #-}

module TestsParser (tests) where

import Prelude hiding (all, any)
import Prelude.Unicode

import Test.Tasty
import Test.Tasty.HUnit

import Data.Matrix as M
import Data.Vector as V

import LinearProgramming.Tableau
import LinearProgramming.Parser

isRight ∷ Either a b → Bool
isRight (Right _) = True
isRight (Left _) = False

tests ∷ TestTree
tests = testGroup "Parser"
          [ simpleMax
          , simpleMin
          , greaterThanOneCoefficients
          , greaterThanOneCoefficientsWithoutMul
          ]

simpleMax =
  testCase "Simple maximization" $
    let text = "max x1 + x2\n" ⧺
               "x1 + x2 + x3 = 6\n" ⧺
               "x2 + x4 = 8\n" ⧺
               "\n"

        tableauExpected = Tableau {
      tabN = 2
    , tabM = 2
    , tabA = fromLists [ [1,1]
                       , [0,1]
                       ]
    , tabB = V.fromList [6,8]
    , tabC = V.fromList [1,1]
    , tabZ = 0
    , tabBasicVariables = V.fromList [3,4]
    , tabIndependantVariables = V.fromList [1,2]
    }

        result = parseTableau text
        Right tableau = result

    in do
      isRight result @? "Parsing failed"
      tableau @?= tableauExpected

simpleMin =
  testCase "Simple minimization" $
    let text = "min x1 + x2\n" ⧺
               "x1 + x2 + x3 = 6\n" ⧺
               "x2 + x4 = 8\n" ⧺
               "\n"

        tableauExpected = Tableau {
      tabN = 2
    , tabM = 2
    , tabA = fromLists [ [1,1]
                       , [0,1]
                       ]
    , tabB = V.fromList [6,8]
    , tabC = V.fromList [-1,-1]
    , tabZ = 0
    , tabBasicVariables = V.fromList [3,4]
    , tabIndependantVariables = V.fromList [1,2]
    }

        result = parseTableau text
        Right tableau = result

    in do
      isRight result @? "Parsing failed"
      tableau @?= tableauExpected

greaterThanOneCoefficients =
  testCase "Greater than one coefficients" $
    let text = "max x1 + 3*x2\n" ⧺
               "x1 + x2 + x3 = 6\n" ⧺
               "9*x2 + x4 = 8\n" ⧺
               "\n"

        tableauExpected = Tableau {
      tabN = 2
    , tabM = 2
    , tabA = fromLists [ [1,1]
                       , [0,9]
                       ]
    , tabB = V.fromList [6,8]
    , tabC = V.fromList [1,3]
    , tabZ = 0
    , tabBasicVariables = V.fromList [3,4]
    , tabIndependantVariables = V.fromList [1,2]
    }

        result = parseTableau text
        Right tableau = result

    in do
      isRight result @? "Parsing failed"
      tableau @?= tableauExpected

greaterThanOneCoefficientsWithoutMul =
  testCase "Greater than one coefficients (no * symbol)" $
    let text = "max x1 + 3 x2\n" ⧺
               "x1 + x2 + x3 = 6\n" ⧺
               "9 x2 + x4 = 8\n" ⧺
               "\n"

        tableauExpected = Tableau {
      tabN = 2
    , tabM = 2
    , tabA = fromLists [ [1,1]
                       , [0,1]
                       ]
    , tabB = V.fromList [6,8]
    , tabC = V.fromList [-1,-1]
    , tabZ = 0
    , tabBasicVariables = V.fromList [3,4]
    , tabIndependantVariables = V.fromList [1,2]
    }

        result = parseTableau text
        Right tableau = result

    in do
      isRight result @? "Parsing failed"
      tableau @?= tableauExpected
