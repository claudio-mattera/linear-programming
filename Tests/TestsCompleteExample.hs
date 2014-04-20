{-# LANGUAGE UnicodeSyntax #-}

module TestsCompleteExample (tests) where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Ratio

import LinearProgramming.Tableau
import LinearProgramming.Parser

isRight ∷ Either a b → Bool
isRight (Right _) = True
isRight (Left _) = False

tests ∷ TestTree
tests = testGroup "Complete example from LP course"
          [ testParser
          , testPivot1
          , testPivot2
          , testPivot3
          , testFeasible0
          , testFeasible1
          , testFeasible2
          , testFeasible3
          , testFinal0
          , testFinal1
          , testFinal2
          , testFinal3
          ]


text = "max x1 + 2 x2\n" ⧺
       "\n" ⧺
       "-3 x1 + x2 <= 2\n" ⧺
       "x2 <= 11\n" ⧺
       "x1 + -1 x2 <= 3\n" ⧺
       "x1 <= 6\n"

t0 = Tableau {
  tabN = 2
, tabM = 4
, tabA = M.fromLists [ [ 3, -1]
                     , [ 0, -1]
                     , [-1,  1]
                     , [-1,  0]
                     ]
, tabB = V.fromList [2, 11, 3, 6]
, tabC = V.fromList [1, 2]
, tabZ = 0
, tabBasicVariables = V.fromList [3,4,5,6]
, tabIndependantVariables = V.fromList [1,2]
}


e0 = 2
l0 = 3


t1 = Tableau {
  tabN = 2
, tabM = 4
, tabA = M.fromLists [ [ 3, -1]
                     , [-3,  1]
                     , [ 2, -1]
                     , [-1,  0]
                     ]
, tabB = V.fromList [2, 9, 5, 6]
, tabC = V.fromList [7, -2]
, tabZ = 4
, tabBasicVariables = V.fromList [2,4,5,6]
, tabIndependantVariables = V.fromList [1,3]
}

e1 = 1
l1 = 4


t2 = Tableau {
  tabN = 2
, tabM = 4
, tabA = M.fromLists [ [-1,    0]
                     , [-1%3,  1%3]
                     , [-2%3, -1%3]
                     , [ 1%3, -1%3]
                     ]
, tabB = V.fromList [11,3,11,3]
, tabC = V.fromList [-7%3, 1%3]
, tabZ = 25
, tabBasicVariables = V.fromList [2,1,5,6]
, tabIndependantVariables = V.fromList [4,3]
}

e2 = 3
l2 = 6


t3 = Tableau {
  tabN = 2
, tabM = 4
, tabA = M.fromLists [ [-1,  0]
                     , [ 0, -1]
                     , [-1,  1]
                     , [ 1, -3]
                     ]
, tabB = V.fromList [11,6,8,9]
, tabC = V.fromList [-2,-1]
, tabZ = 28
, tabBasicVariables = V.fromList [2,1,5,3]
, tabIndependantVariables = V.fromList [4,6]
}


testParser =
  testCase "Parsing the problem" $
    let result = parseTableau text
        Right t = result
        Left err = result
    in do
      isRight result @? ("Parsing failed: " ⧺ (show err))
      t @?= t0

testPivot1 =
  testCase "Pivoting - 1st step" $
    let t = pivot t0 e0 l0
    in t @?= t1

testPivot2 =
  testCase "Pivoting - 2nd step" $
    let t = pivot t1 e1 l1
    in t @?= t2

testPivot3 =
  testCase "Pivoting - 3rd step" $
    let t = pivot t2 e2 l2
    in t @?= t3

testFeasible0 =
  testCase "1st tableau is feasible" $
    isFeasible t0 @?= True

testFeasible1 =
  testCase "2nd tableau is feasible" $
    isFeasible t1 @?= True

testFeasible2 =
  testCase "3rd tableau is feasible" $
    isFeasible t2 @?= True

testFeasible3 =
  testCase "Last tableau is feasible" $
    isFeasible t3 @?= True

testFinal0 =
  testCase "1st tableau is not final" $
    isFinal t0 @?= False

testFinal1 =
  testCase "2nd tableau is not final" $
    isFinal t1 @?= False

testFinal2 =
  testCase "3rd tableau is not final" $
    isFinal t2 @?= False

testFinal3 =
  testCase "Last tableau is final" $
    isFinal t3 @?= True
