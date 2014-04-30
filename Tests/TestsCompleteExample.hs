{-# LANGUAGE UnicodeSyntax #-}

module TestsCompleteExample (tests) where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.HUnit

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

text ∷ String
text = "max x1 + 2 x2\n" ⧺
       "\n" ⧺
       "-3 x1 + x2 <= 2\n" ⧺
       "x2 <= 11\n" ⧺
       "x1 + -1 x2 <= 3\n" ⧺
       "x1 <= 6\n"


t0 ∷ Tableau
t0 = makeTableau
      2
      4
      [ [ 3, -1]
      , [ 0, -1]
      , [-1,  1]
      , [-1,  0]
      ]
      [2, 11, 3, 6]
      [1, 2]
      0
      [3,4,5,6]
      [1,2]


e0, l0 ∷ Int
e0 = 2
l0 = 3


t1 ∷ Tableau
t1 = makeTableau
      2
      4
      [ [ 3, -1]
      , [-3,  1]
      , [ 2, -1]
      , [-1,  0]
      ]
      [2, 9, 5, 6]
      [7, -2]
      4
      [2,4,5,6]
      [1,3]

e1, l1 ∷ Int
e1 = 1
l1 = 4


t2 ∷ Tableau
t2 = makeTableau
      2
      4
      [ [-1,    0]
      , [-1%3,  1%3]
      , [-2%3, -1%3]
      , [ 1%3, -1%3]
      ]
      [11,3,11,3]
      [-7%3, 1%3]
      25
      [2,1,5,6]
      [4,3]

e2, l2 ∷ Int
e2 = 3
l2 = 6


t3 ∷ Tableau
t3 = makeTableau
      2
      4
      [ [-1,  0]
      , [ 0, -1]
      , [-1,  1]
      , [ 1, -3]
      ]
      [11,6,8,9]
      [-2,-1]
      28
      [2,1,5,3]
      [4,6]


testParser ∷ TestTree
testParser =
  testCase "Parsing the problem" $
    let result = parseTableau text
        Right t = result
        Left err = result
    in do
      isRight result @? ("Parsing failed: " ⧺ (show err))
      t @?= t0

testPivot1 ∷ TestTree
testPivot1 =
  testCase "Pivoting - 1st step" $
    let t = pivot t0 e0 l0
    in t @?= t1

testPivot2 ∷ TestTree
testPivot2 =
  testCase "Pivoting - 2nd step" $
    let t = pivot t1 e1 l1
    in t @?= t2

testPivot3 ∷ TestTree
testPivot3 =
  testCase "Pivoting - 3rd step" $
    let t = pivot t2 e2 l2
    in t @?= t3

testFeasible0 ∷ TestTree
testFeasible0 =
  testCase "1st tableau is feasible" $
    isFeasible t0 @?= True

testFeasible1 ∷ TestTree
testFeasible1 =
  testCase "2nd tableau is feasible" $
    isFeasible t1 @?= True

testFeasible2 ∷ TestTree
testFeasible2 =
  testCase "3rd tableau is feasible" $
    isFeasible t2 @?= True

testFeasible3 ∷ TestTree
testFeasible3 =
  testCase "Last tableau is feasible" $
    isFeasible t3 @?= True

testFinal0 ∷ TestTree
testFinal0 =
  testCase "1st tableau is not final" $
    isFinal t0 @?= False

testFinal1 ∷ TestTree
testFinal1 =
  testCase "2nd tableau is not final" $
    isFinal t1 @?= False

testFinal2 ∷ TestTree
testFinal2 =
  testCase "3rd tableau is not final" $
    isFinal t2 @?= False

testFinal3 ∷ TestTree
testFinal3 =
  testCase "Last tableau is final" $
    isFinal t3 @?= True
