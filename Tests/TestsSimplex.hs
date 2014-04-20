{-# LANGUAGE UnicodeSyntax #-}

module TestsSimplex (tests) where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Ratio

import LinearProgramming.Tableau
import LinearProgramming.Simplex

isRight ∷ Either a b → Bool
isRight (Right _) = True
isRight (Left _) = False

tests ∷ TestTree
tests = testGroup "Simplex"
          [ testGroup "Examples from Linear and Integer Programming course" $
              map makeTestsFromSample samples
          ]

type Sample = (String, Tableau, Either Error Tableau)

sample1 ∷ Sample
sample1 =
  let desc = "Lecture \"A Complete Example\""

      tInitial = Tableau {
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

      tExpected = Tableau {
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

  in (desc, tInitial, Right tExpected)

sample2 ∷ Sample
sample2 =
  let desc = "Lecture \"Handling Unbounded Problems\""

      tInitial = Tableau {
        tabN = 3
      , tabM = 4
      , tabA = M.fromLists [ [-1,  1,  0]
                           , [ 1,  0, -1]
                           , [ 2,  0, -1]
                           , [ 1, -1,  0]
                           ]
      , tabB = V.fromList [5,6,2,4]
      , tabC = V.fromList [2,3,-5]
      , tabZ = 0
      , tabBasicVariables = V.fromList [4,5,6,7]
      , tabIndependantVariables = V.fromList [1,2,3]
      }

  in (desc, tInitial, Left Unbounded)

samples ∷ [Sample]
samples =
  [ sample1
  , sample2
  ]

makeTestsFromSample ∷ Sample → TestTree
makeTestsFromSample (desc, tInitial, resultExpected) =
  testCase desc $
    let (result, _) = runSimplex tInitial
    in result @?= resultExpected
