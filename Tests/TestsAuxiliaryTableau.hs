{-# LANGUAGE UnicodeSyntax #-}

module TestsAuxiliaryTableau (tests) where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Ratio

import LinearProgramming.Tableau

tests ∷ TestTree
tests = testGroup "Auxiliary Tableau" $
        [ testGroup "Examples from Linear and Integer Programming course" $
            map makeTestsFromSample samples
        ]

type Sample = (String, Tableau, Tableau, Variable, Variable, Tableau)

sample1 ∷ Sample
sample1 =
  let desc = "Lecture \"Infeasible Problem Example\""

      tInitial = Tableau {
      tabN = 3
    , tabM = 4
    , tabA = M.fromLists [ [-1,  1,  0]
                         , [ 0, -1, -1]
                         , [ 1,  0, -1]
                         , [ 0,  0,  1]
                         ]
    , tabB = V.fromList [5,14,-6,-7]
    , tabC = V.fromList [1,2,-1]
    , tabZ = 0
    , tabBasicVariables = V.fromList [4,5,6,7]
    , tabIndependantVariables = V.fromList [1,2,3]
    }

      tAuxiliaryExpected = Tableau {
      tabN = 4
    , tabM = 4
    , tabA = M.fromLists [ [ 1, -1,  1,  0]
                         , [ 1,  0, -1, -1]
                         , [ 1,  1,  0, -1]
                         , [ 1,  0,  0,  1]
                         ]
    , tabB = V.fromList [5,14,-6,-7]
    , tabC = V.fromList [-1,0,0,0]
    , tabZ = 0
    , tabBasicVariables = V.fromList [4,5,6,7]
    , tabIndependantVariables = V.fromList [0,1,2,3]
    }

      entering = 0
      leaving = 7

      tAuxiliaryInitialExpected = Tableau {
      tabN = 4
    , tabM = 4
    , tabA = M.fromLists [ [ 1, -1,  1, -1]
                         , [ 1,  0, -1, -2]
                         , [ 1,  1,  0, -2]
                         , [ 1,  0,  0, -1]
                         ]
    , tabB = V.fromList [12,21,1,7]
    , tabC = V.fromList [-1,0,0,1]
    , tabZ = -7
    , tabBasicVariables = V.fromList [4,5,6,0]
    , tabIndependantVariables = V.fromList [7,1,2,3]
    }

  in (desc, tInitial, tAuxiliaryExpected, entering, leaving, tAuxiliaryInitialExpected)

sample2 ∷ Sample
sample2 =
  let desc = "Lecture \"The Auxiliary Problem\""

      tInitial = Tableau {
      tabN = 2
    , tabM = 4
    , tabA = M.fromLists [ [ 2, -1]
                         , [ 0, -1]
                         , [-1,  2]
                         , [-1,  0]
                         ]
    , tabB = V.fromList [-2,4,-2,4]
    , tabC = V.fromList [1,2]
    , tabZ = 0
    , tabBasicVariables = V.fromList [3,4,5,6]
    , tabIndependantVariables = V.fromList [1,2]
    }

      tAuxiliaryExpected = Tableau {
      tabN = 3
    , tabM = 4
    , tabA = M.fromLists [ [ 1,  2, -1]
                         , [ 1,  0, -1]
                         , [ 1, -1,  2]
                         , [ 1, -1,  0]
                         ]
    , tabB = V.fromList [-2,4,-2,4]
    , tabC = V.fromList [-1,0,0]
    , tabZ = 0
    , tabBasicVariables = V.fromList [3,4,5,6]
    , tabIndependantVariables = V.fromList [0,1,2]
    }

      entering = 0
      leaving = 3

      tAuxiliaryInitialExpected = Tableau {
      tabN = 3
    , tabM = 4
    , tabA = M.fromLists [ [ 1, -2,  1]
                         , [ 1, -2,  0]
                         , [ 1, -3,  3]
                         , [ 1, -3,  1]
                         ]
    , tabB = V.fromList [2,6,0,6]
    , tabC = V.fromList [-1,2,-1]
    , tabZ = -2
    , tabBasicVariables = V.fromList [0,4,5,6]
    , tabIndependantVariables = V.fromList [3,1,2]
    }

  in (desc, tInitial, tAuxiliaryExpected, entering, leaving, tAuxiliaryInitialExpected)


samples ∷ [Sample]
samples =
  [ sample1
  , sample2
  ]

makeTestsFromSample ∷ Sample → TestTree
makeTestsFromSample (desc, tInitial, tAuxiliaryExpected, entering, leaving, tAuxiliaryInitialExpected) =
  testGroup desc
          [ testAuxiliaryTableau
          , testForcedPivot
          ]
  where

  tAuxiliary = generateAuxiliaryTableau tInitial
  tAuxiliaryInitial = pivot tAuxiliary entering leaving

  testAuxiliaryTableau =
    testCase "Auxiliary tableau" $
      tAuxiliary @?= tAuxiliaryExpected

  testForcedPivot =
    testCase "Forced pivot" $
      tAuxiliaryInitial @?= tAuxiliaryInitialExpected
