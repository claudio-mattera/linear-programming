{-# LANGUAGE UnicodeSyntax #-}

module TestsTableau (tests) where

import Prelude hiding (all, any)
import Prelude.Unicode

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Test.QuickCheck

import Data.Matrix
import Data.Ratio
import Data.Foldable

import LinearProgramming.Tableau

tests ∷ TestTree
tests = testGroup "Tableau"
          [ unitTests
          , qcProps
          ]

unitTests ∷ TestTree
unitTests = testGroup "Unit tests"
          [ testPivot
          , testFeasible
          , testObjectiveUpdate
          ]

qcProps ∷ TestTree
qcProps = testGroup "Properties"
  [ QC.testProperty "Tableau with all non-negative bs are infeasible" $
      \t@Tableau{tabB = b} → all (≥ 0) b ==> isFeasible t
  , QC.testProperty "Tableau with negative bs are infeasible" $
      \t@Tableau{tabB = b} → any (< 0) b ==> not (isFeasible t)
  , QC.testProperty "Tableau with all non-negative cs are optimal" $
      \t@Tableau{tabC = c} → all (≥ 0) c ==> isOptimal t
  , QC.testProperty "Tableau with negative cs are suboptimal" $
      \t@Tableau{tabC = c} → any (< 0) c ==> not (isOptimal t)
  , QC.testProperty "Pivot does not decrease objective" $
      \tableau@Tableau{tabZ = z} →
        isFeasible tableau ∧ not (isOptimal tableau) ==>
          let entering = chooseEnteringVariable tableau
              leaving = chooseLeavingVariable tableau entering
              Tableau {tabZ = z'} = pivot tableau entering leaving
          in z' ≤ z
  ]

testPivot =
  let tInitial = Tableau {
    tabN = 3
  , tabM = 6
  , tabA = fromLists [ [2,1,1,1,0,0]
                    , [4,2,3,0,1,0]
                    , [2,5,5,0,0,1]]
  , tabB = fromLists [[14],[28],[30]]
  , tabC = fromLists [[-1,-2,1,0,0,0]]
  , tabZ = 0
  , tabBasicVariables = [4,5,6]
  , tabIndependantVariables = [1,2,3]
  }

      tExpected = Tableau {
    tabN = 3
  , tabM = 6
  , tabA = fromLists [ [8%5,0,0,1,0,-1%5]
                    , [16%5,0,1,0,1,-2%5]
                    , [2%5,1,1,0,0,1%5]]
  , tabB = fromLists [[8],[16],[6]]
  , tabC = fromLists [[-1%5,0,3,0,0,2%5]]
  , tabZ = 12
  , tabBasicVariables = [4,5,6]
  , tabIndependantVariables = [1,2,3]
  }

      tFinal = pivot tInitial 2 3

  in testCase "Simple pivot" $
    tFinal @?= tExpected


testFeasible =
  let t = Tableau {
    tabN = 3
  , tabM = 6
  , tabA = fromLists [ [2,1,1,1,0,0]
                    , [4,2,3,0,1,0]
                    , [2,5,5,0,0,1]]
  , tabB = fromLists [[14],[28],[30]]
  , tabC = fromLists [[-1,-2,1,0,0,0]]
  , tabZ = 0
  , tabBasicVariables = [4,5,6]
  , tabIndependantVariables = [1,2,3]
  }

  in testCase "Feasible tableau" $
    isFeasible t @?= True

testObjectiveUpdate =
  let tInitial = Tableau {
    tabN = 5
  , tabM = 2
  , tabA = fromLists [ [1%2,2,1,1,0]
                     , [1,  2,4,0,1]]
  , tabB = fromLists [[24],[60]]
  , tabC = fromLists [[6,14,13,0,0]]
  , tabZ = 0
  , tabBasicVariables = [6,7]
  , tabIndependantVariables = [1,2,3,4,5]
  }

      tFinal = pivot tInitial 2 4

  in testCase "Objective value update" $
    tabZ tFinal @?= 168


arbitraryTableau ∷ Int → Gen Tableau
arbitraryTableau size = do
  n ← choose (0, size)
  m ← choose (0, size)
  as ← vector (m*n)
  bs ← vector m
  cs ← vector n
  z ← arbitrary
  let t = Tableau {
    tabN = n
  , tabM = m
  , tabA = fromList m n as
  , tabB = fromList m 1 bs
  , tabC = fromList 1 n cs
  , tabZ = z
  , tabBasicVariables = []
  , tabIndependantVariables = []
  }
  return t

instance Arbitrary Tableau where
  arbitrary = sized arbitraryTableau
