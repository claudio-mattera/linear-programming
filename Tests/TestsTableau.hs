{-# LANGUAGE UnicodeSyntax #-}

module TestsTableau (tests) where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Test.QuickCheck

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Ratio

import LinearProgramming.Tableau

tests ∷ TestTree
tests = testGroup "Tableau" $
        [ testGroup "Examples from Linear and Integer Programming course" $
            map makeTestsFromSample samples
        ,  qcProps
        ]

qcProps ∷ TestTree
qcProps = testGroup "Properties"
  [ QC.testProperty "Tableau with all non-negative bs are feasible" $
      \feasibleTableau →
        let t@Tableau{tabB = b} = unFeasibleTableau feasibleTableau
        in V.all (≥ 0) b ==> isFeasible t
  , QC.testProperty "Tableau with negative bs are infeasible" $
      \t@Tableau{tabB = b} → V.any (< 0) b ==> not (isFeasible t)
  , QC.testProperty "Tableau with negative cs are non final" $
      \t@Tableau{tabC = c} → V.any (> 0) c ==> not (isFinal t)
  , QC.testProperty "Pivoting does not decrease objective" $
      \feasibleTableau →
        let tableau@Tableau{tabZ = z} = unFeasibleTableau feasibleTableau
        in isFeasible tableau ∧ not (isFinal tableau) ==>
          let Just entering = chooseEnteringVariable tableau
          in case chooseLeavingVariable tableau entering of
            Nothing → True -- Accept if the tableau is unbounded
            Just leaving →
              let Tableau {tabZ = z'} = pivot tableau entering leaving
              in z' ≥ z
  ]


type Sample = (String, Tableau, Tableau, Variable, Variable)

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
    , tabAuxiliaryData = Nothing
    }

      tExpected = Tableau {
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
    , tabAuxiliaryData = Nothing
    }

      entering = 2
      leaving = 3

  in (desc, tInitial, tExpected, entering, leaving)


sample2 ∷ Sample
sample2 =
  let desc = "Lecture \"Pivoting\""

      tInitial = Tableau {
      tabN = 3
    , tabM = 3
    , tabA = M.fromLists [ [-2, -3, -1]
                         , [-4, -1, -2]
                         , [-3, -4, -2]
                         ]
    , tabB = V.fromList [5, 11, 8]
    , tabC = V.fromList [5, 4, 3]
    , tabZ = 0
    , tabBasicVariables = V.fromList [4,5,6]
    , tabIndependantVariables = V.fromList [1,2,3]
    , tabAuxiliaryData = Nothing
    }

      tExpected = Tableau {
      tabN = 3
    , tabM = 3
    , tabA = M.fromLists [ [-1%2, -3%2, -1%2]
                         , [ 2,    5,    0]
                         , [ 3%2,  1%2, -1%2]
                         ]
    , tabB = V.fromList [5%2, 1, 1%2]
    , tabC = V.fromList [-5%2, -7%2, 1%2]
    , tabZ = 25%2
    , tabBasicVariables = V.fromList [1,5,6]
    , tabIndependantVariables = V.fromList [4,2,3]
    , tabAuxiliaryData = Nothing
    }

      entering = 1
      leaving = 4

  in (desc, tInitial, tExpected, entering, leaving)

sample3 ∷ Sample
sample3 =
  let desc = "Lecture \"Infeasible problem example\""

      tInitial = Tableau {
      tabN = 4
    , tabM = 4
    , tabA = M.fromLists [ [ 1, -1,  1,  0]
                         , [ 1,  0, -1, -1]
                         , [ 1,  1,  0, -1]
                         , [ 1,  0,  0,  1]
                         ]
    , tabB = V.fromList [5, 14, -6, -7]
    , tabC = V.fromList [-1, 0, 0, 0]
    , tabZ = 0
    , tabBasicVariables = V.fromList [4,5,6,7]
    , tabIndependantVariables = V.fromList [0,1,2,3]
    , tabAuxiliaryData = Nothing
    }

      tExpected = Tableau {
      tabN = 4
    , tabM = 4
    , tabA = M.fromLists [ [ 1, -1,  1, -1]
                         , [ 1,  0, -1, -2]
                         , [ 1,  1,  0, -2]
                         , [ 1,  0,  0, -1]
                         ]
    , tabB = V.fromList [12, 21, 1, 7]
    , tabC = V.fromList [-1, 0, 0, 1]
    , tabZ = -7
    , tabBasicVariables = V.fromList [4,5,6,0]
    , tabIndependantVariables = V.fromList [7,1,2,3]
    , tabAuxiliaryData = Nothing
    }

      entering = 0
      leaving = 7

  in (desc, tInitial, tExpected, entering, leaving)

sample4 ∷ Sample
sample4 =
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
      , tabAuxiliaryData = Nothing
      }

      tExpected = Tableau {
        tabN = 3
      , tabM = 4
      , tabA = M.fromLists [ [ 0, -1,  0]
                           , [ 1,  0, -1]
                           , [ 2,  0, -1]
                           , [ 1, -1,  0]
                           ]
      , tabB = V.fromList [9,6,2,4]
      , tabC = V.fromList [5,-3,-5]
      , tabZ = 12
      , tabBasicVariables = V.fromList [4,5,6,2]
      , tabIndependantVariables = V.fromList [1,7,3]
      , tabAuxiliaryData = Nothing
      }

      entering = 2
      leaving = 7

  in (desc, tInitial, tExpected, entering, leaving)


samples ∷ [Sample]
samples =
  [ sample1
  , sample2
  , sample3
  , sample4
  ]

makeTestsFromSample ∷ Sample → TestTree
makeTestsFromSample (desc, tInitial, tExpected, entering, leaving) =
  testGroup desc
          [ testObjectiveUpdate
          , testBJ
          , testB
          , testRowJ
          , testA
          , testC
          , testPivot
          ]
  where

  tFinal = pivot tInitial entering leaving

  testPivot =
    testCase "Simple pivot" $
      tFinal @?= tExpected

  testA =
    testCase "A update" $
      let a = tabA tFinal
          aExpected = tabA tExpected

      in a @?= aExpected

  testRowJ =
    testCase "Row j update" $
      let a = tabA tFinal
          aExpected = tabA tExpected
          aj = M.getRow 1 a
          ajExpected = M.getRow 1 aExpected

      in aj @?= ajExpected

  testB =
    testCase "B update" $
      let b = tabB tFinal
          bExpected = tabB tExpected

      in b @?= bExpected

  testC =
    testCase "C update" $
      let c = tabC tFinal
          cExpected = tabC tExpected

      in c @?= cExpected


  testBJ =
    testCase "Bj update" $
      let b = tabB tFinal
          bExpected = tabB tExpected
          bj = b V.! 0
          bjExpected = bExpected V.! 0

      in bj @?= bjExpected


  testObjectiveUpdate =
    testCase "Objective value update" $
      let z = tabZ tFinal
          zExpected = tabZ tExpected
      in z @?= zExpected




arbitraryTableau ∷ Int → Gen Tableau
arbitraryTableau size = do
  n ← choose (2, size)
  m ← choose (2, size)
  as ← vector (m*n)
  bs ← vector m
  cs ← vector n
  z ← arbitrary
  let t = Tableau {
    tabN = n
  , tabM = m
  , tabA = M.fromList m n as
  , tabB = V.fromList bs
  , tabC = V.fromList cs
  , tabZ = z
  , tabBasicVariables = V.fromList [n+1 .. n+m]
  , tabIndependantVariables = V.fromList [1..n]
  , tabAuxiliaryData = Nothing
  }
  return t

instance Arbitrary Tableau where
  arbitrary = sized arbitraryTableau


newtype FeasibleTableau = FeasibleTableau { unFeasibleTableau ∷ Tableau } deriving (Eq, Show)

instance Arbitrary FeasibleTableau where
  arbitrary = do
    tableau ← arbitrary
    let b' = V.map abs $ tabB tableau
    return (FeasibleTableau tableau {
      tabB = b'
    })
