{-# LANGUAGE UnicodeSyntax #-}

module TestsAuxiliaryTableau (tests) where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Ratio

import LinearProgramming.Tableau
import LinearProgramming.AuxiliaryTableau

tests ∷ TestTree
tests = testGroup "Auxiliary Tableau" $
        [ testGroup "Examples from Linear and Integer Programming course" $
            map makeTestsFromSample samples
        , completeExample
        ]

type Sample = (String, Tableau, AuxiliaryTableau, Variable, Variable, AuxiliaryTableau)

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

      tAuxiliaryExpected = AuxiliaryTableau {
        auxTableau = Tableau {
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
      , auxZ = 0
      , auxC = V.fromList [0,1,2,-1]
      }


      entering = 0
      leaving = 7

      tAuxiliaryInitialExpected = AuxiliaryTableau {
        auxTableau = Tableau {
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
      , auxZ = 0
      , auxC = V.fromList [0,1,2,-1]
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

      tAuxiliaryExpected = AuxiliaryTableau {
        auxTableau = Tableau {
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
      , auxZ = 0
      , auxC = V.fromList [0,1,2]
      }

      entering = 0
      leaving = 3

      tAuxiliaryInitialExpected = AuxiliaryTableau {
        auxTableau = Tableau {
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
      , auxZ = 0
      , auxC = V.fromList [0,1,2]
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
  tAuxiliaryInitial = auxiliaryPivot tAuxiliary entering leaving

  testAuxiliaryTableau =
    testCase "Auxiliary tableau" $
      tAuxiliary @?= tAuxiliaryExpected

  testForcedPivot =
    testCase "Forced pivot" $
      tAuxiliaryInitial @?= tAuxiliaryInitialExpected



completeExample =
  -- Note: In the video lecture there are few errors. These tableaus are correct.
  testGroup "Complete example from \"Pivoting the Auxiliary Dictionary\"" $
    [ testAuxiliary
    , testPivot1
    , testPivot2
    , testPivot3
    , testFeasibleTableau
    ]

  where

  testAuxiliary =
    testCase "Computing the auxiliary tableau" $
      let t = generateAuxiliaryTableau t0
      in t @?= tAux0

  testPivot1 =
    testCase "Pivoting - 1st step" $
      let t = auxiliaryPivot tAux0 e0 l0
      in t @?= tAux1

  testPivot2 =
    testCase "Pivoting - 2nd step" $
      let t = auxiliaryPivot tAux1 e1 l1
      in t @?= tAux2

  testPivot3 =
    testCase "Pivoting - 3rd step" $
      let t = auxiliaryPivot tAux2 e2 l2
      in t @?= tAux3

  testFeasibleTableau =
    testCase "Computing the feasible tableau for the original problem" $
      let t = toOriginalTableau tAux3
      in t @?= tf


  t0 = Tableau {
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

  tAux0 = AuxiliaryTableau {
    auxTableau = Tableau {
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
  , auxZ = 0
  , auxC =  V.fromList [0,1,2]
  }

  e0 = 0
  l0 = 3

  tAux1 = AuxiliaryTableau {
    auxTableau = Tableau {
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
  , auxZ = 0
  , auxC = V.fromList [0,1,2]
  }

  e1 = 1
  l1 = 5

  tAux2 = AuxiliaryTableau {
    auxTableau = Tableau {
      tabN = 3
    , tabM = 4
    , tabA = M.fromLists [ [ 1%3,  2%3, -1]
                         , [ 1%3,  2%3, -2]
                         , [ 1%3, -1%3,  1]
                         , [ 0,    1,   -2]
                         ]
    , tabB = V.fromList [2,6,0,6]
    , tabC = V.fromList [-1%3,-2%3,1]
    , tabZ = -2
    , tabBasicVariables = V.fromList [0,4,1,6]
    , tabIndependantVariables = V.fromList [3,5,2]
    }
  , auxZ = 0
  , auxC = V.fromList [1%3,-1%3,3%1]
  }

  e2 = 2
  l2 = 0

  tAux3 = AuxiliaryTableau {
    auxTableau = Tableau {
      tabN = 3
    , tabM = 4
    , tabA = M.fromLists [ [ 1%3,  2%3, -1]
                         , [-1%3, -2%3,  2]
                         , [ 2%3,  1%3, -1]
                         , [-2%3, -1%3,  2]
                         ]
    , tabB = V.fromList [2,2,2,2]
    , tabC = V.fromList [0,0,-1]
    , tabZ = 0
    , tabBasicVariables = V.fromList [2,4,1,6]
    , tabIndependantVariables = V.fromList [3,5,0]
    }
  , auxZ = 6
  , auxC = V.fromList [4%3, 5%3, -3]
  }

  tf = Tableau {
    tabN = 2
  , tabM = 4
  , tabA = M.fromLists [ [ 1%3,  2%3]
                       , [-1%3, -2%3]
                       , [ 2%3,  1%3]
                       , [-2%3, -1%3]
                       ]
  , tabB = V.fromList [2,2,2,2]
  , tabC = V.fromList [4%3, 5%3]
  , tabZ = 6
  , tabBasicVariables = V.fromList [2,4,1,6]
  , tabIndependantVariables = V.fromList [3,5]
  }
