{-# LANGUAGE UnicodeSyntax #-}

module TestsAuxiliaryTableau (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Ratio

import LinearProgramming.Tableau

tests ∷ TestTree
tests = testGroup "Auxiliary Tableau" $
        [ testGroup "Examples from Linear and Integer Programming course" $
            map makeTestsFromSample samples
        , completeExample
        ]

type Sample = (String, Tableau, Tableau, Variable, Variable, Tableau)

sample1 ∷ Sample
sample1 =
  let desc = "Lecture \"Infeasible Problem Example\""

      tInitial = makeTableau
            3
            4
            [ [-1,  1,  0]
            , [ 0, -1, -1]
            , [ 1,  0, -1]
            , [ 0,  0,  1]
            ]
            [5,14,-6,-7]
            [1,2,-1]
            0
            [4,5,6,7]
            [1,2,3]

      tAuxiliaryExpected = makeTableau'
            4
            4
            [ [ 1, -1,  1,  0]
            , [ 1,  0, -1, -1]
            , [ 1,  1,  0, -1]
            , [ 1,  0,  0,  1]
            ]
            [5,14,-6,-7]
            [-1,0,0,0]
            0
            [4,5,6,7]
            [0,1,2,3]
            (Just (0, [0,1,2,-1]))


      entering = 0
      leaving = 7

      tAuxiliaryInitialExpected = makeTableau'
            4
            4
            [ [ 1, -1,  1, -1]
            , [ 1,  0, -1, -2]
            , [ 1,  1,  0, -2]
            , [ 1,  0,  0, -1]
            ]
            [12,21,1,7]
            [-1,0,0,1]
            (-7)
            [4,5,6,0]
            [7,1,2,3]
            (Just (0, [0,1,2,-1]))

  in (desc, tInitial, tAuxiliaryExpected, entering, leaving, tAuxiliaryInitialExpected)

sample2 ∷ Sample
sample2 =
  let desc = "Lecture \"The Auxiliary Problem\""

      tInitial = makeTableau
            2
            4
            [ [ 2, -1]
            , [ 0, -1]
            , [-1,  2]
            , [-1,  0]
            ]
            [-2,4,-2,4]
            [1,2]
            0
            [3,4,5,6]
            [1,2]

      tAuxiliaryExpected = makeTableau'
            3
            4
            [ [ 1,  2, -1]
            , [ 1,  0, -1]
            , [ 1, -1,  2]
            , [ 1, -1,  0]
            ]
            [-2,4,-2,4]
            [-1,0,0]
            0
            [3,4,5,6]
            [0,1,2]
            (Just (0, [0,1,2]))

      entering = 0
      leaving = 3

      tAuxiliaryInitialExpected = makeTableau'
            3
            4
            [ [ 1, -2,  1]
            , [ 1, -2,  0]
            , [ 1, -3,  3]
            , [ 1, -3,  1]
            ]
            [2,6,0,6]
            [-1,2,-1]
            (-2)
            [0,4,5,6]
            [3,1,2]
            (Just (0, [0,1,2]))

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


completeExample ∷ TestTree
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
      let t = pivot tAux0 e0 l0
      in t @?= tAux1

  testPivot2 =
    testCase "Pivoting - 2nd step" $
      let t = pivot tAux1 e1 l1
      in t @?= tAux2

  testPivot3 =
    testCase "Pivoting - 3rd step" $
      let t = pivot tAux2 e2 l2
      in t @?= tAux3

  testFeasibleTableau =
    testCase "Computing the feasible tableau for the original problem" $
      let Just t = toOriginalTableau tAux3
      in t @?= tf


  t0 = makeTableau
        2
        4
        [ [ 2, -1]
        , [ 0, -1]
        , [-1,  2]
        , [-1,  0]
        ]
        [-2,4,-2,4]
        [1,2]
        0
        [3,4,5,6]
        [1,2]

  tAux0 = makeTableau'
        3
        4
        [ [ 1,  2, -1]
        , [ 1,  0, -1]
        , [ 1, -1,  2]
        , [ 1, -1,  0]
        ]
        [-2,4,-2,4]
        [-1,0,0]
        0
        [3,4,5,6]
        [0,1,2]
        (Just (0, [0,1,2]))

  e0 = 0
  l0 = 3

  tAux1 = makeTableau'
        3
        4
        [ [ 1, -2,  1]
        , [ 1, -2,  0]
        , [ 1, -3,  3]
        , [ 1, -3,  1]
        ]
        [2,6,0,6]
        [-1,2,-1]
        (-2)
        [0,4,5,6]
        [3,1,2]
        (Just (0, [0,1,2]))

  e1 = 1
  l1 = 5

  tAux2 = makeTableau'
        3
        4
        [ [ 1%3,  2%3, -1]
        , [ 1%3,  2%3, -2]
        , [ 1%3, -1%3,  1]
        , [ 0,    1,   -2]
        ]
        [2,6,0,6]
        [-1%3,-2%3,1]
        (-2)
        [0,4,1,6]
        [3,5,2]
        (Just (0, [1%3,-1%3,3%1]))

  e2 = 2
  l2 = 0

  tAux3 = makeTableau'
        3
        4
        [ [ 1%3,  2%3, -1]
        , [-1%3, -2%3,  2]
        , [ 2%3,  1%3, -1]
        , [-2%3, -1%3,  2]
        ]
        [2,2,2,2]
        [0,0,-1]
        0
        [2,4,1,6]
        [3,5,0]
        (Just (6, [4%3, 5%3, -3]))

  tf = makeTableau
        2
        4
        [ [ 1%3,  2%3]
        , [-1%3, -2%3]
        , [ 2%3,  1%3]
        , [-2%3, -1%3]
        ]
        [2,2,2,2]
        [4%3, 5%3]
        6
        [2,4,1,6]
        [3,5]
