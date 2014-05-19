{-# LANGUAGE UnicodeSyntax #-}

module TestsSimplex (tests) where

import Prelude.Unicode
import Data.Ratio

import qualified Data.DList as DL

import Test.Tasty
import Test.Tasty.HUnit

import LinearProgramming.Tableau
import LinearProgramming.Simplex

tests ∷ TestTree
tests = testGroup "Simplex"
          [ testGroup "Examples from Linear and Integer Programming course" $
              map makeTestsFromSample samples
          , infeasibleProblemTest
          , feasibleAfterInitializationTest
          , completeExampleTest
          ]

type Sample = (String, Tableau, Either Error Tableau)

sample1 ∷ Sample
sample1 =
  let desc = "Lecture \"A Complete Example\""

      tInitial = makeTableau
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

      tExpected = makeTableau
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

  in (desc, tInitial, Right tExpected)

sample2 ∷ Sample
sample2 =
  let desc = "Lecture \"Handling Unbounded Problems\""

      tInitial = makeTableau
            3
            4
            [ [-1,  1,  0]
            , [ 1,  0, -1]
            , [ 2,  0, -1]
            , [ 1, -1,  0]
            ]
            [5,6,2,4]
            [2,3,-5]
            0
            [4,5,6,7]
            [1,2,3]

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


infeasibleProblemTest ∷ TestTree
infeasibleProblemTest =
  let desc = "Infeasible problem"

      tInitial = makeTableau
            2
            4
            [ [ 3, -1]
            , [ 0, -1]
            , [-1,  1]
            , [-1,  0]
            ]
            [2, -11, 3, 6]
            [1, 2]
            0
            [3,4,5,6]
            [1,2]

      sample = (desc, tInitial, Left Infeasible)
  in makeTestsFromSample sample


feasibleAfterInitializationTest ∷ TestTree
feasibleAfterInitializationTest =
  let desc = "Feasible after initialization problem"

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

      tExpected = makeTableau
        2
        4
        [ [ 0, -1]
        , [ 1, -2]
        , [-1,  0]
        , [-2,  1]
        ]
        [4,2,4,2]
        [-1,-2]
        12
        [2,5,1,3]
        [6,4]


      sample = (desc, tInitial, Right tExpected)
  in makeTestsFromSample sample


completeExampleTest ∷ TestTree
completeExampleTest =
  testCase "Complete example" $
    let t0 = makeTableau
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


        e0 = 2 ∷ Integer
        l0 = 3 ∷ Integer


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

        e1 = 1 ∷ Integer
        l1 = 4 ∷ Integer


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

        e2 = 3 ∷ Integer
        l2 = 6 ∷ Integer


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

        historyExpected =
          DL.fromList [
            LogString ("Initial tableau is already feasible, " ⧺
                       "skipping to phase two")
          , LogString "Starting"
          , LogTableau t0
          , LogString ("Entering variable is " ⧺ show e0)
          , LogString ("Leaving variable is " ⧺ show l0)
          , LogTableau t1
          , LogString ("Entering variable is " ⧺ show e1)
          , LogString ("Leaving variable is " ⧺ show l1)
          , LogTableau t2
          , LogString ("Entering variable is " ⧺ show e2)
          , LogString ("Leaving variable is " ⧺ show l2)
          , LogTableau t3
          ]

        (result, history) = runSimplex t0
    in do
      result @?= Right t3
      history @?= historyExpected
