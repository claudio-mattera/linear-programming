{-# LANGUAGE UnicodeSyntax #-}

module TestsSimplex (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import LinearProgramming.Tableau
import LinearProgramming.Simplex

tests ∷ TestTree
tests = testGroup "Simplex"
          [ testGroup "Examples from Linear and Integer Programming course" $
              map makeTestsFromSample samples
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
