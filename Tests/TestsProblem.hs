{-# LANGUAGE UnicodeSyntax #-}

module TestsProblem (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import LinearProgramming.Problem

tests ∷ TestTree
tests = testGroup "Problem"
          [ canonicalIdempotent
          , testLectures
          ]

canonicalIdempotent ∷ TestTree
canonicalIdempotent =
  testCase "Canonical is idempotent" $
    let problem =
          ( Maximize
          , [(1,1), (2,1)]
          , [ ([(1,1), (2,1)], LesserEqual, 1)
            , ([(1,1), (2,5)], LesserEqual, -1)
            ])

        canonicalExpected =
          ( [(1,1), (2,1)]
          , [ ([(1,1), (2,1)], 1)
            , ([(1,1), (2,5)], -1)
            ])

        canonical = makeCanonical problem

    in
      canonical @?= canonicalExpected


testLectures ∷ TestTree
testLectures =
  testGroup "Examples from Linear and Integer Programming course" $
    [ testCase "Lecture \"Standard Form LP Formulation\"" $
        let problem =
              ( Minimize
              , [(1,-5), (2,4), (3,-3)]
              , [ ([(1,2), (2,-3), (3,1)], Equal, 5)
                , ([(1,4), (2,1), (3,2)], GreaterEqual, 11)
                , ([(1,3), (2,4), (3,2)], LesserEqual, 8)
                ])

            canonicalExpected =
              ( [(1,5), (2,-4), (3,3)]
              , [ ([(1,-2), (2,3), (3,-1)], -5)
                , ([(1,2), (2,-3), (3,1)], 5)
                , ([(1,-4), (2,-1), (3,-2)], -11)
                , ([(1,3), (2,4), (3,2)], 8)
                ])

            canonical = makeCanonical problem

        in
          canonical @?= canonicalExpected
    ]
