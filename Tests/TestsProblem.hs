{-# LANGUAGE UnicodeSyntax #-}

module TestsProblem (tests) where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.HUnit

import LinearProgramming.Problem
import LinearProgramming.Tableau

tests ∷ TestTree
tests = testGroup "Problem"
          [ canonicalIdempotent
          ]

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
