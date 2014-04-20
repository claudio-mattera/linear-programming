{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Test.Tasty

import qualified TestsTableau
import qualified TestsVariables
import qualified TestsCompleteExample
import qualified TestsSimplex
import qualified TestsLatex
import qualified TestsParser
import qualified TestsProblem

main ∷ IO ()
main =
  defaultMain tests

tests ∷ TestTree
tests = testGroup "Unit tests"
  [ TestsTableau.tests
  , TestsVariables.tests
  , TestsCompleteExample.tests
  , TestsSimplex.tests
  , TestsLatex.tests
  , TestsParser.tests
  , TestsProblem.tests
  ]
