{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import qualified TestsTableau
import qualified TestsCompleteExample
import qualified TestsLatex
import qualified TestsParser
import qualified TestsProblem

main ∷ IO ()
main =
  defaultMain tests

tests ∷ TestTree
tests = testGroup "Unit tests"
  [ TestsTableau.tests
  , TestsCompleteExample.tests
  , TestsLatex.tests
  , TestsParser.tests
  , TestsProblem.tests
  ]
