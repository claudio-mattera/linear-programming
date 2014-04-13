{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import qualified TestsTableau
import qualified TestsCompleteExample
import qualified TestsLatex

main ∷ IO ()
main =
  defaultMain tests

tests ∷ TestTree
tests = testGroup "Unit tests"
  [ TestsTableau.tests
  , TestsCompleteExample.tests
  , TestsLatex.tests
  ]
