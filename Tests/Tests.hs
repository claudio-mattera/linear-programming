{-# LANGUAGE UnicodeSyntax #-}

module Main where

import System.Exit (exitSuccess, exitFailure)

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import TestMatrix

main ∷ IO ()
main = do
  let tests = [ TestMatrix.test
              ]
  outcomes ← sequence tests
  if and outcomes
    then exitSuccess
    else exitFailure
