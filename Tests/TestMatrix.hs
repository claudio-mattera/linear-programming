{-# LANGUAGE UnicodeSyntax #-}

module TestMatrix where

import LinearProgramming.Matrix
import Numeric.Matrix hiding (map)

import Prelude.Unicode
import System.Exit (exitSuccess, exitFailure)

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

instance (Arbitrary a, Eq a, Ord a, MatrixElement a) ⇒ Arbitrary (Matrix a) where
  arbitrary = sized arbitraryMatrix

newtype FixedLengthList a = FixedLengthList [a]

arbitraryFixedLengthList ∷ (Arbitrary a, Eq a, Ord a) ⇒ Int → Gen (FixedLengthList a)
arbitraryFixedLengthList 0 = return (FixedLengthList [])
arbitraryFixedLengthList size | size > 0 = do
  a ← arbitrary
  FixedLengthList as ← arbitraryFixedLengthList (size - 1)
  return (FixedLengthList (a:as))

groupRows ∷ Int → [a] → [[a]]
groupRows _ [] = []
groupRows n xs = take n xs : groupRows n (drop n xs)

arbitraryMatrix ∷ (Arbitrary a, Eq a, Ord a, MatrixElement a) ⇒ Int → Gen (Matrix a)
arbitraryMatrix 0 = return empty
arbitraryMatrix size | size > 0 = do
  rowsNumber ← elements [1..size]
  colsNumber ← elements [1..size]
  FixedLengthList elements ← arbitraryFixedLengthList (rowsNumber ⋅ colsNumber)
  let m = fromList ∘ groupRows rowsNumber $ elements
  return m

prop_row_vectors_idempotent ∷ MatrixElement e ⇒ Matrix e → Bool
prop_row_vectors_idempotent m =
  let rows = rowVectors m
      m' = fromRowVectors rows
  in m ≡ m'

test ∷ IO Bool
test = do
  putStrLn "TestMatrix"
  let tests = [ quickCheckResult (prop_row_vectors_idempotent ∷ Matrix Int → Bool)
              ]
  results ← sequence tests
  return (and ∘ map isSuccess $ results)
