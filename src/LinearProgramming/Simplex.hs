{-# LANGUAGE UnicodeSyntax #-}

module LinearProgramming.Simplex (
    Error(..)
  , optimize
  ) where

import Prelude.Unicode

import LinearProgramming.Tableau

data Error = Infeasible
           | Unbounded
           | Unknown
           deriving (Show, Eq)

optimize ∷ Tableau → Either Error Tableau
optimize tableau
  | not (isFeasible tableau) = Left Infeasible
  | isFinal tableau          = return tableau
  | otherwise                = do
    case chooseEnteringVariable tableau of
      Nothing → do
        Left Unknown
      Just entering → do
        case chooseLeavingVariable tableau entering of
          Nothing → Left Unbounded
          Just leaving → do
            let tableau' = pivot tableau entering leaving
            optimize tableau'
