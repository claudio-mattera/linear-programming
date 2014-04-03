{-# LANGUAGE UnicodeSyntax #-}

module LinearProgramming.Tableau (
    Variable
  , Value
  , Tableau(..)
  , pivot
  , isFeasible
  , isFinal
  , chooseEnteringVariable
  , chooseLeavingVariable
  ) where

import Prelude hiding (all, any, zipWith3, zip)
import Prelude.Unicode
import Data.Matrix as M
import Data.Vector as V
import Data.Maybe (isJust)
import Data.Function (on)

import Debug.HTrace

{-
NOTE: Data.Matrix indices range from (1,1) to (n,m), while Data.Vector indices
range from 0 to n-1.
-}

type Variable = Int
type Value = ℚ

data Tableau = Tableau {
  tabM                    ∷ Int
, tabN                    ∷ Int
, tabA                    ∷ M.Matrix Value
, tabB                    ∷ V.Vector Value
, tabC                    ∷ V.Vector Value
, tabZ                    ∷ Value
, tabBasicVariables       ∷ V.Vector Variable
, tabIndependantVariables ∷ V.Vector Variable
} deriving Eq

instance Show Tableau where
  show Tableau {
    tabM = m
  , tabN = n
  , tabA = a
  , tabB = b
  , tabC = c
  , tabZ = z
  , tabBasicVariables = basicVariables
  , tabIndependantVariables = independantVariables
  } =
      let t1 = colVector b <|> a
          z' = fromLists [[z]]
          t2 = z' <|> rowVector c
          t3 = t1 <-> t2
      in show t3

pivot ∷ Tableau → Variable → Variable → Tableau
pivot t@(Tableau {
    tabM = m
  , tabN = n
  , tabA = a
  , tabB = b
  , tabC = c
  , tabZ = z
  , tabBasicVariables = basicVariables
  , tabIndependantVariables = independantVariables
  }) entering leaving =
      let Just i = V.elemIndex entering independantVariables
          Just j = V.elemIndex leaving basicVariables
          z' = z - (c V.! i) ⋅ (b V.! j) ÷ (a M.! (j+1, i+1))
          a' = a
          b' = b
          c' = c
      in t {
        tabZ = z'
      , tabA = a'
      , tabB = b'
      , tabC = c'
      }

isFeasible ∷ Tableau → Bool
isFeasible Tableau {tabB = b} = V.all (≥ 0) b

isFinal ∷ Tableau → Bool
isFinal Tableau {tabC = c} = V.all (≤ 0) c

chooseEnteringVariable ∷ Tableau → Maybe Variable
chooseEnteringVariable Tableau {tabC = c, tabIndependantVariables = v} =
  let csWithIndex = V.zip v c
      (largestVariable, largestValue) = V.maximumBy (compare `on` snd) csWithIndex
  in if largestValue ≥ 0
      then Just largestVariable
      else Nothing

chooseLeavingVariable ∷ Tableau → Variable → Maybe Variable
chooseLeavingVariable Tableau {
  tabA = a
, tabB = b
, tabBasicVariables = vb
, tabIndependantVariables = vi
} entering =
  let Just enteringIndex = elemIndex entering vi
      aCol = getCol enteringIndex a
      coefficients = zipWith3 (\i a b →
        (i, if b ≠ 0 then Just (- a ÷ b) else Nothing)) vi aCol b
      finiteCoefficients = V.map (\(i, Just j) → (i, j)) $ V.filter (isJust ∘ snd) coefficients
      (leavingVariable, _) = V.minimumBy (compare `on` snd) finiteCoefficients
  in if V.null finiteCoefficients
      then Nothing
      else Just leavingVariable
