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
  , generateAuxiliaryTableau
  ) where

import Prelude.Unicode
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Maybe (isJust)
import Data.Function (on)
import Data.Ratio

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
      let t1 = M.colVector b M.<|> a
          z' = M.fromLists [[z]]
          t2 = z' M.<|> M.rowVector c
          t3 = t1 M.<-> t2
      in show t3 ⧺ "\n" ⧺ show basicVariables ⧺ " (m: " ⧺ show m ⧺ ")\n" ⧺
       show independantVariables ⧺ " (n: " ⧺ show n ⧺ ")"

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

          aji = a M.! (j+1, i+1)
          bj = b V.! j
          ci = c V.! i

          z' = z - ci ⋅ bj ÷ aji
          a' = M.scaleRow (-1 ÷ aji) (j+1) a
          a'' = M.setElem (1 ÷ aji) (j+1, i+1) a'

          rows_range = [0..j-1] ⧺ [j+1..m-1]

          aFinal = Prelude.foldl f a'' rows_range
          f m k =
            let aki = a M.! (k+1, i+1)
                m' = M.combineRows (k+1) aki (j+1) m
            in M.setElem (m' M.! (k+1, i+1) - aki) (k+1, i+1) m'


          bReplacements = Prelude.foldl g [] rows_range
          g ls k =
            let bk = b V.! k
                aki = a M.! (k+1,i+1)
            in (k, bk - aki ⋅ bj ÷ aji) : ls

          bj' = - bj ÷ aji
          b' = b V.// ((j, bj') : bReplacements)

          ej = M.rowVector $ M.getRow (i+1) $ M.identity n
          aj = M.rowVector $ M.getRow (j+1) aFinal
          cm = M.rowVector c
          c' = M.getRow 1 $ cm + M.scaleMatrix ci (aj - ej)
      in Tableau {
        tabN = n
      , tabM = m
      , tabZ = z'
      , tabA = aFinal
      , tabB = b'
      , tabC = c'
      , tabBasicVariables = basicVariables V.// [(j, entering)]
      , tabIndependantVariables = independantVariables V.// [(i, leaving)]
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
  let Just enteringIndex = V.elemIndex entering vi
      aCol = M.getCol (enteringIndex + 1) a
      coefficients = V.zipWith3 (\i a b →
        (i, if b ≠ 0 then Just (a ÷ b) else Nothing)) vb aCol b
      finiteCoefficients = V.map (\(i, Just j) → (i, j)) $ V.filter (isJust ∘ snd) coefficients
      (leavingVariable, minimalCoefficient) = V.minimumBy (compare `on` snd) finiteCoefficients
  in if V.null finiteCoefficients ∨ minimalCoefficient ≥ 0
      then Nothing
      else Just leavingVariable


generateAuxiliaryTableau ∷ Tableau → Tableau
generateAuxiliaryTableau t@(Tableau {
    tabM = m
  , tabN = n
  , tabA = a
  , tabB = b
  , tabC = c
  , tabZ = z
  , tabBasicVariables = basicVariables
  , tabIndependantVariables = independantVariables
  }) =
    let x0Col = V.replicate m 1
        a' = M.colVector x0Col M.<|> a
        c' = V.replicate (n+1) 0 V.// [(0, -1)]
        independantVariables' = 0 `V.cons` independantVariables
    in Tableau {
      tabM = m
    , tabN = n + 1
    , tabA = a'
    , tabB = b
    , tabC = c'
    , tabZ = 0
    , tabBasicVariables = basicVariables
    , tabIndependantVariables = independantVariables'
    }
