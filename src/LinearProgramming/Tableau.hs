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
  , toOriginalTableau
  ) where

import Prelude.Unicode
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Maybe (isJust, fromJust)
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
, tabAuxiliaryData        ∷ Maybe (Value, V.Vector Value)
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
  , tabAuxiliaryData = auxiliaryData
  } =
      let t1 = fmap sr0 (M.colVector b) M.<|> foldl (\a colNo → M.mapCol (addVar colNo) colNo a) (fmap sr a) [1..n]
          z' = fmap sr0 (M.fromLists [[z]])
          t2 = z' M.<|> foldl (\a colNo → M.mapCol (addVar colNo) colNo a) (fmap sr (M.rowVector c)) [1..n]
          t3 = t1 M.<-> t2
          t4 = M.colVector (V.generate m (\k → "x" ⧺ show (basicVariables V.! k))) M.<-> M.rowVector (V.singleton "z")
          t5 = fmap (\x → x ⧺ " = ") t4 M.<|> t3
          auxiliaryDataRow = case auxiliaryData of
            Nothing           → M.fromList 0 (n+2) []
            Just (auxZ, auxC) → M.fromLists [["aux Z = ", sr0 auxZ] ⧺ (V.toList ∘ M.getRow 1) (foldl (\a colNo → M.mapCol (addVar colNo) colNo a) (fmap sr0 (M.rowVector auxC)) [1..n])]
          t6 = t5 M.<-> auxiliaryDataRow
      in "\n" ⧺ filter (≠ '"') (show t6)

    where

    addVar ∷ Int → Int → String → String
    addVar _ _ [] = []
    addVar k _ xs =
      let v = independantVariables V.! (k - 1)
      in if xs ≡ "1"
        then " x" ⧺ show v ⧺ " "
        else " " ⧺ xs ⧺ " x" ⧺ show v ⧺ " "

    sr ∷ Value → String
    sr r
      | numerator r ≡ 0     = ""
      | denominator r ≡ 1   = show (numerator r)
      | otherwise           = show (numerator r) ⧺ "/" ⧺ show (denominator r)

    sr0 ∷ Value → String
    sr0 r
      | denominator r ≡ 1   = show (numerator r)
      | otherwise           = show (numerator r) ⧺ "/" ⧺ show (denominator r)



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
  , tabAuxiliaryData = auxiliaryData
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

          auxiliaryData' = case auxiliaryData of
            Nothing → Nothing
            Just (auxZ, auxC) →
              let auxCi = auxC V.! i
                  auxZ' = auxZ - auxCi ⋅ bj ÷ aji

                  auxCm = M.rowVector auxC
                  auxC' = M.getRow 1 $ auxCm + M.scaleMatrix auxCi (aj - ej)

              in Just (auxZ', auxC')
      in Tableau {
        tabN = n
      , tabM = m
      , tabZ = z'
      , tabA = aFinal
      , tabB = b'
      , tabC = c'
      , tabBasicVariables = basicVariables V.// [(j, entering)]
      , tabIndependantVariables = independantVariables V.// [(i, leaving)]
      , tabAuxiliaryData = auxiliaryData'
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
, tabAuxiliaryData = aux
} entering =
  let Just enteringIndex = V.elemIndex entering vi
      aCol = M.getCol (enteringIndex + 1) a
      coefficients = V.zipWith3 (\i a b →
        if a < 0
        then Just (i, -b ÷ a)
        else Nothing) vb aCol b
      finiteCoefficients = V.map fromJust ∘ V.filter isJust $ coefficients
      (minimalVariable, minimalCoefficient) = V.minimumBy (compare `on` snd) finiteCoefficients
      minimalCoefficients = V.filter ((≡ minimalCoefficient) ∘ snd) finiteCoefficients
      leavingVariable = case aux of
          Nothing → minimalVariable
          Just _  → case V.find ((≡ 0) ∘ fst) minimalCoefficients of
            Nothing                     → minimalVariable
            Just (auxiliaryVariable, _) → auxiliaryVariable
      result = if V.null finiteCoefficients
        then Nothing
        else Just leavingVariable
  in result


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
    , tabAuxiliaryData = Just (z, 0 `V.cons` c)
    }


toOriginalTableau ∷ Tableau → Tableau
toOriginalTableau tableau@(Tableau {
    tabM = m
  , tabN = n
  , tabA = a
  , tabB = b
  , tabBasicVariables = basicVariables
  , tabIndependantVariables = independantVariables
  , tabAuxiliaryData = Just (z, c)
  }) =
    let Just i0 = V.elemIndex 0 independantVariables
        a' = M.submatrix 1 m 1 i0 a M.<|> M.submatrix 1 m (i0 + 2) n a
        c' = V.take i0 c V.++ V.drop (i0 + 1) c
        independantVariables' =
          V.take i0 independantVariables V.++ V.drop (i0 + 1) independantVariables
    in Tableau {
      tabM = m
    , tabN = n - 1
    , tabA = a'
    , tabB = b
    , tabC = c'
    , tabZ = z
    , tabBasicVariables = basicVariables
    , tabIndependantVariables = independantVariables'
    , tabAuxiliaryData = Nothing
  }
