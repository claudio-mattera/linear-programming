{-# LANGUAGE UnicodeSyntax #-}

module LinearProgramming.Tableau (
    Variable
  , Value
  , Tableau(..)
  , makeTableau
  , makeTableau'
  , pivot
  , newPivot
  , matrixToTableau
  , tableauToMatrix
  , isFeasible
  , isFinal
  , chooseEnteringVariable
  , chooseLeavingVariable
  , getMinimalNegativeCoefficientVariable
  , generateAuxiliaryTableau
  , toOriginalTableau
  ) where

import Prelude.Unicode
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Maybe (isJust, fromJust)
import Data.Function (on)
import Data.Ratio

import Control.Arrow (second)

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

makeTableau ∷ Int → Int → [[Value]] → [Value] → [Value] → Value →
                [Variable] → [Variable] → Tableau
makeTableau n m a b c z basicVariables independantVariables =
  makeTableau'  n m a b c z basicVariables independantVariables Nothing

makeTableau' ∷ Int → Int → [[Value]] → [Value] → [Value] → Value →
                [Variable] → [Variable] → Maybe (Value, [Value]) → Tableau
makeTableau' n m a b c z basicVariables independantVariables auxiliaryData =
  Tableau {
    tabM = m
  , tabN = n
  , tabA = M.fromLists a
  , tabB = V.fromList b
  , tabC = V.fromList c
  , tabZ = z
  , tabBasicVariables = V.fromList basicVariables
  , tabIndependantVariables = V.fromList independantVariables
  , tabAuxiliaryData = fmap (second V.fromList) auxiliaryData
}


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
      let t1 = fmap showKeepZero (M.colVector b) M.<|> showMatrix a
          z' = fmap showKeepZero (M.fromLists [[z]])
          t2 = z' M.<|> showMatrix (M.rowVector c)
          t3 = t1 M.<-> t2
          basicVars = V.generate m (\k → "x" ⧺ show (basicVariables V.! k))
          t4 = M.colVector basicVars M.<-> M.rowVector (V.singleton "z")
          t5 = fmap (⧺ " = ") t4 M.<|> t3
          auxiliaryDataRow = case auxiliaryData of
            Nothing           → M.fromList 0 (n+2) []
            Just (auxZ, auxC) → M.fromLists [["aux Z = ", showKeepZero auxZ] ⧺
                (V.toList ∘ M.getRow 1) (showMatrix (M.rowVector auxC))]
          t6 = t5 M.<-> auxiliaryDataRow
      in "\n" ⧺ filter (≠ '"') (show t6)

    where

    showMatrix ∷ M.Matrix Value → M.Matrix String
    showMatrix matrix =
      foldl
        (\acc colNo → M.mapCol (addVar colNo) colNo acc)
        (fmap showEmptyZero matrix)
        [1 .. M.ncols matrix]

    addVar ∷ Int → Int → String → String
    addVar _ _ [] = []
    addVar k _ xs =
      let v = independantVariables V.! (k - 1)
      in if xs ≡ "1"
        then " x" ⧺ show v ⧺ " "
        else if xs ≡ "-1"
          then " -x" ⧺ show v ⧺ " "
          else " " ⧺ xs ⧺ " x" ⧺ show v ⧺ " "

    showEmptyZero ∷ Value → String
    showEmptyZero r
      | numerator r ≡ 0     = ""
      | denominator r ≡ 1   = show (numerator r)
      | otherwise           = show (numerator r) ⧺ "/" ⧺ show (denominator r)

    showKeepZero ∷ Value → String
    showKeepZero r
      | denominator r ≡ 1   = show (numerator r)
      | otherwise           = show (numerator r) ⧺ "/" ⧺ show (denominator r)

matrixToTableau ∷ Int → Int → V.Vector Variable → V.Vector Variable → M.Matrix Value → Tableau
matrixToTableau n m vb vi mat =
    let a = M.submatrix 1 m 2 (n+1) mat
        b = M.submatrix 1 m 1 1 mat
        c = M.submatrix (m+1) (m+1) 2 (n+1) mat
        z = M.submatrix (m+1) (m+1) 1 1 mat
        auxC = M.submatrix (m+2) (m+2) 2 (n+1) mat
        auxZ = M.submatrix (m+2) (m+2) 1 1 mat
    in Tableau {
      tabM = m
    , tabN = n
    , tabA = a
    , tabB = M.getCol 1 b
    , tabC = M.getRow 1 c
    , tabZ = z M.! (1,1)
    , tabBasicVariables = vb
    , tabIndependantVariables = vi
    , tabAuxiliaryData = if M.nrows mat > m + 1
        then Just (auxZ M.! (1,1), (M.getRow 1 auxC))
        else Nothing
    }

tableauToMatrix ∷ Tableau → M.Matrix Value
tableauToMatrix Tableau {
    tabA = a
  , tabB = b
  , tabC = c
  , tabZ = z
  , tabAuxiliaryData = auxiliaryData
  } =
    case auxiliaryData of
      Nothing → (M.colVector b M.<|> a) M.<-> M.rowVector (z `V.cons` c)
      Just (auxZ, auxC) → (M.colVector b M.<|> a) M.<-> M.rowVector (z `V.cons` c) M.<-> M.rowVector (auxZ `V.cons` auxC)


newPivot ∷ Tableau → Variable → Variable → Tableau
newPivot tableau@(Tableau {
    tabM = m
  , tabN = n
  , tabBasicVariables = basicVariables
  , tabIndependantVariables = independantVariables
  , tabAuxiliaryData = auxiliaryData
  }) entering leaving =
      let mat = tableauToMatrix tableau

          Just i = V.elemIndex entering independantVariables >>= (return ∘ (+2))
          Just j = V.elemIndex leaving basicVariables >>= (return ∘ (+1))

          aji = mat M.! (j, i)

          mat' = M.mapCol (\k _ → if k ≡ j then -1 else 0) i mat

          mat'' = M.scaleRow (-1 ÷ aji) j mat'

          rowsRange = case auxiliaryData of
            Nothing → [1..j-1] ⧺ [j+1..m+1]
            Just _ → [1..j-1] ⧺ [j+1..m+2]
          mat''' = foldl f mat'' rowsRange

          f acc k =
            let aik = mat M.! (k, i)
            in M.combineRows k aik j acc

          newBasicVariables = basicVariables V.// [((j-1), entering)]
          newIndependantVariables = independantVariables V.// [((i-2), leaving)]
      in matrixToTableau n m newBasicVariables newIndependantVariables mat'''

pivot ∷ Tableau → Variable → Variable → Tableau
pivot Tableau {
    tabM = m
  , tabN = n
  , tabA = a
  , tabB = b
  , tabC = c
  , tabZ = z
  , tabBasicVariables = basicVariables
  , tabIndependantVariables = independantVariables
  , tabAuxiliaryData = auxiliaryData
  } entering leaving =
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
          f acc k =
            let aki = a M.! (k+1, i+1)
                acc' = M.combineRows (k+1) aki (j+1) acc
            in M.setElem (acc' M.! (k+1, i+1) - aki) (k+1, i+1) acc'


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
      (largestVariable, largestValue) =
        V.maximumBy (compare `on` snd) csWithIndex
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
      coefficients = V.zipWith3 (\j aji bj →
        if aji < 0
        then Just (j, -bj ÷ aji)
        else Nothing) vb aCol b
      finiteCoefficients = V.map fromJust ∘ V.filter isJust $ coefficients
      (minimalVariable, minimalCoefficient) =
        V.minimumBy (compare `on` snd) finiteCoefficients
      minimalCoefficients =
        V.filter ((≡ minimalCoefficient) ∘ snd) finiteCoefficients
      leavingVariable = case aux of
          Nothing → minimalVariable
          Just _  → case V.find ((≡ 0) ∘ fst) minimalCoefficients of
            Nothing                     → minimalVariable
            Just (auxiliaryVariable, _) → auxiliaryVariable
      result = if V.null finiteCoefficients
        then Nothing
        else Just leavingVariable
  in result


getMinimalNegativeCoefficientVariable ∷ Tableau → Variable
getMinimalNegativeCoefficientVariable Tableau {
    tabB = b
  , tabBasicVariables = vb
  } =
    let pairs = V.zip vb b
    in fst $ V.minimumBy (compare `on` snd) pairs


generateAuxiliaryTableau ∷ Tableau → Tableau
generateAuxiliaryTableau Tableau {
    tabM = m
  , tabN = n
  , tabA = a
  , tabB = b
  , tabC = c
  , tabZ = z
  , tabBasicVariables = basicVariables
  , tabIndependantVariables = independantVariables
  } =
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


toOriginalTableau ∷ Tableau → Maybe Tableau
toOriginalTableau Tableau {tabAuxiliaryData = Nothing} =
  error "Not an auxiliary tableau"
toOriginalTableau Tableau {
    tabM = m
  , tabN = n
  , tabA = a
  , tabB = b
  , tabBasicVariables = vb
  , tabIndependantVariables = vi
  , tabAuxiliaryData = Just (z, c)
  } = do
    i0 ← V.elemIndex 0 vi
    let a' = M.submatrix 1 m 1 i0 a M.<|> M.submatrix 1 m (i0 + 2) n a
        c' = V.take i0 c V.++ V.drop (i0 + 1) c
        vi' =
          V.take i0 vi V.++ V.drop (i0 + 1) vi
    Just Tableau {
      tabM = m
    , tabN = n - 1
    , tabA = a'
    , tabB = b
    , tabC = c'
    , tabZ = z
    , tabBasicVariables = vb
    , tabIndependantVariables = vi'
    , tabAuxiliaryData = Nothing
  }
