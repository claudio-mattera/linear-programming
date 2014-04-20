{-# LANGUAGE UnicodeSyntax #-}

module LinearProgramming.AuxiliaryTableau (
    AuxiliaryTableau(..)
  , generateAuxiliaryTableau
  , toOriginalTableau
  , auxiliaryPivot
  ) where

import Prelude.Unicode
import qualified Data.Matrix as M
import qualified Data.Vector as V

import LinearProgramming.Tableau

{-
NOTE: Data.Matrix indices range from (1,1) to (n,m), while Data.Vector indices
range from 0 to n-1.
-}


data AuxiliaryTableau = AuxiliaryTableau {
  auxTableau ∷ Tableau
, auxZ ∷ Value
, auxC ∷ V.Vector Value
} deriving (Show, Eq)


generateAuxiliaryTableau ∷ Tableau → AuxiliaryTableau
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
    in AuxiliaryTableau {
      auxTableau = Tableau {
        tabM = m
      , tabN = n + 1
      , tabA = a'
      , tabB = b
      , tabC = c'
      , tabZ = 0
      , tabBasicVariables = basicVariables
      , tabIndependantVariables = independantVariables'
      }
    , auxZ = z
    , auxC = 0 `V.cons` c
    }

toOriginalTableau ∷ AuxiliaryTableau → Tableau
toOriginalTableau AuxiliaryTableau {
    auxTableau = tableau@(Tableau {
      tabM = m
    , tabN = n
    , tabA = a
    , tabB = b
    , tabBasicVariables = basicVariables
    , tabIndependantVariables = independantVariables
    })
  , auxZ = z
  , auxC = c
  } =
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
  }


auxiliaryPivot ∷ AuxiliaryTableau → Variable → Variable → AuxiliaryTableau
auxiliaryPivot AuxiliaryTableau {
    auxTableau = tableau@(Tableau {
      tabN = n
    , tabA = a
    , tabB = b
    , tabBasicVariables = basicVariables
    , tabIndependantVariables = independantVariables
    })
  , auxZ = z
  , auxC = c
  } entering leaving =
      let Just i = V.elemIndex entering independantVariables
          Just j = V.elemIndex leaving basicVariables

          t' = pivot tableau entering leaving

          aji = a M.! (j+1, i+1)
          bj = b V.! j
          ci = c V.! i

          z' = z - ci ⋅ bj ÷ aji

          aFinal = tabA t'

          ej = M.rowVector $ M.getRow (i+1) $ M.identity n
          aj = M.rowVector $ M.getRow (j+1) aFinal
          cm = M.rowVector c
          c' = M.getRow 1 $ cm + M.scaleMatrix ci (aj - ej)
      in AuxiliaryTableau {
        auxTableau = t'
      , auxZ = z'
      , auxC = c'
      }
