{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.DList as DL

import Data.Ratio

import Control.Monad (forM_)

import System.IO (hFlush, stdout)

import LinearProgramming.Tableau
import LinearProgramming.Simplex
import LinearProgramming.Parser

import Data.Maybe (isJust, fromJust)
import Data.Function (on)

import Control.Arrow ((***))

sr ∷ Value → String
sr r
  | denominator r ≡ 1   = show (numerator r)
  | otherwise           = show (numerator r) ⧺ " / " ⧺ show (denominator r)

function ∷ Tableau → Variable → Variable → IO (Maybe Variable)
function tableau@Tableau {
  tabA = a
, tabB = b
, tabBasicVariables = vb
, tabIndependantVariables = vi
, tabAuxiliaryData = aux
} entering leavingExpected = do
  let Just enteringIndex = V.elemIndex entering vi
      aCol = M.getCol (enteringIndex + 1) a
      coefficients = V.zipWith3 (\i a b →
        if a ≠ 0 ∧ a < 0
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
  print tableau
  putStrLn ("Entering variable: x" ⧺ show entering ⧺ " (index: " ⧺ show enteringIndex ⧺ ")")
  putStrLn ("aCol: " ⧺ show aCol)
  putStrLn ("Coefficients: " ⧺ show (V.map (fmap (id *** sr)) coefficients))
  putStrLn ("Finite coefficients: " ⧺ show (V.map (id *** sr) finiteCoefficients))
  putStrLn ("Minimal coefficient: " ⧺ show minimalCoefficient)
  putStrLn ("Leaving variable: x" ⧺ show leavingVariable)
  putStrLn ("Expected leaving variable: x" ⧺ show leavingExpected)
  if leavingVariable ≠ leavingExpected
    then putStrLn "ERROR!!!"
    else return ()

  putStrLn "\n---------------------------\n"

  return result


main = do
  let t1 = Tableau {
        tabN = 3
      , tabM = 4
      , tabA = M.fromLists [ [ 1%3,  2%3, -1]
                           , [ 1%3,  2%3, -2]
                           , [ 1%3, -1%3,  1]
                           , [ 0,    1,   -2]
                           ]
      , tabB = V.fromList [2,6,0,6]
      , tabC = V.fromList [-1%3,-2%3,1]
      , tabZ = -2
      , tabBasicVariables = V.fromList [0,4,1,6]
      , tabIndependantVariables = V.fromList [3,5,2]
      , tabAuxiliaryData = Just (0, V.fromList [1%3,-1%3,3%1])
      }

      e1 = 2
      l1 = 0


  function t1 e1 l1

  let text = "max x1 + x2 + x3\n" ⧺
             "\n" ⧺
             "x1 + x2 <= 18\n" ⧺
             "x1 + x3 <= 46\n" ⧺
             "x2 + -1 x3 <= -5\n" ⧺
             "\n"

  case parseTableau text of
    Left e → do
      print e
    Right tableau → do
      let (result, history) = runSimplex tableau


      putStrLn ""
      putStrLn "History:"
      forM_ (DL.toList history) (\t → print t >> putStrLn "")

      putStrLn "Result:"
      print result

