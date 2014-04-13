{-# LANGUAGE UnicodeSyntax #-}

module LinearProgramming.Latex (
    toLatex
  , showRational
  ) where

import LinearProgramming.Tableau

import Prelude hiding (all, any, zipWith3, zip)
import Prelude.Unicode
import Data.Matrix as M
import Data.Vector as V
import Data.Ratio

type ShowFunction = Value → String

showRational ∷ ShowFunction
showRational r
  | denominator r ≡ 1   = "$" ⧺ show (numerator r) ⧺ "$"
  | numerator r < 1     = "$-\\frac{" ⧺ show (abs (numerator r)) ⧺
                          "}{" ⧺ show (denominator r) ⧺ "}$"
  | otherwise           = "$\\frac{" ⧺ show (numerator r) ⧺
                          "}{" ⧺ show (denominator r) ⧺ "}$"

toLatex ∷ Tableau → String
toLatex Tableau {
    tabM = m
  , tabN = n
  , tabA = a
  , tabB = b
  , tabC = c
  , tabZ = z
  , tabBasicVariables = basicVariables
  , tabIndependantVariables = independantVariables
  } =
    let rows_indices = [0..m-1]
        rows = Prelude.map writeLine rows_indices
        last_row = showRational z ⧺ " & " ⧺
          V.foldl1 (\a x → a ⧺ " & " ⧺ x) (V.map showRational c)
    in  "\\begin{tabular}{c|" ⧺ Prelude.replicate n 'c' ⧺ "}\n" ⧺
        Prelude.concat rows ⧺
        "\\hline\n" ⧺
        last_row ⧺ "\\\\\n" ⧺
        "\\end{tabular}"
  where
    writeLine i =
      let f a x = a ⧺ " & " ⧺ x
          bi = b V.! i
          asn = M.getRow (i+1) a
          as = V.foldl1 f (V.map showRational asn)
      in showRational bi ⧺ " & " ⧺ as ⧺ "\\\\\n"
