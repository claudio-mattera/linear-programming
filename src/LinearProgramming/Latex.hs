{-# LANGUAGE UnicodeSyntax #-}

{- | Latex representation of tableau.
-}

module LinearProgramming.Latex (
    tableauToLatex
  , rationalToLatex
  ) where

import LinearProgramming.Tableau

import Prelude hiding (all, any, zipWith3, zip)
import Prelude.Unicode
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Ratio

-- | Generates a Latex string that represents the rational number @r@.
--
--   It generates the most compact representation, depending whether @r@ is
--   integer, rational, positive or negative.
rationalToLatex ∷ Value → String
rationalToLatex r
  | denominator r ≡ 1   = "$" ⧺ show (numerator r) ⧺ "$"
  | numerator r < 1     = "$-\\frac{" ⧺ show (abs (numerator r)) ⧺
                          "}{" ⧺ show (denominator r) ⧺ "}$"
  | otherwise           = "$\\frac{" ⧺ show (numerator r) ⧺
                          "}{" ⧺ show (denominator r) ⧺ "}$"


-- | Generates the Latex string that represents a 'Tableau'.
--
--   It uses the @tabular@ environment to draw the typical mathematical
--   representation of a LP tableau.
tableauToLatex ∷ Tableau → String
tableauToLatex Tableau {
    tabM = m
  , tabN = n
  , tabA = a
  , tabB = b
  , tabC = c
  , tabZ = z
  } =
    let rowsIndices = [0..m-1]
        rows = Prelude.map writeLine rowsIndices
        lastRow = rationalToLatex z ⧺ " & " ⧺
          V.foldl1 (\acc x → acc ⧺ " & " ⧺ x) (V.map rationalToLatex c)
    in  "\\begin{tabular}{c|" ⧺ Prelude.replicate n 'c' ⧺ "}\n" ⧺
        Prelude.concat rows ⧺
        "\\hline\n" ⧺
        lastRow ⧺ "\\\\\n" ⧺
        "\\end{tabular}"
  where
    writeLine i =
      let f acc x = acc ⧺ " & " ⧺ x
          bi = b V.! i
          asn = M.getRow (i+1) a
          as = V.foldl1 f (V.map rationalToLatex asn)
      in rationalToLatex bi ⧺ " & " ⧺ as ⧺ "\\\\\n"
