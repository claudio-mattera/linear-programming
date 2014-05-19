{-# LANGUAGE UnicodeSyntax #-}

{- | MathML representation of tableau.
-}

module LinearProgramming.MathML (
    tableauToMathML
  , extendedTableauToMathML
  , rationalToMathML
  ) where

import LinearProgramming.Tableau

import Prelude hiding (all, any, zipWith3, zip)
import Prelude.Unicode
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Ratio

-- | Generates a MathML string that represents the rational number @r@.
--
--   It generates the most compact representation, depending whether @r@ is
--   integer, rational, positive or negative.
rationalToMathML ∷ Value → String
rationalToMathML r
  | denominator r ≡ 1   = integerToMathML (numerator r)
  | numerator r < 0     = "<mo>-</mo>" ⧺ positiveRationalToMathML (-r)
  | otherwise           = positiveRationalToMathML r
  where
  positiveRationalToMathML g =
    "<mfrac>" ⧺ integerToMathML (numerator g) ⧺
    integerToMathML (denominator g) ⧺ "</mfrac>"

-- | Generates a MathML string that represents the integer number @n@.
integerToMathML ∷ Integer → String
integerToMathML n
  | n < 0     = "<mo>-</mo>" ⧺ positiveIntegerToMathML (-n)
  | otherwise = positiveIntegerToMathML n
  where
  positiveIntegerToMathML i = "<mn>" ⧺ show i ⧺ "</mn>"

-- | Generates a MathML string that represents the number @r@ wrapped in a @mtd@
--   tag.
cellToMathML ∷ String → Value → String
cellToMathML extra r =
  if r ≡ 0
  then "<mtd></mtd>"
  else cellToMathMLKeepZero extra r

-- | Generates a MathML string that represents the number @r@ wrapped in a @mtd@
--   tag.
cellToMathMLKeepZero ∷ String → Value → String
cellToMathMLKeepZero extra r =
  if null extra
  then "<mtd>" ⧺ rationalToMathML r ⧺ extra ⧺ "</mtd>"
  else
    if r ≡ 1
    then "<mtd>" ⧺ extra ⧺ "</mtd>"
    else
      if r ≡ (-1)
      then "<mtd><mo>-</mo>" ⧺ extra ⧺ "</mtd>"
      else "<mtd>" ⧺ rationalToMathML r ⧺ extra ⧺ "</mtd>"


-- | Generates a MathML string that represents the variable @x_v@.
variableToMathML ∷ Variable → String
variableToMathML v = "<msub><mi>x</mi><mn>" ⧺ show v ⧺ "</mn></msub>"


-- | Generates the MathML string that represents a 'Tableau'.
--
--   It uses the @mtable@ tag to draw the typical mathematical
--   representation of a LP tableau.
tableauToMathML ∷ Tableau → String
tableauToMathML Tableau {
    tabM = m
  , tabA = a
  , tabB = b
  , tabC = c
  , tabZ = z
  } =
    let rowsIndices = [0..m-1]
        rows = Prelude.map writeLine rowsIndices
        lastRow = "<mtr>" ⧺ cellToMathMLKeepZero "" z ⧺
          V.foldl (\acc x → acc ⧺ cellToMathMLKeepZero "" x) "" c ⧺ "</mtr>"
    in  "<mtable columnalign=\"right\" columnlines=\"solid none\" rowlines=\"" ⧺ unwords (Prelude.replicate (m-1) "none") ⧺ " solid\">\n" ⧺
        Prelude.unlines rows ⧺
        lastRow ⧺
        "\n</mtable>"
  where
    writeLine i =
      let f acc x = acc ⧺ cellToMathML "" x
          bi = b V.! i
          asn = M.getRow (i+1) a
          as = V.foldl f "" asn
      in "<mtr>" ⧺ cellToMathMLKeepZero "" bi ⧺ as ⧺ "</mtr>"



-- | Generates the MathML string that represents a 'Tableau'.
--
--   It uses the @mtable@ tag to draw the typical mathematical
--   representation of a LP tableau.
extendedTableauToMathML ∷ Tableau → String
extendedTableauToMathML Tableau {
    tabM = m
  , tabA = a
  , tabB = b
  , tabC = c
  , tabZ = z
  , tabIndependantVariables = vi
  , tabBasicVariables = vb
  } =
    let rowsIndices = [0..m-1]
        rows = Prelude.map writeLine rowsIndices
        lastRow = "<mtr>" ⧺ cellToMathML "<mi>z</mi><mo>=</mo>" 1 ⧺ cellToMathML "" z ⧺
          V.foldl (\acc (x, i) → acc ⧺ cellToMathML (variableToMathML i) x) "" (V.zip c vi) ⧺ "</mtr>"
    in  "<mtable columnalign=\"right\" columnlines=\"none\" rowlines=\"" ⧺ unwords (Prelude.replicate (m-1) "none") ⧺ " solid\">\n" ⧺
        Prelude.unlines rows ⧺
        lastRow ⧺
        "\n</mtable>"
  where
    writeLine i =
      let f acc (x, j) = acc ⧺ "\n" ⧺ cellToMathML (variableToMathML j) x
          bi = b V.! i
          asn = M.getRow (i+1) a
          as = V.foldl f "" (V.zip asn vi)
      in "<mtr>" ⧺ cellToMathML (variableToMathML (vb V.! i) ⧺ "<mo>=</mo>") 1 ⧺ cellToMathML "" bi ⧺ as ⧺ "</mtr>"
