{-# LANGUAGE UnicodeSyntax #-}

module TestsLatex (tests) where

import Prelude hiding (all, any)
import Prelude.Unicode

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Test.QuickCheck

import Data.Matrix as M
import Data.Vector as V
import Data.Ratio
import Data.Foldable

import LinearProgramming.Tableau
import LinearProgramming.Latex

tests ∷ TestTree
tests = testGroup "Latex"
          [ testShowRational
          , testToLatex
          ]


testShowRational =
  testGroup "Rational to Latex" $
    [ QC.testProperty "Whole number" $
        \n →
          let r = n % 1
              text = showRational r
              textExpected = "$" ⧺ show n ⧺ "$"
          in (text === textExpected)
    , QC.testProperty "Positive rational" $
        \r' →
          let r = abs r'
              text = showRational r
              textExpected = "$\\frac{" ⧺ show (numerator r) ⧺ "}{" ⧺
                show (denominator r) ⧺ "}$"
          in (denominator r ≠ 1) ==> (text === textExpected)
    , QC.testProperty "Negative rational" $
        \r' →
          let r = - (abs r')
              text = showRational r
              textExpected = "$-\\frac{" ⧺ show (- (numerator r)) ⧺ "}{" ⧺
                show (denominator r) ⧺ "}$"
          in (denominator r ≠ 1) ==> (text === textExpected)
    ]

testToLatex =
  testCase "Tableau to Latex" $
    let tableau = Tableau {
      tabN = 6
    , tabM = 3
    , tabA = fromLists [ [8%5,0,0,1,0,-1%5]
                       , [16%5,0,1,0,1,-2%5]
                       , [2%5,1,1,0,0,1%5]
                       ]
    , tabB = V.fromList [8,16,6]
    , tabC = V.fromList [-1%5,0,3,0,0,2%5]
    , tabZ = 12
    , tabBasicVariables = V.fromList [4,5,6]
    , tabIndependantVariables = V.fromList [1,2,3]
    }

        textExpected =
          "\\begin{tabular}{c|cccccc}\n" ⧺
          "$8$ & $\\frac{8}{5}$ & $0$ & $0$ & $1$ & $0$ & $-\\frac{1}{5}$\\\\\n" ⧺
          "$16$ & $\\frac{16}{5}$ & $0$ & $1$ & $0$ & $1$ & $-\\frac{2}{5}$\\\\\n" ⧺
          "$6$ & $\\frac{2}{5}$ & $1$ & $1$ & $0$ & $0$ & $\\frac{1}{5}$\\\\\n" ⧺
          "\\hline\n" ⧺
          "$12$ & $-\\frac{1}{5}$ & $0$ & $3$ & $0$ & $0$ & $\\frac{2}{5}$\\\\\n" ⧺
          "\\end{tabular}"

        text = toLatex tableau

    in text @?= textExpected
