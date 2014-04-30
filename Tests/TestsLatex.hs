{-# LANGUAGE UnicodeSyntax #-}

module TestsLatex (tests) where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Ratio

import LinearProgramming.Tableau
import LinearProgramming.Latex

tests ∷ TestTree
tests = testGroup "Latex"
          [ testShowRational
          , testToLatex
          ]

testShowRational ∷ TestTree
testShowRational =
  testGroup "Rational to Latex" $
    [ QC.testProperty "Whole number" $
        \n →
          let r = n % 1
              text = showRational r
              textExpected = "$" ⧺ show n ⧺ "$"
          in (text == textExpected)
    , QC.testProperty "Positive rational" $
        \r' →
          let r = abs r'
              text = showRational r
              textExpected = "$\\frac{" ⧺ show (numerator r) ⧺ "}{" ⧺
                show (denominator r) ⧺ "}$"
          in (denominator r ≠ 1) ==> (text == textExpected)
    , QC.testProperty "Negative rational" $
        \r' →
          let r = - (abs r')
              text = showRational r
              textExpected = "$-\\frac{" ⧺ show (- (numerator r)) ⧺ "}{" ⧺
                show (denominator r) ⧺ "}$"
          in (denominator r ≠ 1) ==> (text == textExpected)
    ]

testToLatex ∷ TestTree
testToLatex =
  testCase "Tableau to Latex" $
    let tableau = makeTableau
          6
          3
          [ [8%5,0,0,1,0,-1%5]
          , [16%5,0,1,0,1,-2%5]
          , [2%5,1,1,0,0,1%5]
          ]
          [8,16,6]
          [-1%5,0,3,0,0,2%5]
          12
          [4,5,6]
          [1,2,3]

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
