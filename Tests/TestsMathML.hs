{-# LANGUAGE UnicodeSyntax #-}

module TestsMathML (tests) where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Ratio

import LinearProgramming.Tableau
import LinearProgramming.MathML

tests ∷ TestTree
tests = testGroup "Latex"
          [ testRationalToMathML
          , testTableauToMathML
          , testExtendedTableauToMathML
          ]

testRationalToMathML ∷ TestTree
testRationalToMathML =
  testGroup "Rational to MathML" $
    [ QC.testProperty "Positive whole number" $
        \n →
          let r = n % 1
              text = rationalToMathML r
              textExpected = "<mn>" ⧺ show n ⧺ "</mn>"
          in (n ≥ 0) ==> (text == textExpected)
    , QC.testProperty "Negative whole number" $
        \n →
          let r = n % 1
              text = rationalToMathML r
              textExpected = "<mo>-</mo><mn>" ⧺ show (-n) ⧺ "</mn>"
          in (n < 0) ==> (text == textExpected)
    , QC.testProperty "Positive rational" $
        \r' →
          let r = abs r'
              text = rationalToMathML r
              textExpected = "<mfrac><mn>" ⧺ show (numerator r) ⧺ "</mn><mn>" ⧺
                show (denominator r) ⧺ "</mn></mfrac>"
          in (denominator r ≠ 1) ==> (text == textExpected)
    , QC.testProperty "Negative rational" $
        \r' →
          let r = - (abs r')
              text = rationalToMathML r
              textExpected = "<mo>-</mo><mfrac><mn>" ⧺ show (- (numerator r)) ⧺
                "</mn><mn>" ⧺ show (denominator r) ⧺ "</mn></mfrac>"
          in (denominator r ≠ 1) ==> (text == textExpected)
    ]

testTableauToMathML ∷ TestTree
testTableauToMathML =
  testCase "Tableau to MathML" $
    let tableau = makeTableau
          3
          3
          [ [8%5,0,3]
          , [0,-1,1]
          , [4,-3,-3%4]
          ]
          [1,0,-3]
          [-1%5,0,1]
          0
          [4,5,6]
          [1,2,3]

        textExpected =
          "<mtable columnalign=\"right\" columnlines=\"solid none\" rowlines=\"none none solid\">\n" ⧺
          "<mtr><mtd><mn>1</mn></mtd><mtd><mfrac><mn>8</mn><mn>5</mn></mfrac></mtd><mtd></mtd><mtd><mn>3</mn></mtd></mtr>\n" ⧺
          "<mtr><mtd><mn>0</mn></mtd><mtd></mtd><mtd><mo>-</mo><mn>1</mn></mtd><mtd><mn>1</mn></mtd></mtr>\n" ⧺
          "<mtr><mtd><mo>-</mo><mn>3</mn></mtd><mtd><mn>4</mn></mtd><mtd><mo>-</mo><mn>3</mn></mtd><mtd><mo>-</mo><mfrac><mn>3</mn><mn>4</mn></mfrac></mtd></mtr>\n" ⧺
          "<mtr><mtd><mn>0</mn></mtd><mtd><mo>-</mo><mfrac><mn>1</mn><mn>5</mn></mfrac></mtd><mtd><mn>0</mn></mtd><mtd><mn>1</mn></mtd></mtr>\n" ⧺
          "</mtable>"

        text = tableauToMathML tableau

    in text @?= textExpected

testExtendedTableauToMathML ∷ TestTree
testExtendedTableauToMathML =
  testCase "Extended tableau to MathML" $
    let tableau = makeTableau
          3
          3
          [ [8%5,0,3]
          , [0,-1,1]
          , [4,-3,-3%4]
          ]
          [1,0,-3]
          [-1%5,0,1]
          0
          [4,5,6]
          [1,2,3]

        textExpected =
          "<mtable columnalign=\"right\" columnlines=\"none\" rowlines=\"none none solid\">\n" ⧺
          "<mtr><mtd><msub><mi>x</mi><mn>4</mn></msub><mo>=</mo></mtd><mtd><mn>1</mn></mtd>\n" ⧺
          "<mtd><mfrac><mn>8</mn><mn>5</mn></mfrac><msub><mi>x</mi><mn>1</mn></msub></mtd>\n" ⧺
          "<mtd></mtd>\n" ⧺
          "<mtd><mn>3</mn><msub><mi>x</mi><mn>3</mn></msub></mtd></mtr>\n" ⧺
          "<mtr><mtd><msub><mi>x</mi><mn>5</mn></msub><mo>=</mo></mtd><mtd></mtd>\n" ⧺
          "<mtd></mtd>\n" ⧺
          "<mtd><mo>-</mo><msub><mi>x</mi><mn>2</mn></msub></mtd>\n" ⧺
          "<mtd><msub><mi>x</mi><mn>3</mn></msub></mtd></mtr>\n" ⧺
          "<mtr><mtd><msub><mi>x</mi><mn>6</mn></msub><mo>=</mo></mtd><mtd><mo>-</mo><mn>3</mn></mtd>\n" ⧺
          "<mtd><mn>4</mn><msub><mi>x</mi><mn>1</mn></msub></mtd>\n" ⧺
          "<mtd><mo>-</mo><mn>3</mn><msub><mi>x</mi><mn>2</mn></msub></mtd>\n" ⧺
          "<mtd><mo>-</mo><mfrac><mn>3</mn><mn>4</mn></mfrac><msub><mi>x</mi><mn>3</mn></msub></mtd></mtr>\n" ⧺
          "<mtr><mtd><mi>z</mi><mo>=</mo></mtd><mtd></mtd><mtd><mo>-</mo><mfrac><mn>1</mn><mn>5</mn></mfrac><msub><mi>x</mi><mn>1</mn></msub></mtd><mtd></mtd><mtd><msub><mi>x</mi><mn>3</mn></msub></mtd></mtr>\n" ⧺
          "</mtable>"

        text = extendedTableauToMathML tableau

    in text @?= textExpected
