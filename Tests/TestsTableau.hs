{-# LANGUAGE UnicodeSyntax #-}

module TestsTableau (tests) where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Test.QuickCheck

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Ratio

import LinearProgramming.Tableau

tests ∷ TestTree
tests = testGroup "Tableau" $
        [ testGroup "Examples from Linear and Integer Programming course" $
            map makeTestsFromSample samples
        , qcProps
        , testsInstances
        ]

qcProps ∷ TestTree
qcProps = testGroup "Properties"
  [ QC.testProperty "Tableau with all non-negative bs are feasible" $
      \feasibleTableau →
        let t@Tableau{tabB = b} = unFeasibleTableau feasibleTableau
        in V.all (≥ 0) b ==> isFeasible t
  , QC.testProperty "Tableau with negative bs are infeasible" $
      \t@Tableau{tabB = b} → V.any (< 0) b ==> not (isFeasible t)
  , QC.testProperty "Tableau with negative cs are non final" $
      \t@Tableau{tabC = c} → V.any (> 0) c ==> not (isFinal t)
  , QC.testProperty "Pivoting does not decrease objective" $
      \feasibleTableau →
        let tableau@Tableau{tabZ = z} = unFeasibleTableau feasibleTableau
        in isFeasible tableau ∧ not (isFinal tableau) ==>
          let Just entering = chooseEnteringVariable tableau
          in case chooseLeavingVariable tableau entering of
            Nothing → True -- Accept if the tableau is unbounded
            Just leaving →
              let Tableau {tabZ = z'} = pivot tableau entering leaving
              in z' ≥ z
  ]


type Sample = (String, Tableau, Tableau, Variable, Variable)

sample1 ∷ Sample
sample1 =
  let desc = "Lecture \"A Complete Example\""

      tInitial = makeTableau
          2
          4
          [ [ 3, -1]
          , [ 0, -1]
          , [-1,  1]
          , [-1,  0]
          ]
          [2, 11, 3, 6]
          [1, 2]
          0
          [3,4,5,6]
          [1,2]

      tExpected = makeTableau
          2
          4
          [ [ 3, -1]
          , [-3,  1]
          , [ 2, -1]
          , [-1,  0]
          ]
          [2, 9, 5, 6]
          [7, -2]
          4
          [2,4,5,6]
          [1,3]

      entering = 2
      leaving = 3

  in (desc, tInitial, tExpected, entering, leaving)


sample2 ∷ Sample
sample2 =
  let desc = "Lecture \"Pivoting\""

      tInitial = makeTableau
          3
          3
          [ [-2, -3, -1]
          , [-4, -1, -2]
          , [-3, -4, -2]
          ]
          [5, 11, 8]
          [5, 4, 3]
          0
          [4,5,6]
          [1,2,3]

      tExpected = makeTableau
          3
          3
          [ [-1%2, -3%2, -1%2]
          , [ 2,    5,    0]
          , [ 3%2,  1%2, -1%2]
          ]
          [5%2, 1, 1%2]
          [-5%2, -7%2, 1%2]
          (25%2)
          [1,5,6]
          [4,2,3]

      entering = 1
      leaving = 4

  in (desc, tInitial, tExpected, entering, leaving)

sample3 ∷ Sample
sample3 =
  let desc = "Lecture \"Infeasible problem example\""

      tInitial = makeTableau
          4
          4
          [ [ 1, -1,  1,  0]
          , [ 1,  0, -1, -1]
          , [ 1,  1,  0, -1]
          , [ 1,  0,  0,  1]
          ]
          [5, 14, -6, -7]
          [-1, 0, 0, 0]
          0
          [4,5,6,7]
          [0,1,2,3]

      tExpected = makeTableau
          4
          4
          [ [ 1, -1,  1, -1]
          , [ 1,  0, -1, -2]
          , [ 1,  1,  0, -2]
          , [ 1,  0,  0, -1]
          ]
          [12, 21, 1, 7]
          [-1, 0, 0, 1]
          (-7)
          [4,5,6,0]
          [7,1,2,3]

      entering = 0
      leaving = 7

  in (desc, tInitial, tExpected, entering, leaving)

sample4 ∷ Sample
sample4 =
  let desc = "Lecture \"Handling Unbounded Problems\""

      tInitial = makeTableau
            3
            4
            [ [-1,  1,  0]
            , [ 1,  0, -1]
            , [ 2,  0, -1]
            , [ 1, -1,  0]
            ]
            [5,6,2,4]
            [2,3,-5]
            0
            [4,5,6,7]
            [1,2,3]

      tExpected = makeTableau
            3
            4
            [ [ 0, -1,  0]
            , [ 1,  0, -1]
            , [ 2,  0, -1]
            , [ 1, -1,  0]
            ]
            [9,6,2,4]
            [5,-3,-5]
            12
            [4,5,6,2]
            [1,7,3]

      entering = 2
      leaving = 7

  in (desc, tInitial, tExpected, entering, leaving)


samples ∷ [Sample]
samples =
  [ sample1
  , sample2
  , sample3
  , sample4
  ]

makeTestsFromSample ∷ Sample → TestTree
makeTestsFromSample (desc, tInitial, tExpected, entering, leaving) =
  testGroup desc
          [ testObjectiveUpdate
          , testBJ
          , testB
          , testRowJ
          , testA
          , testC
          , testPivot
          ]
  where

  tFinal = pivot tInitial entering leaving

  testPivot =
    testCase "Simple pivot" $
      tFinal @?= tExpected

  testA =
    testCase "A update" $
      let a = tabA tFinal
          aExpected = tabA tExpected

      in a @?= aExpected

  testRowJ =
    testCase "Row j update" $
      let a = tabA tFinal
          aExpected = tabA tExpected
          aj = M.getRow 1 a
          ajExpected = M.getRow 1 aExpected

      in aj @?= ajExpected

  testB =
    testCase "B update" $
      let b = tabB tFinal
          bExpected = tabB tExpected

      in b @?= bExpected

  testC =
    testCase "C update" $
      let c = tabC tFinal
          cExpected = tabC tExpected

      in c @?= cExpected


  testBJ =
    testCase "Bj update" $
      let b = tabB tFinal
          bExpected = tabB tExpected
          bj = b V.! 0
          bjExpected = bExpected V.! 0

      in bj @?= bjExpected


  testObjectiveUpdate =
    testCase "Objective value update" $
      let z = tabZ tFinal
          zExpected = tabZ tExpected
      in z @?= zExpected




arbitraryTableau ∷ Int → Gen Tableau
arbitraryTableau size = do
    n ← choose (2, size)
    m ← choose (2, size)
    as ← vector (m*n)
    let ass = group n as
    bs ← vector m
    cs ← vector n
    z ← arbitrary
    let t = makeTableau
              n
              m
              ass
              bs
              cs
              z
              [n+1 .. n+m]
              [1..n]
    return t

  where

  group :: Int → [a] → [[a]]
  group _ [] = []
  group n l
    | n > 0 = (take n l) : (group n (drop n l))
    | otherwise = error "Negative n"

instance Arbitrary Tableau where
  arbitrary = sized arbitraryTableau


newtype FeasibleTableau = FeasibleTableau { unFeasibleTableau ∷ Tableau } deriving (Eq, Show)

instance Arbitrary FeasibleTableau where
  arbitrary = do
    tableau ← arbitrary
    let b' = V.map abs $ tabB tableau
    return (FeasibleTableau tableau {
      tabB = b'
    })



testsInstances ∷ TestTree
testsInstances = testGroup "Instances" [
                      showIntegerNonOneCoefficients
                    , showIntegerOneCoefficients
                    , showIntegerZeroCoefficients
                    , showRationalCoefficients
                    , showAuxiliaryData
                    ]

  where

  removeParsAndSpaces ∷ String → String
  removeParsAndSpaces (' ' : ' ' : ys) = removeParsAndSpaces (' ' : ys)
  removeParsAndSpaces ('(' : ys) = removeParsAndSpaces ys
  removeParsAndSpaces (')' : ys) = removeParsAndSpaces ys
  removeParsAndSpaces (' ' : []) = []
  removeParsAndSpaces (x:ys) = x : removeParsAndSpaces ys
  removeParsAndSpaces [] = []

  trim ∷ String → String
  trim xs = helper $ dropWhile (≡ ' ') xs
    where
    helper = reverse ∘ dropWhile (≡ ' ') ∘ reverse

  purge ∷ String → String
  purge = unlines ∘ filter (not ∘ null) ∘ map (trim ∘ removeParsAndSpaces) ∘ lines

  showIntegerNonOneCoefficients = testCase "Show integer non 1 coefficients" $
    let tableau = makeTableau
          2
          4
          [ [ 3, -2]
          , [ 7, -2]
          , [-2,  2]
          , [-2,  8]
          ]
          [2, 11, 3, 6]
          [5, 2]
          0
          [3,4,5,6]
          [1,2]

        sExpected = "x3 =  2  3 x1  -2 x2\n" ⧺
                    "x4 = 11  7 x1  -2 x2\n" ⧺
                    "x5 =  3 -2 x1   2 x2\n" ⧺
                    "x6 =  6 -2 x1   8 x2\n" ⧺
                    "z  =  0  5 x1   2 x2"
        s = show tableau

        sExpected' = purge sExpected
        s' = purge s

    in s' @?= sExpected'


  showIntegerOneCoefficients = testCase "Show integer 1 coefficients" $
    let tableau = makeTableau
          2
          4
          [ [ 3, -1]
          , [ 7, -1]
          , [-2,  1]
          , [-2,  8]
          ]
          [2, 11, 3, 6]
          [1, 2]
          0
          [3,4,5,6]
          [1,2]

        sExpected = "x3 =  2  3 x1    -x2\n" ⧺
                    "x4 = 11  7 x1    -x2\n" ⧺
                    "x5 =  3 -2 x1     x2\n" ⧺
                    "x6 =  6 -2 x1   8 x2\n" ⧺
                    "z  =  0    x1   2 x2"
        s = show tableau

        sExpected' = purge sExpected
        s' = purge s

    in s' @?= sExpected'


  showIntegerZeroCoefficients = testCase "Show integer 0 coefficients" $
    let tableau = makeTableau
          2
          4
          [ [ 3, -1]
          , [ 0, -1]
          , [-2,  1]
          , [-2,  0]
          ]
          [2, 11, 0, 6]
          [1, 0]
          0
          [3,4,5,6]
          [1,2]

        sExpected = "x3 =  2  3 x1    -x2\n" ⧺
                    "x4 = 11          -x2\n" ⧺
                    "x5 =  0 -2 x1     x2\n" ⧺
                    "x6 =  6 -2 x1\n" ⧺
                    "z  =  0    x1"
        s = show tableau

        sExpected' = purge sExpected
        s' = purge s

    in s' @?= sExpected'


  showRationalCoefficients = testCase "Show rational coefficients" $
    let tableau = makeTableau
          2
          4
          [ [ 3%4, -1]
          , [ 0, -1/12]
          , [-2,  1]
          , [-2/5,  0]
          ]
          [2, 11/9, 0, 6]
          [1, 0]
          0
          [3,4,5,6]
          [1,2]

        sExpected = "x3 =  2    3/4 x1       -x2\n" ⧺
                    "x4 = 11/9          -1/12 x2\n" ⧺
                    "x5 =  0   -2   x1        x2\n" ⧺
                    "x6 =  6   -2/5 x1\n" ⧺
                    "z  =  0        x1"
        s = show tableau

        sExpected' = purge sExpected
        s' = purge s

    in s' @?= sExpected'


  showAuxiliaryData = testCase "Show auxiliary data" $
    let tableau = makeTableau'
          2
          4
          [ [ 3%4, -1]
          , [ 0, -1/12]
          , [-2,  1]
          , [-2/5,  0]
          ]
          [2, 11/9, 0, 6]
          [1, 0]
          0
          [3,4,5,6]
          [1,2]
          (Just (4, [1%2, 0]))

        sExpected = "x3    =  2    3/4 x1       -x2\n" ⧺
                    "x4    = 11/9          -1/12 x2\n" ⧺
                    "x5    =  0   -2   x1        x2\n" ⧺
                    "x6    =  6   -2/5 x1\n" ⧺
                    "z     =  0        x1\n" ⧺
                    "aux Z =  4    1/2 x1"
        s = show tableau

        sExpected' = purge sExpected
        s' = purge s

    in s' @?= sExpected'
