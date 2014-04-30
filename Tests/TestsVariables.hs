{-# LANGUAGE UnicodeSyntax #-}

module TestsVariables (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Ratio

import LinearProgramming.Tableau

tests ∷ TestTree
tests = testGroup "Entering and leaving variables choice"
        [ testGroup "Examples from Linear and Integer Programming course" $
            Prelude.map makeTestsFromSample samples
        ]

type Sample = (String, Tableau, Maybe Variable, Maybe Variable)

sample1 ∷ Sample
sample1 =
  let desc = "Lecture \"Pivoting\""

      tableau = makeTableau
          3
          3
          [ [ 2, -3,  1]
          , [-4,  1,  2]
          , [-3, -4,  2]
          ]
          [7, 12, 9]
          [2, 2, -3]
          0
          [4,5,6]
          [1,2,3]

      entering = Just 1
      leaving = Just 5

  in (desc, tableau, entering, leaving)


sample2 ∷ Sample
sample2 =
  let desc = "Lecture \"A Complete Example\" (1)"

      tableau = makeTableau
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

      entering = Just 2
      leaving = Just 3

  in (desc, tableau, entering, leaving)


sample3 ∷ Sample
sample3 =
  let desc = "Lecture \"A Complete Example\" (2)"

      tableau = makeTableau
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

      entering = Just 1
      leaving = Just 4

  in (desc, tableau, entering, leaving)

sample4 ∷ Sample
sample4 =
  let desc = "Lecture \"A Complete Example\" (3)"

      tableau = makeTableau
            2
            4
            [ [-1,    0]
            , [-1%3,  1%3]
            , [-2%3, -1%3]
            , [ 1%3, -1%3]
            ]
            [11,3,11,3]
            [-7%3, 1%3]
            5
            [2,1,5,6]
            [4,3]

      entering = Just 3
      leaving = Just 6

  in (desc, tableau, entering, leaving)

sample5 ∷ Sample
sample5 =
  let desc = "Lecture \"Entering and leaving variable analysis\""

      tableau = makeTableau
            3
            3
            [ [-2, -3, -1]
            , [-4, -1, -2]
            , [-3, -4, -2]
            ]
            [5,11,8]
            [5,4,3]
            0
            [4,5,6]
            [1,2,3]

      entering = Just 1
      leaving = Just 4

  in (desc, tableau, entering, leaving)

sample6 ∷ Sample
sample6 =
  let desc = "Lecture \"Handling Unbounded Problems\" (1)"

      tableau = makeTableau
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

      entering = Just 2
      leaving = Just 7

  in (desc, tableau, entering, leaving)

sample7 ∷ Sample
sample7 =
  let desc = "Lecture \"Handling Unbounded Problems\" (2)"

      tableau = makeTableau
            3
            4
            [ [ 0, -1,  0]
            , [ 1,  0, -1]
            , [ 2,  0, -1]
            , [ 1, -1,  0]
            ]
            [9,6,2,4]
            [5,-3,-5]
            2
            [4,5,6,2]
            [1,7,3]

      entering = Just 1
      leaving = Nothing

  in (desc, tableau, entering, leaving)

sample8 ∷ Sample
sample8 =
  let desc = "Lecture \"Pivoting the Auxiliary Dictionary\" (1)"

      tableau = makeTableau
            3
            4
            [ [ 1, -2,  1]
            , [ 1, -2,  0]
            , [ 1, -3,  3]
            , [ 1, -3,  1]
            ]
            [2,6,0,6]
            [-1,2,-1]
            (-2)
            [0,4,5,6]
            [3,1,2]

      entering = Just 1
      leaving = Just 5

  in (desc, tableau, entering, leaving)

sample9 ∷ Sample
sample9 =
  let desc = "Lecture \"Pivoting the Auxiliary Dictionary\" (2)"

      tableau = makeTableau
            3
            4
            [ [ 1%3, -1%3,  1]
            , [ 1%3,  2%3, -1]
            , [ 1%3,  2%3, -2]
            , [ 0,   -1,   -2]
            ]
            [0,2,6,6]
            [-1%3,-2%3,1]
            (-2)
            [1,0,4,6]
            [3,5,2]

      entering = Just 2
      leaving = Just 0

  in (desc, tableau, entering, leaving)


samples ∷ [Sample]
samples =
  [ sample1
  , sample2
  , sample3
  , sample4
  , sample5
  , sample6
  , sample7
  , sample8
  , sample9
  ]

makeTestsFromSample ∷ Sample → TestTree
makeTestsFromSample (desc, tableau, enteringExpected, leavingExpected) =
  testCase desc $
    let entering = chooseEnteringVariable tableau
    in case entering of
      Nothing       → entering @?= enteringExpected
      Just entering' →
        let leaving = chooseLeavingVariable tableau entering'
        in do
          entering @?= enteringExpected
          leaving @?= leavingExpected
