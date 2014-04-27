{-# LANGUAGE UnicodeSyntax #-}

module TestsVariables (tests) where

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Ratio

import LinearProgramming.Tableau

tests ∷ TestTree
tests = testGroup "Entering and leaving variables choice" $
        [ testGroup "Examples from Linear and Integer Programming course" $
            Prelude.map makeTestsFromSample samples
        ]

type Sample = (String, Tableau, Maybe Variable, Maybe Variable)

sample1 ∷ Sample
sample1 =
  let desc = "Lecture \"Pivoting\""

      tableau = Tableau {
      tabN = 3
    , tabM = 3
    , tabA = M.fromLists [ [ 2, -3,  1]
                         , [-4,  1,  2]
                         , [-3, -4,  2]
                         ]
    , tabB = V.fromList [7, 12, 9]
    , tabC = V.fromList [2, 2, -3]
    , tabZ = 10
    , tabBasicVariables = V.fromList [4,5,6]
    , tabIndependantVariables = V.fromList [1,2,3]
    , tabAuxiliaryData = Nothing
    }

      entering = Just 1
      leaving = Just 5

  in (desc, tableau, entering, leaving)


sample2 ∷ Sample
sample2 =
  let desc = "Lecture \"A Complete Example\" (1)"

      tableau = Tableau {
        tabN = 2
      , tabM = 4
      , tabA = M.fromLists [ [ 3, -1]
                           , [ 0, -1]
                           , [-1,  1]
                           , [-1,  0]
                           ]
      , tabB = V.fromList [2, 11, 3, 6]
      , tabC = V.fromList [1, 2]
      , tabZ = 0
      , tabBasicVariables = V.fromList [3,4,5,6]
      , tabIndependantVariables = V.fromList [1,2]
      , tabAuxiliaryData = Nothing
      }

      entering = Just 2
      leaving = Just 3

  in (desc, tableau, entering, leaving)


sample3 ∷ Sample
sample3 =
  let desc = "Lecture \"A Complete Example\" (2)"

      tableau = Tableau {
        tabN = 2
      , tabM = 4
      , tabA = M.fromLists [ [ 3, -1]
                           , [-3,  1]
                           , [ 2, -1]
                           , [-1,  0]
                           ]
      , tabB = V.fromList [2, 9, 5, 6]
      , tabC = V.fromList [7, -2]
      , tabZ = 4
      , tabBasicVariables = V.fromList [2,4,5,6]
      , tabIndependantVariables = V.fromList [1,3]
      , tabAuxiliaryData = Nothing
      }

      entering = Just 1
      leaving = Just 4

  in (desc, tableau, entering, leaving)

sample4 ∷ Sample
sample4 =
  let desc = "Lecture \"A Complete Example\" (3)"

      tableau = Tableau {
        tabN = 2
      , tabM = 4
      , tabA = M.fromLists [ [-1,    0]
                           , [-1%3,  1%3]
                           , [-2%3, -1%3]
                           , [ 1%3, -1%3]
                           ]
      , tabB = V.fromList [11,3,11,3]
      , tabC = V.fromList [-7%3, 1%3]
      , tabZ = 25
      , tabBasicVariables = V.fromList [2,1,5,6]
      , tabIndependantVariables = V.fromList [4,3]
      , tabAuxiliaryData = Nothing
      }

      entering = Just 3
      leaving = Just 6

  in (desc, tableau, entering, leaving)

sample5 ∷ Sample
sample5 =
  let desc = "Lecture \"Entering and leaving variable analysis\""

      tableau = Tableau {
        tabN = 3
      , tabM = 3
      , tabA = M.fromLists [ [-2, -3, -1]
                           , [-4, -1, -2]
                           , [-3, -4, -2]
                           ]
      , tabB = V.fromList [5,11,8]
      , tabC = V.fromList [5,4,3]
      , tabZ = 0
      , tabBasicVariables = V.fromList [4,5,6]
      , tabIndependantVariables = V.fromList [1,2,3]
      , tabAuxiliaryData = Nothing
      }

      entering = Just 1
      leaving = Just 4

  in (desc, tableau, entering, leaving)

sample6 ∷ Sample
sample6 =
  let desc = "Lecture \"Handling Unbounded Problems\" (1)"

      tableau = Tableau {
        tabN = 3
      , tabM = 4
      , tabA = M.fromLists [ [-1,  1,  0]
                           , [ 1,  0, -1]
                           , [ 2,  0, -1]
                           , [ 1, -1,  0]
                           ]
      , tabB = V.fromList [5,6,2,4]
      , tabC = V.fromList [2,3,-5]
      , tabZ = 0
      , tabBasicVariables = V.fromList [4,5,6,7]
      , tabIndependantVariables = V.fromList [1,2,3]
      , tabAuxiliaryData = Nothing
      }

      entering = Just 2
      leaving = Just 7

  in (desc, tableau, entering, leaving)

sample7 ∷ Sample
sample7 =
  let desc = "Lecture \"Handling Unbounded Problems\" (2)"

      tableau = Tableau {
        tabN = 3
      , tabM = 4
      , tabA = M.fromLists [ [ 0, -1,  0]
                           , [ 1,  0, -1]
                           , [ 2,  0, -1]
                           , [ 1, -1,  0]
                           ]
      , tabB = V.fromList [9,6,2,4]
      , tabC = V.fromList [5,-3,-5]
      , tabZ = 12
      , tabBasicVariables = V.fromList [4,5,6,2]
      , tabIndependantVariables = V.fromList [1,7,3]
      , tabAuxiliaryData = Nothing
      }

      entering = Just 1
      leaving = Nothing

  in (desc, tableau, entering, leaving)

sample8 ∷ Sample
sample8 =
  let desc = "Lecture \"Pivoting the Auxiliary Dictionary\" (1)"

      tableau = Tableau {
        tabN = 3
      , tabM = 4
      , tabA = M.fromLists [ [ 1, -2,  1]
                           , [ 1, -2,  0]
                           , [ 1, -3,  3]
                           , [ 1, -3,  1]
                           ]
      , tabB = V.fromList [2,6,0,6]
      , tabC = V.fromList [-1,2,-1]
      , tabZ = -2
      , tabBasicVariables = V.fromList [0,4,5,6]
      , tabIndependantVariables = V.fromList [3,1,2]
      , tabAuxiliaryData = Nothing
      }

      entering = Just 1
      leaving = Just 5

  in (desc, tableau, entering, leaving)

sample9 ∷ Sample
sample9 =
  let desc = "Lecture \"Pivoting the Auxiliary Dictionary\" (2)"

      tableau = Tableau {
        tabN = 3
      , tabM = 4
      , tabA = M.fromLists [ [ 1%3, -1%3,  1]
                           , [ 1%3,  2%3, -1]
                           , [ 1%3,  2%3, -2]
                           , [ 0,   -1,   -2]
                           ]
      , tabB = V.fromList [0,2,6,6]
      , tabC = V.fromList [-1%3,-2%3,1]
      , tabZ = -2
      , tabBasicVariables = V.fromList [1,0,4,6]
      , tabIndependantVariables = V.fromList [3,5,2]
      , tabAuxiliaryData = Nothing
      }

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
