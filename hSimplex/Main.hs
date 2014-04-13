{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode
import LinearProgramming.Tableau
import LinearProgramming.Latex
import Data.Matrix as M
import Data.Vector as V
import Data.Ratio

sample1 =
    let tInitial = Tableau {
      tabN = 2
    , tabM = 4
    , tabA = fromLists [ [ 3, -1]
                       , [ 0, -1]
                       , [-1,  1]
                       , [-1,  0]
                       ]
    , tabB = V.fromList [2, 11, 3, 6]
    , tabC = V.fromList [1, 2]
    , tabZ = 0
    , tabBasicVariables = V.fromList [3,4,5,6]
    , tabIndependantVariables = V.fromList [1,2]
    }

        tExpected = Tableau {
      tabN = 2
    , tabM = 4
    , tabA = fromLists [ [ 3, -1]
                       , [-3,  1]
                       , [ 2, -1]
                       , [-1,  0]
                       ]
    , tabB = V.fromList [2, 9, 5, 6]
    , tabC = V.fromList [7, -2]
    , tabZ = 4
    , tabBasicVariables = V.fromList [2,4,5,6]
    , tabIndependantVariables = V.fromList [1,3]
    }

        entering = 2
        leaving = 3

        tFinal = pivot tInitial entering leaving

  in (tInitial, tExpected, entering, leaving)



f sample = do
  let (tInitial, tExpected, entering, leaving) = sample
      Just i = V.elemIndex entering (tabIndependantVariables tInitial)
      Just j = V.elemIndex leaving (tabBasicVariables tInitial)
      bj = (tabB tInitial) V.! j
      ci = (tabC tInitial) V.! i
      aji = (tabA tInitial) M.! (j+1, i+1)
      rows_range = [0..j-1] ⧺ [j+1..(tabM tInitial)-1]
      tFinal = pivot tInitial entering leaving

  putStrLn "Initial: "
  putStrLn ("n: " ⧺ show (tabN tInitial))
  putStrLn ("m: " ⧺ show (tabM tInitial))
  print tInitial
  putStrLn ("Entering variable: " ⧺ show entering ⧺ " (0-index: " ⧺ show i ⧺ ")")
  putStrLn ("Leaving variable: " ⧺ show leaving ⧺ " (0-index: " ⧺ show j ⧺ ")")
  putStrLn ("bj: " ⧺ show bj)
  putStrLn ("ci: " ⧺ show ci)
  putStrLn ("aji: " ⧺ show aji)
  putStrLn ("Rows range: " ⧺ show rows_range)
  putStrLn "Expected: "
  print tExpected
  putStrLn "Final: "
  print tFinal
  putStrLn (toLatex tFinal)

main = do
  f sample1
