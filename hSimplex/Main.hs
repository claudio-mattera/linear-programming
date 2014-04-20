{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.DList as DL

import Data.Ratio

import Control.Monad (forM_)

import LinearProgramming.Tableau
import LinearProgramming.Simplex

main = do
  let tableau = Tableau {
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

      (result, history) = runSimplex tableau

  putStrLn "Result:"
  print result

  putStrLn ""
  putStrLn "History:"
  forM_ (DL.toList history) (\t → print t >> putStrLn "")
