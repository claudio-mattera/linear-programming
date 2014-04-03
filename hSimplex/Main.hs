{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode
import LinearProgramming.Tableau
import Data.Matrix as M
import Data.Vector as V
import Data.Ratio

main = do

  let tInitial = Tableau {
    tabN = 5
  , tabM = 2
  , tabA = fromLists [ [1%2,2,1,1,0]
                     , [1,  2,4,0,1]]
  , tabB = V.fromList [24,60]
  , tabC = V.fromList [6,14,13,0,0]
  , tabZ = 0
  , tabBasicVariables = V.fromList [6,7]
  , tabIndependantVariables = V.fromList [1,2,3,4,5]
  }

      tFinal = pivot tInitial 2 6

  print tFinal

  let k = Tableau {
    tabN = 3
  , tabM = 6
  , tabA = fromLists [ [2,1,1,1,0,0]
                     , [4,2,3,0,1,0]
                     , [2,5,5,0,0,1]]
  , tabB = V.fromList [14,28,30]
  , tabC = V.fromList [-1,-2,1,0,0,0]
  , tabZ = 0
  , tabBasicVariables = V.fromList [4,5,6]
  , tabIndependantVariables = V.fromList [1,2,3]
  }

      kFinal = Tableau {
    tabN = 3
  , tabM = 6
  , tabA = fromLists [ [8%5,0,0,1,0,-1%5]
                     , [16%5,0,1,0,1,-2%5]
                     , [2%5,1,1,0,0,1%5]]
  , tabB = V.fromList [8,16,6]
  , tabC = V.fromList [-1%5,0,3,0,0,2%5]
  , tabZ = 12
  , tabBasicVariables = V.fromList [4,5,6]
  , tabIndependantVariables = V.fromList [1,2,3]
  }

      k' = pivot k 2 6

  print k
  print k'
  print kFinal
