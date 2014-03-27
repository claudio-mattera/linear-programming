> {-# LANGUAGE UnicodeSyntax #-}

> import Prelude.Unicode
> import LinearProgramming.Tableau
> import Data.Matrix
> import Data.Ratio

> main = do
>   putStr "Hi"
>   putStrLn " World"
>
>   let k = Tableau {
>     tabN = 3
>   , tabM = 6
>   , tabA = fromLists [ [2,1,1,1,0,0]
>                     , [4,2,3,0,1,0]
>                     , [2,5,5,0,0,1]]
>   , tabB = fromLists [[14],[28],[30]]
>   , tabC = fromLists [[-1,-2,1,0,0,0]]
>   , tabZ = 0
>   , tabBasicVariables = [4,5,6]
>   , tabIndependantVariables = [1,2,3]
>   }
>
>       kFinal = Tableau {
>     tabN = 3
>   , tabM = 6
>   , tabA = fromLists [ [8%5,0,0,1,0,-1%5]
>                     , [16%5,0,1,0,1,-2%5]
>                     , [2%5,1,1,0,0,1%5]]
>   , tabB = fromLists [[8],[16],[6]]
>   , tabC = fromLists [[-1%5,0,3,0,0,2%5]]
>   , tabZ = 12
>   , tabBasicVariables = [4,5,6]
>   , tabIndependantVariables = [1,2,3]
>   }
>
>       k' = pivot k 2 3
>
>   print k
>   print k'
>   print kFinal
