> {-# LANGUAGE UnicodeSyntax #-}

> module LinearProgramming.Tableau where

> import Prelude.Unicode
> import Numeric.Matrix hiding (map)
> import LinearProgramming.Matrix

> data Tableau = Tableau {
>   tabM ∷ Int
> , tabN ∷ Int
> , tabA ∷ Matrix Rational
> , tabB ∷ Matrix Rational
> , tabC ∷ Matrix Rational
> , tabZ ∷ Rational
> , tabBasicVariables ∷ [Int]
> , tabIndependantVariables ∷ [Int]
> }

> instance Show Tableau where
>   show Tableau {
>     tabM = m
>   , tabN = n
>   , tabA = a
>   , tabB = b
>   , tabC = c
>   , tabZ = z
>   , tabBasicVariables = basicVariables
>   , tabIndependantVariables = independantVariables
>   } =
>       let t1 = b <|> a
>           z' = fromList [[z]]
>           t2 = z' <|> c
>           t3 = t1 <-> t2
>       in show t3

> pivot ∷ Tableau → Int → Int → Tableau
> pivot t@(Tableau {
>     tabM = m
>   , tabN = n
>   , tabA = a
>   , tabB = b
>   , tabC = c
>   , tabZ = z
>   , tabBasicVariables = basicVariables
>   , tabIndependantVariables = independantVariables
>   }) i j =
>       let z' = z - (c `at` (1, i)) ⋅ (b `at` (j, 1)) ÷ (a `at` (j, i))
>
>           oldRows = zip [1..] $ rowVectors a
>           oldPreviousRows = take (j-1) oldRows
>           oldNextRows = drop j oldRows
>           newPreviousRows = map g oldPreviousRows
>           newNextRows = map g oldNextRows
>           newRows = newPreviousRows ⧺ [newRow] ⧺ newNextRows
>           a1 = a `at` (j, i)
>           a2 = rowVector a j
>           a3 = scale a2 (1 / a1)
>           newRow = a3
>           ej = fromList [map (\s → if s == i then 1 else 0) [1 .. numCols a]]
>           g (k, r) =
>               let oldA = a `at` (k, i)
>               in rowVector a k + scale (newRow - ej) oldA
>           a' = fromRowVectors newRows
>
>           oldBs = zip [1..] $ concat $ map (`row` b) [1..(numRows b)]
>           oldPreviousBs = take (j-1) oldBs
>           oldNextBs = drop j oldBs
>           newPreviousBs = map f oldPreviousBs
>           newNextBs = map f oldNextBs
>           newBs = newPreviousBs ⧺ [newB] ⧺ newNextBs
>           newB = (b `at` (j, 1)) ÷ (a `at` (j, i))
>           f (k, e) =
>               let oldA = a `at` (k, i)
>               in e + newB ⋅ oldA
>           b' = fromList (map (\e → [e]) newBs)
>       in t {tabZ = z', tabA = a', tabB = b'}
>
