> {-# LANGUAGE UnicodeSyntax #-}

> module LinearProgramming.Tableau where

> import Prelude hiding (all, any)
> import Prelude.Unicode
> import Data.Matrix
> import Data.Foldable

> data Tableau = Tableau {
>   tabM ∷ Int
> , tabN ∷ Int
> , tabA ∷ Matrix Rational
> , tabB ∷ Matrix Rational
> , tabC ∷ Matrix Rational
> , tabZ ∷ Rational
> , tabBasicVariables ∷ [Int]
> , tabIndependantVariables ∷ [Int]
> } deriving Eq

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
>           z' = fromLists [[z]]
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
>       let z' = z - (c ! (1, i)) ⋅ (b ! (j, 1)) ÷ (a ! (j, i))
>           a' = undefined
>           b' = undefined
>           c' = undefined
>       in t {
>         tabZ = z'
>       , tabA = a'
>       , tabB = b'
>       , tabC = c'
>       }

> isFeasible ∷ Tableau → Bool
> isFeasible Tableau {tabB = b} = all (≥ 0) b

> isOptimal ∷ Tableau → Bool
> isOptimal Tableau {tabC = c} = all (≥ 0) c

> chooseEnteringVariable ∷ Tableau → Int
> chooseEnteringVariable = undefined

> chooseLeavingVariable ∷ Tableau → Int → Int
> chooseLeavingVariable = undefined
