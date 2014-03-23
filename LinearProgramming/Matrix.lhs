> {-# LANGUAGE UnicodeSyntax #-}

> module LinearProgramming.Matrix where

> import Prelude.Unicode
> import Numeric.Matrix hiding (map)

> rowVector ∷ MatrixElement e ⇒ Matrix e → Int → Matrix e
> rowVector m i =
>   let list = toList m
>       row = list !! (i - 1)
>   in fromList [row]

> rowVectors ∷ MatrixElement e ⇒ Matrix e → [Matrix e]
> rowVectors m = map (rowVector m) [1..numRows m]

> fromRowVectors ∷ MatrixElement e ⇒ [Matrix e] → Matrix e
> fromRowVectors rs = fromList (map (concat ∘ toList) rs)
