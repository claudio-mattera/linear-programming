{-# LANGUAGE UnicodeSyntax #-}

module LinearProgramming.Parser (
    parseTableau
  , other
  ) where

import Prelude.Unicode

import Data.Matrix as M hiding ((<|>))
import Data.Vector as V hiding (map, foldl)
import Text.Parsec
--import Text.ParserCombinators.Parsec

import Control.Applicative ((<*>), (<*), (*>), (<$>), liftA2)
import Control.Arrow ((***))

import LinearProgramming.Tableau

data Relation = Equal
              | GreaterEqual
              | LesserEqual
              deriving (Show, Eq)

instance Read Relation where
  readsPrec _ r = case r of
    "<=" → [(LesserEqual, "")]
    "≤" → [(LesserEqual, "")]
    "=" → [(Equal, "")]
    ">=" → [(GreaterEqual, "")]
    "≥" → [(GreaterEqual, "")]

{-
max x1 + 2 * x2

-3 * x1 + x2 <= 2
x2 <= 11
x1 - x2 <= 3
x1 <=
-}

parseTableau ∷ String → Either String Tableau
parseTableau text = undefined

other ∷ String → Either ParseError RawTableau
other text = parse parser "" text

parser = do
  obj ← objectiveLine
  many1 newline
  cs ← constraintLine `sepEndBy` newline
  eof
  return (obj, cs)

objectiveLine = do
  f ← try (string "max") <|> string "min"
  e ← many1 whitespace *> expression
  return $ if f ≡ "min"
    then map (id *** (⋅(-1))) e
    else e

constraintLine = do
  e ← expression <* whitespaces
  r ← relationSymbol <* whitespaces
  n ← number
  return (e, r, n)

expression ∷ Parsec String () [(Int, Int)]
expression = between whitespaces whitespaces addend `sepBy1` string "+"

addend = do
  coefficient ← option 1 (number <* optional (char '*')) <* whitespaces
  v ← variable
  return (v, coefficient)

variable ∷ Parsec String () Int
variable = read <$> (char 'x' *> many1 digit)

number ∷ Parsec String () Int
number = do
  coefficient ← option 1 (char '-' *> return (-1))
  ds ← many1 digit
  return (coefficient ⋅ read ds)


relationSymbol ∷ Parsec String () Relation
relationSymbol =
  read <$> (string "=" <|> string "<=" <|> string ">=")


whitespace = oneOf " \t"
whitespaces = many whitespace





type RawTableau = ([(Int, Int)], [([(Int, Int)], Relation, Int)])

makeCanonical ∷ RawTableau → RawTableau
makeCanonical (obj, constraints) =
  let maxIndex = undefined
      slackVariablesCount = foldl (\c (_, r, _) → case r of
        Equal → c
        _ → c+1) 0 constraints
  in undefined
