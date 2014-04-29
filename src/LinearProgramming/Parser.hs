{-# LANGUAGE UnicodeSyntax #-}

{- | A parser to read LP problems in textual representation.

    > max x1 + 2 * x2
    >
    > -3 * x1 + x2 = 2
    > x2 <= 11
    > x1 - x2 >= 3
    > x1 <= 6

-}

module LinearProgramming.Parser (
    parseTableau
  , parseDirectTableau
  , parseProblem
  ) where

import Prelude.Unicode

import Text.Parsec

import Control.Applicative ((<*>), (<*), (*>), (<$>), liftA2)

import LinearProgramming.Tableau
import LinearProgramming.Problem


-- | Parses a 'Tableau' from a 'String'. It first parses to a 'Problem' using
--   'parseProblem', then converts it to a 'Tableau' using
--   'Problem.makeCanonical' and 'Problem.computeTableau'.
parseTableau ∷ String → Either ParseError Tableau
parseTableau text =
  parseProblem text >>= (return ∘ computeTableau ∘ makeCanonical)


-- | Parses a 'Problem' from a 'String'.
parseProblem ∷ String → Either ParseError Problem
parseProblem text = parse parser "" text


parser = do
  many newline
  (objType, obj) ← objectiveLine
  many1 newline
  cs ← constraintLine `sepEndBy` many1 newline
  many newline
  eof
  return (objType, obj, cs)

objectiveLine = do
  f ← try (string "max") <|> string "min"
  e ← many1 whitespace *> expression
  return (if f ≡ "min" then Minimize else Maximize, e)

constraintLine = do
  e ← expression <* whitespaces
  r ← relationSymbol <* whitespaces
  n ← integer
  return (e, r, n)

expression ∷ Parsec String () [(Int, Int)]
expression = between whitespaces whitespaces addend `sepBy1` string "+"

addend = do
  coefficient ← option 1 (integer <* optional (char '*')) <* whitespaces
  v ← variable
  return (v, coefficient)

variable ∷ Parsec String () Int
variable = read <$> (char 'x' *> many1 digit)


relationSymbol ∷ Parsec String () Relation
relationSymbol =
  read <$> (string "=" <|> string "<=" <|> string ">=")


whitespace = oneOf " \t"
whitespaces = many whitespace



integer ∷ Parsec String () Int
integer = do
  coefficient ← option 1 (char '-' *> return (-1))
  ds ← many1 digit
  return (coefficient ⋅ read ds)

-- Parses '123.456' as '123456 % 100'.
real ∷ Parsec String () Rational
real = do
  coefficient ← option 1 (char '-' *> return (-1))
  ds1 ← many1 digit
  char '.'
  ds2 ← many1 digit
  let ts = takeWhile (≠ '0') ds2
      k = fromIntegral (length ts)
      den = 10 ^^ k
      ds = ds1 ⧺ ts
      num = read ds ∷ Integer
  return (coefficient ⋅ fromIntegral num / den)

matrix m = count m (listOfReals <* newline)

listOf field = field `sepEndBy` whitespaces
listOfReals = listOf real
listOfIntegers = listOf integer

-- | Parses a 'Tableau' from a 'String'.
parseDirectTableau ∷ String → Either ParseError Tableau
parseDirectTableau text = parse parserDirectTableau "" text

parserDirectTableau = do
  many newline
  m ← integer <* whitespaces
  n ← integer <* newline
  vbs ← listOfIntegers <* newline
  vis ← listOfIntegers <* newline
  bs ← listOfReals <* newline
  ass ← matrix m
  z:cs ← listOfReals <* many newline
  eof
  return $ makeTableau n m ass bs cs z vbs vis
