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

import Control.Applicative ((<*), (*>), (<$>))

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
parseProblem = parse parser ""

parser ∷ Parsec String () Problem
parser = do
  skipMany newline
  (objType, obj) ← objectiveLine
  skipMany1 newline
  cs ← constraintLine `sepEndBy` many1 newline
  skipMany newline
  eof
  return (objType, obj, cs)

objectiveLine ∷ Parsec String () (ObjFuncType, [Coefficient])
objectiveLine = do
  f ← try (string "max") <|> string "min"
  e ← many1 whitespace *> expression
  return (if f ≡ "min" then Minimize else Maximize, e)

constraintLine ∷ Parsec String () Constraint
constraintLine = do
  e ← expression <* whitespaces
  r ← relationSymbol <* whitespaces
  n ← integer
  return (e, r, n)

expression ∷ Parsec String () [(Int, Int)]
expression = between whitespaces whitespaces addend `sepBy1` string "+"

addend ∷ Parsec String () Coefficient
addend = do
  coefficient ← option 1 (integer <* optional (char '*')) <* whitespaces
  v ← variable
  return (v, coefficient)

variable ∷ Parsec String () Int
variable = read <$> (char 'x' *> many1 digit)


relationSymbol ∷ Parsec String () Relation
relationSymbol =
  read <$> (string "=" <|> string "<=" <|> string ">=")


whitespace ∷ Parsec String () Char
whitespace = oneOf " \t"

whitespaces ∷ Parsec String () String
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
  _ ← char '.'
  ds2 ← many1 digit
  let ts = takeWhile (≠ '0') ds2
      k = fromIntegral (length ts)
      den = 10 ^^ k
      ds = ds1 ⧺ ts
      num = read ds ∷ Integer
  return (coefficient ⋅ fromIntegral num / den)

matrix ∷ Int → Parsec String () [[Rational]]
matrix m = count m (listOfReals <* newline)

listOf ∷ Parsec String () a → Parsec String () [a]
listOf field = field `sepEndBy` whitespaces

listOfReals ∷ Parsec String () [Rational]
listOfReals = listOf real

listOfIntegers ∷ Parsec String () [Int]
listOfIntegers = listOf integer

-- | Parses a 'Tableau' from a 'String'.
parseDirectTableau ∷ String → Either ParseError Tableau
parseDirectTableau = parse parserDirectTableau ""

parserDirectTableau ∷ Parsec String () Tableau
parserDirectTableau = do
  skipMany newline
  m ← integer <* whitespaces
  n ← integer <* newline
  vbs ← listOfIntegers <* newline
  vis ← listOfIntegers <* newline
  bs ← listOfReals <* newline
  ass ← matrix m
  z:cs ← listOfReals <* many newline
  eof
  return $ makeTableau n m ass bs cs z vbs vis
