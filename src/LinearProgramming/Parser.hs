{-# LANGUAGE UnicodeSyntax #-}

{- | A parser to read LP problems in textual representation. The input can
     be in human readable mathematical form ('parseProblem', 'parseTableau'):

    > max x1 + 2 * x2
    >
    > -3 * x1 + x2 = 2
    > x2 <= 11
    > x1 - x2 >= 3
    > x1 <= 6

    or a direct representation of a tableau ('parseDirectTableau'):

    > 3 4
    > 1 3 6
    > 2 4 5 7
    > 1.5 3.0 0.0
    > 0.0 0.0 -1.0 -2.0
    > 1.0 -1.0 0.0 -1.0
    > -1.0 0.9 -2.0 0.0
    > 1.0 -1.0  2.0 3.0 1.0

    which must follow this format:

    > m n
    > basic variables indices (m values)
    > independent variables indices (n values)
    > b coefficients (m values)
    > 1st row of matrix A (n values)
    > 2nd row of matrix A (n values)
    > ...
    > mth row of matrix A (n values)
    > z and c coefficients (1 + n values)


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
parseProblem = parse problem ""

-- | Parses a 'Tableau' from a 'String'.
parseDirectTableau ∷ String → Either ParseError Tableau
parseDirectTableau = parse directTableau ""


problem ∷ Parsec String () Problem
problem = do
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
  n ← number
  return (e, r, n)

expression ∷ Parsec String () [Coefficient]
expression = between whitespaces whitespaces addend `sepBy1` string "+"

addend ∷ Parsec String () Coefficient
addend = do
  coefficient ← option 1 (number <* optional (char '*')) <* whitespaces
  v ← variable
  return (v, coefficient)

variable ∷ Parsec String () Int
variable = read <$> (char 'x' *> many1 digit)

relationSymbol ∷ Parsec String () Relation
relationSymbol =
  read <$> (string "=" <|> string "<=" <|> string ">=" <|> string "≥" <|> string "≤")

whitespace ∷ Parsec String () Char
whitespace = oneOf " \t"

whitespaces ∷ Parsec String () String
whitespaces = many whitespace

number ∷ Parsec String () Rational
number = try rational <|> (integer >>= return ∘ fromIntegral)

integer ∷ Parsec String () Int
integer = do
  coefficient ← option 1 (char '-' *> return (-1))
  ds ← many1 digit
  return (coefficient ⋅ read ds)

-- Parses '123.456' as '123456 % 100'.
rational ∷ Parsec String () Rational
rational = do
  coefficient ← option 1 (char '-' *> return (-1))
  ds1 ← many1 digit
  _ ← char '.'
  ds2 ← many1 digit
  let ts = takeWhile (≠ '0') ds2
      k = fromIntegral (length ts) ∷ Integer
      den = 10 ^^ k ∷ Rational
      ds = dropWhile (≡ '0') ds1 ⧺ ts
      num = fromIntegral (read ds ∷ Integer)
  return (if null ds
          then 0
          else coefficient ⋅ num / den
          )

matrixOfNumbers ∷ Int → Parsec String () [[Rational]]
matrixOfNumbers m = count m (listOfNumbers <* newline)

listOf ∷ Parsec String () a → Parsec String () [a]
listOf field = field `sepEndBy` whitespaces

listOfNumbers ∷ Parsec String () [Rational]
listOfNumbers = listOf number

listOfIntegers ∷ Parsec String () [Int]
listOfIntegers = listOf integer

directTableau ∷ Parsec String () Tableau
directTableau = do
  skipMany newline
  m ← integer <* whitespaces
  n ← integer <* newline
  vbs ← listOfIntegers <* newline
  vis ← listOfIntegers <* newline
  bs ← listOfNumbers <* newline
  ass ← matrixOfNumbers m
  z:cs ← listOfNumbers <* many newline
  eof
  return $ makeTableau n m ass bs cs z vbs vis
