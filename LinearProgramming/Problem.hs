{-# LANGUAGE UnicodeSyntax #-}

{- | An LP problem is given by an objective function, maximized either minimized,
     and a list of linear constraints.

    > min x1 + 2 * x2
    >
    > -3 * x1 + x2 = 2
    > x2 <= 11
    > x1 - x2 >= 3
    > x1 <= 6

    The problem can be expressed in its canonical form, i.e., a form in which
    the objective function is maximized and all the constraints are of type
    lesser or equal. Every LP problem is equivalent to a problem in canonical
    form.

    E.g. the previous problem's canonical form is

    > max -x1 - 2 * x2
    >
    > 3 * x1 - x2 <= -2
    > -3 * x1 + x2 <= 2
    > x2 <= 11
    > -x1 + x2 <= 3
    > x1 <= 6

    * Minimizing a function @f(x)@ is equivalent to maximizing @- f(x)@.

    * A constraint @g(x) = b@ is equivalent to @g(x) ≤ b@ ∧ @g(x) ≥ b@.

    * A constraint @g(x) ≥ b@ is equivalent to @- g(x) ≤ - b@.

    A problem in canonical form can be trivially converted to a tableau to
    be solved using the simplex method. E.g.

    >   -2  |   3  -1
    >    2  |  -3   1
    >   11  |   0   1
    >    3  |  -1   1
    >    6  |   1   0
    > ------+---------
    >    0  |  -1  -2
-}

module LinearProgramming.Problem (
    Problem(..)
  , CanonicalProblem(..)
  , ObjFuncType(..)
  , Relation(..)
  , Constraint(..)
  , CanonicalConstraint(..)
  , Coefficient(..)
  , computeTableau
  , makeCanonical
  ) where

import Prelude.Unicode

import qualified Data.Matrix as M
import qualified Data.Vector as V

import Control.Arrow ((***))

import Data.Ratio
import Data.List (maximumBy)

import LinearProgramming.Tableau


-- | A LP program human readable representation.
type Problem = (ObjFuncType, [Coefficient], [Constraint])

-- | A LP program human readable representation in canonical form, i.e., a form
--   in which the objective function is maximized and all constraints relations
--   are of type 'LesserEqual'.
type CanonicalProblem = ([Coefficient], [CanonicalConstraint])

-- | A constraint relation, i.e., equal, greater equal or lesser equal.
data Relation = Equal
              | GreaterEqual
              | LesserEqual
              deriving (Show, Eq)

-- | Objective function direction, i.e., maximization or minimization.
data ObjFuncType = Maximize
                 | Minimize
                 deriving (Show, Eq)

-- | The value of a coefficient.
type Coefficient = (Int, Int)

-- | A constraint is a linear combination of the problem's variable in relation
--   to a value.
type Constraint = ([Coefficient], Relation, Int)

-- | A canonical constraint is a linear combination of the problem's variable
--   lesser or equal a value.
type CanonicalConstraint = ([Coefficient], Int)


instance Read Relation where
  readsPrec _ r = case r of
    "<=" → [(LesserEqual, "")]
    "≤" → [(LesserEqual, "")]
    "=" → [(Equal, "")]
    ">=" → [(GreaterEqual, "")]
    "≥" → [(GreaterEqual, "")]


-- | Convert a 'Problem' to its canonical form.
makeCanonical ∷ Problem → CanonicalProblem
makeCanonical (objType, obj, constraints) =
  let (objType', obj') =
        if objType ≡ Minimize
        then (Maximize, negateCoefficients obj)
        else (objType, obj)

      constraints' = map toCanonical ∘ convertGreaterConstraints ∘
        convertEqualityConstraints $ constraints
  in (obj', constraints')
  where
  convertEqualityConstraints = foldl convertEqualityConstraint []
  convertGreaterConstraints = foldl convertGreaterConstraint []

  -- Convert equality constraints in double inequality constraints
  -- x ≡ b   ⇒   x ≤ b ∧ x ≥ b
  convertEqualityConstraint ∷ [Constraint] → Constraint → [Constraint]
  convertEqualityConstraint constraints (as, Equal, b) =
    (as, LesserEqual, b) : (as, GreaterEqual, b) : constraints
  convertEqualityConstraint constraints c = c : constraints

  -- Convert greater inequality constraints in negated lesser inequality
  -- constraints
  -- x ≥ b   ⇒   -x ≤ -b
  convertGreaterConstraint ∷ [Constraint] → Constraint → [Constraint]
  convertGreaterConstraint constraints (as, GreaterEqual, b) =
    let as' = negateCoefficients as
    in (as', LesserEqual, -b) : constraints
  convertGreaterConstraint constraints c = c : constraints

  toCanonical ∷ Constraint → CanonicalConstraint
  toCanonical (as, _, b) = (as, b)


-- | Convert a 'CanonicalProblem' to a 'Tableau'.
computeTableau ∷ CanonicalProblem → Tableau
computeTableau (obj, constraints) =
  let toVariableIndex (cs, _) = maximum (map fst cs)
      n = maximum (map toVariableIndex constraints)
      m = length constraints
      slackVariables = map (+n) [1..m]

      generateLine (cs, _) = V.generate n ∘ safeGet 0 ∘ negateCoefficients $ cs

      a = foldl1 (M.<->) $ map (M.rowVector ∘ generateLine) constraints

      bs = map (\(_, bj) → bj) constraints
      b = V.generate m (\j → bs !! j)

      c = V.generate n (safeGet 0 obj)

      z = 0

  in Tableau {
    tabM = m
  , tabN = n
  , tabA = fmap (\i → fromIntegral i % 1) a
  , tabB = fmap (\i → fromIntegral i % 1) b
  , tabC = fmap (\i → fromIntegral i % 1) c
  , tabZ = z
  , tabBasicVariables = V.fromList slackVariables
  , tabIndependantVariables = V.fromList [1..n]
  }

  where

  safeGet ∷ b → [(Int, b)] → Int → b
  safeGet def v i =
    case lookup (i+1) v of
      Just r  → r
      Nothing → def


negateCoefficients ∷ [Coefficient] → [Coefficient]
negateCoefficients = map (id *** (⋅(-1)))

