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
    Problem
  , CanonicalProblem
  , ObjFuncType(..)
  , Relation(..)
  , Constraint
  , CanonicalConstraint
  , Coefficient
  , computeTableau
  , makeCanonical
  ) where

import Prelude.Unicode

import Data.Maybe (fromMaybe)

import Control.Arrow (second)

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
type Coefficient = (Int, Value)

-- | A constraint is a linear combination of the problem's variable in relation
--   to a value.
type Constraint = ([Coefficient], Relation, Value)

-- | A canonical constraint is a linear combination of the problem's variable
--   lesser or equal a value.
type CanonicalConstraint = ([Coefficient], Value)


instance Read Relation where
  readsPrec _ r = case r of
    "<=" → [(LesserEqual, "")]
    "≤" → [(LesserEqual, "")]
    "=" → [(Equal, "")]
    ">=" → [(GreaterEqual, "")]
    "≥" → [(GreaterEqual, "")]
    _ → []


-- | Converts a 'Problem' to its canonical form.
makeCanonical ∷ Problem → CanonicalProblem
makeCanonical (objType, obj, constraints) =
  let (_, obj') =
        if objType ≡ Minimize
        then (Maximize, negateCoefficients obj)
        else (objType, obj)

      constraints' = map toCanonical ∘ convertGreaterConstraints ∘
        convertEqualityConstraints $ constraints
  in (obj', constraints')
  where
  convertEqualityConstraints = foldl convertEqualityConstraint []
  convertGreaterConstraints = foldl convertGreaterConstraint []

  -- Converts equality constraints in double inequality constraints
  -- x ≡ b   ⇒   x ≤ b ∧ x ≥ b
  convertEqualityConstraint ∷ [Constraint] → Constraint → [Constraint]
  convertEqualityConstraint cs (as, Equal, b) =
    (as, LesserEqual, b) : (as, GreaterEqual, b) : cs
  convertEqualityConstraint cs c = c : cs

  -- Converts greater inequality constraints in negated lesser inequality
  -- constraints
  -- x ≥ b   ⇒   -x ≤ -b
  convertGreaterConstraint ∷ [Constraint] → Constraint → [Constraint]
  convertGreaterConstraint cs (as, GreaterEqual, b) =
    let as' = negateCoefficients as
    in (as', LesserEqual, -b) : cs
  convertGreaterConstraint cs c = c : cs

  toCanonical ∷ Constraint → CanonicalConstraint
  toCanonical (as, _, b) = (as, b)


-- | Converts a 'CanonicalProblem' to a 'Tableau'.
computeTableau ∷ CanonicalProblem → Tableau
computeTableau (obj, constraints) =
  let toVariableIndex (cs, _) = maximum (map fst cs)
      n = maximum (map toVariableIndex constraints)
      m = length constraints
      slackVariables = map (+n) [1..m]
      generateLine (cs, _) = [ safeGet 0 (negateCoefficients cs) i | i ← [0..n-1] ]
      a = map generateLine constraints
      b = map snd constraints
      c = [ safeGet 0 obj i | i ← [0..n-1] ]
      z = 0

  in makeTableau n m a b c z slackVariables [1..n]

  where

  safeGet ∷ b → [(Int, b)] → Int → b
  safeGet def v i = fromMaybe def (lookup (i+1) v)


negateCoefficients ∷ [Coefficient] → [Coefficient]
negateCoefficients = map (second (⋅(-1)))
