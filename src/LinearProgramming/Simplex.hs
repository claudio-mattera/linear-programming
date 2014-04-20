{-# LANGUAGE UnicodeSyntax #-}

module LinearProgramming.Simplex (
    Error(..)
  , optimize
  , runSimplex
  ) where

import Prelude.Unicode

import Control.Monad.Trans.Maybe
--import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)
import Control.Monad.Writer
import Control.Monad.Trans.Either

import LinearProgramming.Tableau

data Error = Infeasible
           | Unbounded
           | Unknown
           deriving (Show, Eq)

type Log = [Tableau]


returnError ∷ Monoid c ⇒ a → EitherT a (Writer c) b
returnError = EitherT ∘ return ∘ Left


logTableau ∷ Tableau → EitherT a (Writer Log) ()
logTableau = lift ∘ tell ∘ (:[])


enteringVariable ∷ Tableau → EitherT Error (Writer Log) Variable
enteringVariable tableau =
  case chooseEnteringVariable tableau of
    Nothing → returnError Unknown
    Just e  → return e


leavingVariable ∷ Tableau → Variable → EitherT Error (Writer Log) Variable
leavingVariable tableau entering =
  case chooseLeavingVariable tableau entering of
    Nothing → returnError Unbounded
    Just l  → return l


optimize ∷ Tableau → EitherT Error (Writer Log) Tableau
optimize tableau
  | not (isFeasible tableau) = returnError Infeasible
  | isFinal tableau          = return tableau
  | otherwise                = do
      logTableau tableau
      entering ← enteringVariable tableau
      leaving ← leavingVariable tableau entering
      let tableau' = pivot tableau entering leaving
      optimize tableau'


runSimplex ∷ Tableau → (Either Error Tableau, Log)
runSimplex = runWriter ∘ runEitherT ∘ optimize
