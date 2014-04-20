{-# LANGUAGE UnicodeSyntax #-}

module LinearProgramming.Simplex (
    Error(..)
  , Log(..)
  , optimize
  , runSimplex
  ) where

import Prelude.Unicode

import Control.Monad.Trans (lift)
import Control.Monad.Writer
import Control.Monad.Trans.Either

import qualified Data.DList as DL

import LinearProgramming.Tableau

data Error = Infeasible
           | Unbounded
           | Unknown
           deriving (Show, Eq)

data LogEntry = LogString String
              | LogTableau Tableau
              | LogError Error
              deriving (Show, Eq)

type Log = DL.DList LogEntry

returnError ∷ Monoid c ⇒ a → EitherT a (Writer c) b
returnError = EitherT ∘ return ∘ Left

logEntry ∷ LogEntry → EitherT a (Writer Log) ()
logEntry = lift ∘ tell ∘ DL.singleton

logTableau ∷ Tableau → EitherT a (Writer Log) ()
logTableau = logEntry ∘ LogTableau

logString ∷ String → EitherT a (Writer Log) ()
logString = logEntry ∘ LogString

logError ∷ Error → EitherT a (Writer Log) ()
logError = logEntry ∘ LogError


enteringVariable ∷ Tableau → EitherT Error (Writer Log) Variable
enteringVariable tableau =
  case chooseEnteringVariable tableau of
    Nothing → logError Unknown >> returnError Unknown
    Just e  → return e


leavingVariable ∷ Tableau → Variable → EitherT Error (Writer Log) Variable
leavingVariable tableau entering =
  case chooseLeavingVariable tableau entering of
    Nothing → logError Unbounded >> returnError Unbounded
    Just l  → return l


optimize ∷ Tableau → EitherT Error (Writer Log) Tableau
optimize initialTableau = do
    logString "Starting"
    logTableau initialTableau
    helper initialTableau
  where
  helper ∷ Tableau → EitherT Error (Writer Log) Tableau
  helper tableau
    | not (isFeasible tableau) = logError Infeasible >> returnError Infeasible
    | isFinal tableau          = return tableau
    | otherwise                = do
        entering ← enteringVariable tableau
        leaving ← leavingVariable tableau entering
        let nextTableau = pivot tableau entering leaving
        logTableau nextTableau
        helper nextTableau


runSimplex ∷ Tableau → (Either Error Tableau, Log)
runSimplex = runWriter ∘ runEitherT ∘ optimize
