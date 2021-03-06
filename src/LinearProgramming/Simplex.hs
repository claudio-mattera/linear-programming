{-# LANGUAGE UnicodeSyntax #-}

module LinearProgramming.Simplex (
    Error(..)
  , Log
  , LogEntry(..)
  , runSimplex
  ) where

import Prelude.Unicode

import Control.Monad.Writer
import Control.Monad.Trans.Except

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

returnError ∷ Monoid c ⇒ a → ExceptT a (Writer c) b
returnError = ExceptT ∘ return ∘ Left

logEntry ∷ LogEntry → ExceptT a (Writer Log) ()
logEntry = lift ∘ tell ∘ DL.singleton

logTableau ∷ Tableau → ExceptT a (Writer Log) ()
logTableau = logEntry ∘ LogTableau

logString ∷ String → ExceptT a (Writer Log) ()
logString = logEntry ∘ LogString

logError ∷ Error → ExceptT a (Writer Log) ()
logError = logEntry ∘ LogError


enteringVariable ∷ Tableau → ExceptT Error (Writer Log) Variable
enteringVariable tableau =
  case chooseEnteringVariable tableau of
    Nothing → logError Unknown >> returnError Unknown
    Just e  → return e


leavingVariable ∷ Tableau → Variable → ExceptT Error (Writer Log) Variable
leavingVariable tableau entering =
  case chooseLeavingVariable tableau entering of
    Nothing → logError Unbounded >> returnError Unbounded
    Just l  → return l


originalTableau ∷ Tableau → ExceptT Error (Writer Log) Tableau
originalTableau tableau =
  case toOriginalTableau tableau of
    Nothing → logError Infeasible >> returnError Infeasible
    Just t  → return t


simplex ∷ Tableau → ExceptT Error (Writer Log) Tableau
simplex initialTableau = do
    logString "Starting"
    logTableau initialTableau
    helper initialTableau
  where
  helper ∷ Tableau → ExceptT Error (Writer Log) Tableau
  helper tableau
    | not (isFeasible tableau) = logError Infeasible >> returnError Infeasible
    | isFinal tableau          = return tableau
    | otherwise                = do
        entering ← enteringVariable tableau
        logString ("Entering variable is " ⧺ show entering)
        leaving ← leavingVariable tableau entering
        logString ("Leaving variable is " ⧺ show leaving)
        let nextTableau = pivot tableau entering leaving
        logTableau nextTableau
        helper nextTableau

twoPhasesSimplex ∷ Tableau → ExceptT Error (Writer Log) Tableau
twoPhasesSimplex initialTableau
  | isFeasible initialTableau = do
      logString "Initial tableau is already feasible, skipping to phase two"
      simplex initialTableau
  | otherwise = do
      logString "Generating auxiliary tableau"
      let auxiliaryTableau = generateAuxiliaryTableau initialTableau
      finalAuxiliaryTableau ← phaseOneSimplex auxiliaryTableau
      logString "Obtaining a feasible tableau for the original problem"
      feasibleTableau ← originalTableau finalAuxiliaryTableau
      logString "Phase two simplex"
      simplex feasibleTableau


phaseOneSimplex ∷ Tableau → ExceptT Error (Writer Log) Tableau
phaseOneSimplex tableau = do
      logString "Forcing x0 to enter the basis"
      let entering = 0
          leaving = getMinimalNegativeCoefficientVariable tableau
      let initialTableau = pivot tableau entering leaving
      logString "Phase one simplex"
      simplex initialTableau



runSimplex ∷ Tableau → (Either Error Tableau, Log)
runSimplex = runWriter ∘ runExceptT ∘ twoPhasesSimplex
