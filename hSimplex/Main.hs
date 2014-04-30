{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode

import LinearProgramming.Tableau
import LinearProgramming.Simplex
import LinearProgramming.Parser

import Data.List (sort)
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import System.Environment (getArgs)

import qualified Data.DList as DL
import qualified Data.Vector as V

main ∷ IO ()
main = do
  args ← getArgs
  case args of
    [] → putStrLn "Usage: program tableau_file.txt"
    fileName:_ → do
    text ← readFile fileName

    case parseDirectTableau text of
      Left e → print e
      Right tableau → do
        let actualVariables = sort ∘  V.toList ∘ tabIndependantVariables $ tableau
        print tableau
        let (result, history) = runSimplex tableau
            values = map (fromRational ∘ tabZ ∘ fromLogTableau) ∘
                      filter isLogTableau ∘ DL.toList $ history ∷ [Double]

        case result of
          Left msg → print msg
          Right tf → do

            putStrLn "Optimal tableau:"
            print tf

            let vb = V.toList (tabBasicVariables tf)
                b = V.toList (tabB tf)
                basicVariablesValues = zip vb b
                variables = map (\i → (i,fromRational ∘ lookupDef 0 basicVariablesValues $ i)) actualVariables

            putStrLn "Optimal solution:"
            forM_ variables (\(i, v) → putStrLn ("x" ⧺ show i ⧺ " = " ⧺ show v))


            putStrLn ""
            putStrLn "Objective functions values at each iteration:"
            forM_ values print

  where

  isLogTableau (LogTableau _) = True
  isLogTableau _ = False

  fromLogTableau (LogTableau t) = t
  fromLogTableau _ = error "Not a tableau"

  lookupDef def xs e = fromMaybe def (lookup e xs)
