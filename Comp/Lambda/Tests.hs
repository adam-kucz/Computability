module Comp.Lambda.Tests (
) where

import Data.Monoid (Sum(..), getSum)

import Comp.Theory.Class
import Comp.Theory.Tests
import Comp.Lambda.Types
import Comp.Lambda.Parser
import Comp.Lambda.Interface

showReduction :: ([ReduKind], LambdaTerm, LambdaTerm) -> String
showReduction (ls, lt, lt')
  | lt == lt' = "No reduction for " ++ prettyShow lt ++ ", search path: " ++ show ls ++ "\n"
  | otherwise = "Reduction path: " ++ show ls ++", reducing [" ++ prettyShow lt ++ "] to [" ++ prettyShow lt' ++ "]\n"

isNonTerminal :: Num a => ([ReduKind], LambdaTerm, LambdaTerm) -> Sum a
isNonTerminal (_, lt, lt')
  | lt == lt' = Sum 0
  | otherwise = Sum 1

main :: IO ()
main = testSuite (betaReduceAndLog isNonTerminal) getSum