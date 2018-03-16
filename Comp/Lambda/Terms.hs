{-# LANGUAGE FlexibleContexts #-}

module Comp.Lambda.Terms (
  freeVariables,
  boundVariables,
  alphaEquiv,
  subs,
  toBNF,
  betaEquiv
) where 

import Data.Set (Set, (\\), union)
import qualified Data.Set as Set

import Data.Maybe (isJust, fromJust)

import Comp.Lambda.Types
import Comp.Lambda.Util

freeVariables :: LambdaTerm -> Set Var
freeVariables = foldLT Set.singleton (\v mVars -> mVars \\ Set.singleton v) union

boundVariables :: LambdaTerm -> Set Var
boundVariables = foldLT (const Set.empty) (\v mVars -> mVars `union` Set.singleton v) union

alphaEquiv :: LambdaTerm -> LambdaTerm -> Bool
alphaEquiv (V x) (V x')
  | x == x' = True
alphaEquiv (Lambda x m) (Lambda y n)
  | subsVar z x m `alphaEquiv` subsVar z y n = True
  where z = getNewVar [m,n]
alphaEquiv (App m n) (App m' n')
  | m `alphaEquiv` m' && n `alphaEquiv` n' = True
alphaEquiv _ _ = False

subs :: LambdaTerm -> Var -> LambdaTerm -> LambdaTerm
subs m x = subsMX
  where subsMX (V y)
          | y == x          = m
          | otherwise       = V y
        subsMX (Lambda y n) = Lambda z (subsMX $ subsVar z y n)
          where z = getNewVarExcept (Set.singleton x `union` variables m `union` variables n)
        subsMX (App n1 n2)  = App (subsMX n1) (subsMX n2)
        
-- uses outer-most, left-most reduction order    
oneStepBetaReduce :: LambdaTerm -> Maybe LambdaTerm
oneStepBetaReduce (App (Lambda x m) n) 
  = Just $ subs n x m
oneStepBetaReduce (Lambda x m) 
  | isJust mm' = Just $ Lambda x m'
  where mm' = oneStepBetaReduce m
        m' = fromJust mm'
oneStepBetaReduce (App m n) 
  | isJust mm' = Just $ App m' n
  | isJust mn' = Just $ App m n'
  where mm' = oneStepBetaReduce m
        mn' = oneStepBetaReduce n
        m' = fromJust mm'
        n' = fromJust mn'
oneStepBetaReduce _ = Nothing
-- TODO: think about the following rule:
-- given N `alphaEquiv` M and M' = oneStepBetaReduce M and M' `alphaEquiv` N'
-- we have N' = oneStepBetaReduce M'

toBNF :: LambdaTerm -> LambdaTerm
toBNF m = case oneStepBetaReduce m of
            Just m' -> toBNF m'
            Nothing -> m
-- TODO: implement checks for non-existence of bnf
-- scratch the above, that's the Halting Problem ;p


-- determines equivalency only when both terms have bnf
betaEquiv :: LambdaTerm -> LambdaTerm -> Bool
betaEquiv m n = toBNF m `alphaEquiv` toBNF n
-- TODO: implement betaEquivalent properly
-- scratch the above, betaEquivalence in uncomputable in general
