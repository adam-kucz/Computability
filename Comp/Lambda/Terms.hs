{-# LANGUAGE FlexibleContexts #-}

module Comp.Lambda.Terms (
  freeVariables,
  boundVariables,
  alphaEquiv,
  subs,
  toBNF,
  oneStepBetaReduce,
  betaEquiv
) where 

import Control.Applicative ((<|>))

import Control.Monad (MonadPlus, mzero)
import Control.Monad.State.Class (MonadState, get, put, modify)
import Control.Monad.State (evalState)
import Control.Monad.Trans.Maybe (runMaybeT)

import Data.Set (Set, (\\), union)
import qualified Data.Set as Set

import Data.Maybe (fromJust)

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

-- TODO: refactor functions to use monads all the way down
subs :: LambdaTerm -> Var -> LambdaTerm -> LambdaTerm
subs m x = subsMX
  where subsMX (V y)
          | y == x          = m
          | otherwise       = V y
        subsMX (Lambda y n) = Lambda z (subsMX $ subsVar z y n)
          where z = getNewVarExcept (Set.singleton x `union` variables m `union` variables n)
        subsMX (App n1 n2)  = App (subsMX n1) (subsMX n2)
        
-- uses outer-most, left-most reduction order    
oneStepBetaReduce :: (MonadState LambdaTerm m, MonadPlus m) => m ()
oneStepBetaReduce = do
  lt <- get
  case lt of 
    App (Lambda x m) n  ->  put $ subs n x m
    Lambda x m          ->  put m >> oneStepBetaReduce >> get >>= \m' -> put $ Lambda x m'
    App m n             -> (put m >> oneStepBetaReduce >> get >>= \m' -> put $ App m' n) <|> 
                           (put n >> oneStepBetaReduce >> get >>= \n' -> put $ App m n')
    _                   -> mzero
-- TODO: think about the following rule:
-- given N `alphaEquiv` M and M' = oneStepBetaReduce M and M' `alphaEquiv` N'
-- we have N' = oneStepBetaReduce M'

toBNF :: (MonadState LambdaTerm m) => m ()
toBNF = fmap fromJust . runMaybeT $ (oneStepBetaReduce >> toBNF) <|> (modify id)
-- TODO: implement checks for non-existence of bnf
-- scratch the above, that's the Halting Problem ;p

-- determines equivalency only when both terms have bnf
betaEquiv :: LambdaTerm -> LambdaTerm -> Bool
betaEquiv m n = getBNF m `alphaEquiv` getBNF n
  where getBNF :: LambdaTerm -> LambdaTerm
        getBNF = evalState (toBNF >> get)
-- TODO: implement betaEquivalent properly
-- scratch the above, betaEquivalence in uncomputable in general
