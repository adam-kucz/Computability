{-# LANGUAGE FlexibleContexts #-}

module LambdaInterface (
  humanFriendlySubs,
  humanFriendlyBNF
) where 

import Control.Monad.State.Class (MonadState, gets, modify)
import Control.Monad.State (evalState)

import Data.Set (Set, union)
import qualified Data.Set as Set

import Data.Maybe (isJust, fromJust)

import Data.Natural

import Computable.Comp (Comp(..))

import LambdaTypes
import LambdaUtil
import LambdaTerms
import LambdaComputability
import LambdaParser

-- TODO: rewrite as a makeHumanFriendly :: LambdaTerm -> LambdaTerm function

humanFriendlySubs :: LambdaTerm -> Var -> LambdaTerm -> LambdaTerm
humanFriendlySubs m x n = evalState (subsMX n) (Set.singleton x `union` variables m `union` variables n)
  where subsMX :: (MonadState (Set Var) m) => LambdaTerm -> m LambdaTerm
        subsMX (V y)
          | y == x          = return m
          | otherwise       = return $ V y
        subsMX (Lambda y n)
          | y == x          = return $ Lambda y n
          | isAbsent y m
            || isAbsent x n = do
              n' <- subsMX n
              return $ Lambda y n'
          | otherwise       = do
              z <- gets getNewVarExcept
              modify $ Set.insert z
              n' <- subsMX $ subsVar z y n
              return $ Lambda z n'
          where z = getNewVar (App m $ V x)
        subsMX (App n1 n2)  = do
          n1' <- subsMX n1
          n2' <- subsMX n2
          return $ App n1' n2'

-- uses outer-most, left-most reduction order    
hfOneStepBetaReduce :: LambdaTerm -> Maybe LambdaTerm
hfOneStepBetaReduce (App (Lambda x m) n) 
  = Just $ humanFriendlySubs n x m
hfOneStepBetaReduce (Lambda x m) 
  | isJust mm' = Just $ Lambda x m'
  where mm' = hfOneStepBetaReduce m
        m' = fromJust mm'
hfOneStepBetaReduce (App m n) 
  | isJust mm' = Just $ App m' n
  | isJust mn' = Just $ App m n'
  where mm' = hfOneStepBetaReduce m
        mn' = hfOneStepBetaReduce n
        m' = fromJust mm'
        n' = fromJust mn'
hfOneStepBetaReduce l = Nothing
          
humanFriendlyBNF :: LambdaTerm -> LambdaTerm
humanFriendlyBNF m = case hfOneStepBetaReduce m of
                      Just m' -> humanFriendlyBNF m'
                      Nothing -> m

churchAsFunction :: LambdaTerm -> (a -> a) -> a -> a
churchAsFunction (Lambda f' (Lambda x' l)) f x = go l
  where go (V v)          | v == x' = x
        go (App (V v) l') | v == f' = f (go l')
        go _                        = undefined
          
toNatural :: LambdaTerm -> Natural
toNatural n = churchAsFunction (toBNF n) (+1) 0

instance Comp LambdaTerm where
  computeC :: a -> [Natural] -> Natural
  computeC lt ns = toNatural $ applyToAll [churchNum n | n <- ns] lt
  
  succC = succF
  zeroC = zeroF
  projC = projF
  composeC = composeF
  primRecC = primRecF
  minimizeC = minimizeF