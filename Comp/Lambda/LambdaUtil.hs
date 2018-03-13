module LambdaUtil (
  foldLT,
  subsVar,
  isAbsent,
  variables,
  getNewVarExcept,
  getNewVar,
  getNNewVars,
  applyToAll
) where 

import Data.Set (Set, union)
import qualified Data.Set as Set

import LambdaTypes

import Debug.Trace (trace)

foldLT :: (Var -> a) -> (Var -> a -> a) -> (a -> a -> a) -> LambdaTerm -> a
foldLT v l a = vlaFoldLT
  where vlaFoldLT (V x)       = v x
        vlaFoldLT (Lambda x m)  = l x (vlaFoldLT m)
        vlaFoldLT (App m n)     = a (vlaFoldLT m) (vlaFoldLT n)

subsVar :: Var -> Var -> LambdaTerm -> LambdaTerm
subsVar x' x = foldLT (V . replace) (Lambda . replace) App
  where replace y = if y == x then x' else y

variables :: LambdaTerm -> Set Var
variables = foldLT Set.singleton (\v mVars -> mVars `union` Set.singleton v) union

allVars :: String -> [String]
allVars alphabet = [ c : s | s <- "" : allVars alphabet, c <- alphabet]

auxVars :: [String]
auxVars = allVars ['a'..'z']

isAbsent :: Var -> LambdaTerm -> Bool
isAbsent x = notElem x . variables

getNNewVarsExcept :: Int -> Set Var -> [Var]
getNNewVarsExcept n ls = take n . filter (`notElem` ls) $ auxVars

getNewVarExcept :: Set Var -> Var
getNewVarExcept = head . getNNewVarsExcept 1

getNNewVars :: Int -> [LambdaTerm] -> [Var]
getNNewVars m = getNNewVarsExcept m . foldr (union . variables) Set.empty

getNewVar :: [LambdaTerm] -> Var
getNewVar = head . getNNewVars 1

applyToAll :: [LambdaTerm] -> LambdaTerm -> LambdaTerm
applyToAll xs lt = foldl App lt xs