{-# LANGUAGE FlexibleContexts #-}

module Comp.Lambda.Parser (
  prettyShow,
  fromString
) where

import Comp.Lambda.Types
import Comp.Lambda.Util

import Control.Monad.State.Class (MonadState, get, put, modify)
import Control.Monad (MonadPlus, mzero)
import Control.Applicative ((<|>))

import Control.Monad.State (evalStateT)
import Data.Monoid (Endo(..), appEndo)

import Data.Set (Set, union, member)
import qualified Data.Set as Set

lambda :: Char
lambda = '\\'

prettyShow :: LambdaTerm -> String
prettyShow = foldLT id processLambda processApp
  where processLambda x s
          | head s == lambda  = lambda : x ++ " " ++ tail s
          | otherwise         = lambda : x ++ " . " ++ s
        processApp s1 s2 = bracket s1 ++ " " ++ bracket s2
          where bracket s = if ' ' `elem` s then "(" ++ s ++ ")" else s

lambdaChars :: Set Char
lambdaChars = Set.fromList ['\\', 'λ']

data Token = Lparen | Rparen | LambdaToken | Dot | Var String
  deriving (Show, Read, Eq)

varInitChars :: Set Char
varInitChars = Set.fromList $ ['a'..'z'] ++ ['A'..'Z']

varChars :: Set Char
varChars = varInitChars `union` Set.fromList ("_'" ++ ['0'..'9'])

-- TODO: write those using proper parsing / lexing techinques

tokenize :: String -> [Token]
tokenize ""                 = []
tokenize ('(':s)            = Lparen : tokenize s
tokenize (')':s)            = Rparen : tokenize s
tokenize ('.':s)            = Dot : tokenize s
tokenize (' ':s)            = tokenize s
tokenize (a:s)
  | a `member` lambdaChars  = LambdaToken : tokenize s
  | a `member` varInitChars = Var (a:restVar) : tokenize rest
  where (restVar, rest) = span (`member` varChars) s

safeHead :: MonadPlus m => [a] -> m a
safeHead []     = mzero
safeHead (x:xs) = return x
  
parse :: [Token] -> Maybe LambdaTerm
parse = evalStateT greedyParse
  where greedyParse :: (MonadState [Token] m, MonadPlus m) => m LambdaTerm
        greedyParse = unitParse >>= tryApp
          where tryApp :: (MonadState [Token] m, MonadPlus m) => LambdaTerm -> m LambdaTerm
                tryApp m = (unitParse >>= tryApp . App m) <|> return m
        
        unitParse :: (MonadState [Token] m, MonadPlus m) => m LambdaTerm
        unitParse = do
          s <- get
          h <- safeHead s
          modify tail
          case h of
            Rparen  -> mzero
            Dot     -> mzero
            Var x   -> return $ V x
            Lparen  -> do m <- greedyParse
                          s' <- get
                          rparen <- safeHead s'
                          modify tail
                          if rparen == Rparen
                            then return m
                            else mzero
            LambdaToken -> do let (vars, rest) = span (\t -> case t of {Var _ -> True; _ -> False}) (tail s)
                              dot <- safeHead rest
                              if null vars || dot /= Dot
                                then mzero
                                else do put $ tail rest
                                        m <- greedyParse
                                        return . flip appEndo m $ 
                                          foldMap (\t -> case t of {Var v -> Endo $ Lambda v; _ -> undefined}) vars
  
fromString :: String -> Maybe LambdaTerm
fromString = parse . tokenize