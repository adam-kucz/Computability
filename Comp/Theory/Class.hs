{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}

module Comp.Theory.Class (
  Comp(..),
  LogStep,
  Natural
) where

import Control.Applicative ((<|>))

import Control.Monad (MonadPlus)
import Control.Monad.State.Class (MonadState, get)
import Control.Monad.Writer.Class (MonadWriter, tell)

import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Writer (Writer, runWriter)

import Data.Maybe (fromJust)
import Data.Natural (Natural)

type LogStep c b = StateT b (MaybeT (Writer c)) ()

class Comp a b | a -> b, b -> a where
  {-# MINIMAL succC, zeroC, projC, composeC, primRecC, minimizeC, initC, stepC, getResultC #-}
  
  succC :: a
  -- zeroC N takes N arguments and returns 0
  zeroC :: Natural -> a
  -- projC N M returns Mth out of N arguments
  projC :: Natural -> Natural -> a
  -- composeC M N takes an M-ary function and M N-ary functions and returns their composition
  composeC :: Natural -> Natural -> a -> [a] -> a
  -- primRecC N takes an N-ary function and an (N+2)-ary function and returns an (N+1)-ary function
  primRecC :: Natural -> a -> a -> a
  -- minimizeC N takes an (N+1)-ary function and returns N-ary function
  minimizeC :: Natural -> a -> a
  constC :: Natural -> a
  
  initC :: a -> [Natural] -> b
  stepC :: (MonadState b m, MonadPlus m) => m ()
  getResultC ::(MonadState b m, MonadPlus m) => m Natural
  -- traceCWith :: (MonadState b m, MonadPlus m, MonadWriter c m) => m () -> a -> [Natural] -> (Natural, c)
  traceCWith :: Monoid c => LogStep c b -> a -> [Natural] -> (Natural, c)
  traceC :: a -> [Natural] -> (Natural, [b])
  runC :: a -> [Natural] -> Natural
  
  constC 0 = zeroC 0
  constC n = composeC 1 0 succC [constC (n-1)]
  
  traceCWith logStep program args = runWriter . fmap fromJust . runMaybeT $ evalStateT run initial
    where initial = initC program args
          run = (logStep >> run) <|> getResultC
  traceC = traceCWith $ get >>= \b -> tell [b] >> stepC
  runC program = (fst :: (Natural, ()) -> Natural) . traceCWith stepC program