{-# LANGUAGE FlexibleContexts #-}

module Comp.Theory.Class (
  Comp(..)
) where

import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Writer (runWriter)

import Data.Natural (Natural)

class Comp a b | a -> b where
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
  stepC :: b -> Maybe b
  getResultC :: b -> Maybe Natural
  traceC :: a -> [Natural] -> (Natural, [b])
  runC :: a -> [Natural] -> Natural
  
  constC 0 = zeroC 0
  constC n = composeC 1 0 succC [constC (n-1)]
  
  traceC program args = runWriter . go $ initC program args
    where go :: MonadWriter [b] m => b -> m Natural
          go b = do
            tell [b]
            case stepC b of
              Just b' -> go b'
              Nothing -> return . fromJust $ getResultC b
  runC program args = fst $ traceC program args