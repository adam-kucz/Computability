{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}

module Comp.Theory.Class (
  Comp(..),
  Natural
) where

import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Writer (runWriter)

import Data.Maybe (fromJust, catMaybes)
import Data.Natural (Natural)

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
  stepC :: b -> Maybe b
  getResultC :: b -> Maybe Natural
  traceCWith :: Monoid c => (b -> (Maybe b, c)) -> a -> [Natural] -> (Natural, c)
  traceC :: a -> [Natural] -> (Natural, [b])
  runC :: a -> [Natural] -> Natural
  
  constC 0 = zeroC 0
  constC n = composeC 1 0 succC [constC (n-1)]
  
  traceCWith logStep program args = runWriter . go $ initC program args
    where -- go :: MonadWriter [c] m => b -> m Natural
          go b = do
            let (mb', c) = logStep b
            tell c
            case mb' of
              Just b' -> go b'
              Nothing -> return . fromJust $ getResultC b
  traceC program args = 
    let (a, log) = traceCWith (\b -> let mb' = stepC b in (mb', [mb'])) program args
    in  (a, catMaybes log)
  runC program args = fst $ traceC program args