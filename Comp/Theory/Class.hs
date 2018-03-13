module Comp.Theory.Class (
  Comp(..)
) where

import Data.Natural (Natural)

class Comp a where
  {-# MINIMAL computeC, succC, zeroC, projC, composeC, primRecC, minimizeC #-}
  computeC :: a -> [Natural] -> Natural
  succC :: a
  zeroC :: Natural -> a
  projC :: Natural -> Natural -> a
  composeC :: Natural -> Natural -> a -> [a] -> a
  primRecC :: Natural -> a -> a -> a
  minimizeC :: Natural -> a -> a
  constC :: Natural -> a
  
  constC 0 = zeroC 0
  constC n = composeC 1 0 succC [constC (n-1)]