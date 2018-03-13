module Comp.Theory.Tests (
  addC, predC, multC,
  unOpTest, binOpTest
) where

import Data.Natural (Natural)

import Comp.Theory.Class (Comp(..))

addC :: Comp a => a
addC = primRecC 1 (projC 1 1) (composeC 3 1 succC [projC 3 3])

predC :: Comp a => a
predC = primRecC 0 (zeroC 0) (projC 2 1)

multC :: Comp a => a
multC = primRecC 1 (zeroC 1) (composeC 3 2 addC [projC 3 3, projC 3 1])

unOpTest :: Comp c => c -> Natural -> Natural
unOpTest c a = computeC c [a]

binOpTest :: Comp c => c -> Natural -> Natural -> Natural
binOpTest c a b = computeC c [a, b]