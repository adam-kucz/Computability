module Comp.Theory.Tests (
  addC, predC, multC,
  unOpTest, binOpTest,
  expr1, expr2
) where

import Data.Natural (Natural)

import Comp.Theory.Class (Comp(..))

addC :: Comp a => a
addC = primRecC 1 (projC 1 1) (composeC 1 3 succC [projC 3 3])

predC :: Comp a => a
predC = primRecC 0 (zeroC 0) (projC 2 1)

multC :: Comp a => a
multC = primRecC 1 (zeroC 1) (composeC 2 3 addC [projC 3 3, projC 3 1])

unOpTest :: Comp c => c -> Natural -> Natural
unOpTest c a = computeC c [a]

binOpTest :: Comp c => c -> Natural -> Natural -> Natural
binOpTest c a b = computeC c [a, b]

-- takes three arguments and returns the third one incremented
expr1 :: Comp c => c
expr1 = composeC 1 3 succC [projC 3 3]

-- takes an argument and returns 0
expr2 :: Comp c => c
expr2 = primRecC 0 (zeroC 0) (projC 2 2)