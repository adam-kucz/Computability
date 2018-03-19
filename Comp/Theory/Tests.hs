{-# LANGUAGE ScopedTypeVariables #-}

module Comp.Theory.Tests (
  addC, predC, multC,
  testExpression, testManyExprs,
  testSuite
) where

import Data.Natural (Natural)

import Comp.Theory.Class

addC :: Comp a b => a
addC = primRecC 1 (projC 1 1) (composeC 1 3 succC [projC 3 3])

predC :: Comp a b => a
predC = primRecC 0 (zeroC 0) (projC 2 1)

multC :: Comp a b => a
multC = primRecC 1 (zeroC 1) (composeC 2 3 addC [projC 3 3, projC 3 1])

{-
unOpTest :: Comp a b => a -> Natural -> Natural
unOpTest c a = runC c [a]

binOpTest :: Comp a b => a -> Natural -> Natural -> Natural
binOpTest c a b = runC c [a, b]

-- takes three arguments and returns the third one incremented
expr1 :: Comp a b => a
expr1 = composeC 1 3 succC [projC 3 3]

-- takes an argument and returns 0
expr2 :: Comp a b => a
expr2 = primRecC 0 (zeroC 0) (projC 2 2)
-}

testExpression :: (Comp a b, Monoid c, Show s) => 
                    (b -> (Maybe b, c)) -> (c -> s) -> String -> a -> [Natural] -> IO ()
testExpression stepAndLog processTrace name comp args = do
  let (r, t) = traceCWith stepAndLog comp args
  putStrLn $ "Testing " ++ name
  putStrLn $ "Result: " ++ show r
  putStrLn $ "Trace: " ++ show (processTrace t)
  putStrLn ""

testManyExprs :: (Comp a b, Monoid c, Show s1, Show s2) => 
                  (b -> (Maybe b, c)) -> (c -> s1) -> String -> (s2 -> (a, [Natural])) -> [s2] -> IO ()
testManyExprs stepAndLog processTrace name extractor ns = do
  putStrLn $ "Testing " ++ name
  let rs = testOne . extractor <$> ns
  putStrLn $ "Results: " ++ show (zip ns $ fst <$> rs)
  putStrLn $ "Trace lengths: " ++ show (zip ns $ snd <$> rs)
  putStrLn ""
  where testOne (comp,args) = (\(r, t) -> (r, processTrace t)) $ traceCWith stepAndLog comp args
  
testSuite :: forall a b c s1 . (Comp a b, Monoid c, Show s1) => (b -> (Maybe b, c)) -> (c -> s1) -> IO ()
testSuite stepAndLog processTrace = do
  doTests "constant" (\n -> (constC n, [])) [0..10]
  doTests "successor" (\n -> (succC, [n])) [0..10]
  doTests "zero with n arguments" (\n -> (zeroC n, replicate (fromIntegral n) 0)) [0..10]
  doTests "projection" (\(i,j) -> (projC i j, (+10) <$> [1..i])) $ do { i <- [1..10]; j <- [1..i]; return (i,j) }
  doTests "composition of n successor calls" (\n -> (composeSuccNTimes n, [0])) [0..7]
  doTests "predecessor" (\n -> (predC, [n])) [0..10]
  doTests "addition" (\(a,b) -> (addC, [a,b])) $ do { i <- [0..3]; j <- [0..3]; return (i,j) }
  doTests "multiplication" (\(a,b) -> (multC, [a,b])) $ do { i <- [0..2]; j <- [0..2]; return (i,j) }
  where composeSuccNTimes 0 = projC 1 1
        composeSuccNTimes n = composeC 1 1 succC . (:[]) . composeSuccNTimes $ n-1
        doTests :: (Show s2) => String -> (s2 -> (a, [Natural])) -> [s2] -> IO ()
        doTests = testManyExprs stepAndLog processTrace