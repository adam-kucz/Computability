module Comp.Lambda.Comp (
  churchNum,
  projF,
  zeroF,
  succF,
  composeF,
  primRecF,
  minimizeF
) where

import Data.Maybe (fromJust)
import qualified Data.Set as Set

import Data.Natural

import Comp.Lambda.Types
import Comp.Lambda.Util
import Comp.Lambda.Parser
import Comp.Lambda.Terms

unsafeRead :: String -> LambdaTerm
unsafeRead = fromJust . fromString

churchNum :: Natural -> LambdaTerm
churchNum n = unsafeRead $ "λ f x . " ++ mconcat (replicate intN "f (") ++ "x" ++ replicate intN ')'
  where intN = fromIntegral n

true :: LambdaTerm
true = unsafeRead "λ x y . x"

false :: LambdaTerm
false = unsafeRead "λ x y . y"

if' :: LambdaTerm
if' = unsafeRead "λ f x y . f x y"

eq0 :: LambdaTerm
eq0 = subs true "t" . subs false "f" $ unsafeRead "λ x . x (λ y . f) t"

pair :: LambdaTerm
pair = unsafeRead "λ x y f . f x y"

fst' :: LambdaTerm
fst' = subs true "t" $ unsafeRead "λ f . f t"

snd' :: LambdaTerm
snd' = subs false "t" $ unsafeRead "λ f . f t"

pred' :: LambdaTerm
pred' = subs snd' "Snd" . subs pair "Pair" . subs g "G" $ unsafeRead "λ y f x . Snd (y (G f) (Pair x x))"
  where g = subs fst' "Fst" $ unsafeRead "λ f p . Pair (f (Fst p)) (Fst p)"

idF :: LambdaTerm
idF = unsafeRead "λ x . x"

yCombinator :: LambdaTerm
yCombinator = unsafeRead "λ f . (λ x . f (x x)) (λ x . f (x x))"

-- functions for partial recursive set
succF :: LambdaTerm
succF = unsafeRead "λ n . λ f x . f (n f x)"

zeroF :: Natural -> LambdaTerm
zeroF n = bindAll xVars z
  where z :: LambdaTerm
        z = churchNum 0
        xVars :: [Var]
        xVars = getNNewVars (fromIntegral n) [z]

projF :: Natural -> Natural -> LambdaTerm
projF n i = bindAll xVars . V $ xVars !! (fromIntegral i - 1)
  where xVars :: [Var]
        xVars = getNNewVarsExcept (fromIntegral n) Set.empty

composeF :: Natural -> Natural -> LambdaTerm -> [LambdaTerm] -> LambdaTerm
composeF m n fLT gLTs | length gLTs == fromIntegral m = bindAll xVars $ gChecks $$ appliedF
  where xVars :: [Var]
        xVars = getNNewVars (fromIntegral n) (fLT:gLTs)
        applyToXs :: LambdaTerm -> LambdaTerm
        applyToXs = applyToAll [V x | x <- xVars]
        appliedGs :: [LambdaTerm]
        appliedGs = [applyToXs g | g <- gLTs]
        gChecks :: LambdaTerm
        gChecks = applyToAll [gVal $$ idF | gVal <- appliedGs] idF
        appliedF :: LambdaTerm
        appliedF = applyToAll appliedGs fLT

primRecF :: Natural -> LambdaTerm -> LambdaTerm -> LambdaTerm
primRecF n f g =  App yCombinator .
                  Lambda h . 
                  bindAll vars .
                  Lambda x $
                  if' $$
                  (eq0 $$ V x) $$
                  applyToXs f $$
                  (applyToXs g $$ xPred $$ (applyToXs (V h) $$ xPred))
  where (h:x:vars) = getNNewVars (2 + fromIntegral n) [f,g]
        applyToXs :: LambdaTerm -> LambdaTerm
        applyToXs = applyToAll [V y | y <- vars]
        xPred :: LambdaTerm
        xPred = App pred' $ V x

minimizeF :: Natural -> LambdaTerm -> LambdaTerm
minimizeF n f = bindAll vars $ applyToXs (yCombinator $$ body) $$ churchNum 0
  where body =  Lambda g . 
                bindAll vars . 
                Lambda x $ 
                if' $$ 
                (eq0 $$ (applyToXs f $$ V x)) $$
                V x $$
                (applyToXs (V g) $$ (succF $$ V x))
        (g:x:vars) = getNNewVars (2 + fromIntegral n) [f]
        applyToXs :: LambdaTerm -> LambdaTerm
        applyToXs = applyToAll [V y | y <- vars]















