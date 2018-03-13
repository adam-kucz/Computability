-- most of this should be in Comp.Util
module Comp.RegisterMachine.Packer  (
{-    
    pairToPosInt,
    posIntToPair,
    pairToNat,
    natToPair,
    listToNat,
    natToList,
    natToInstr,
-}
    progToNat,
    natToProg
) where

import RMTypes

pairToPosInt :: (Natural, Natural) -> Natural
pairToPosInt (n1, n2) = 2 ^ n1 * (2*n2 + 1)

posIntToPair :: Natural -> (Natural, Natural)
posIntToPair n = case n `divMod` 2 of
  (n', 0) -> let (x, y) = posIntToPair n' in (x+1,y)
  (n', 1) -> (0, n')

pairToNat :: (Natural, Natural) -> Natural
pairToNat = (subtract 1) . pairToPosInt

natToPair :: Natural -> (Natural, Natural)
natToPair = posIntToPair . (+1)

listToNat :: [Natural] -> Natural
listToNat [] = 0
listToNat (n:ns) = pairToPosInt (n, listToNat ns)

natToList :: Natural -> [Natural]
natToList 0 = []
natToList n = let (x, n') = posIntToPair n in x : natToList n'

instrToNat :: Instruction -> Natural
instrToNat HALT = 0
instrToNat (INC r j) = pairToPosInt (2*r, j)
instrToNat (DEC r j1 j2) = pairToPosInt (2*r+1, pairToNat (j1, j2))

natToInstr :: Natural -> Instruction
natToInstr 0 = HALT
natToInstr n = case flag of
    0 -> INC x' y
    1 -> let (j1, j2) = natToPair y in DEC x' j1 j2
  where (x,y) = posIntToPair n
        (x',flag) = x `divMod` 2

progToNat :: [Instruction] -> Natural
progToNat = listToNat . fmap instrToNat

natToProg :: Natural -> [Instruction]
natToProg = fmap natToInstr . natToList