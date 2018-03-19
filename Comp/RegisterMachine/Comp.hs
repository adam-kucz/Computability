module Comp.RegisterMachine.Comp (
  succF, zeroF, projF,
  composeF, primRecF, minimizeF,
  constant
) where

import Data.List ((\\))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Comp.RegisterMachine.Types
import Comp.RegisterMachine.Machines
import Comp.RegisterMachine.Trans

succF :: RegisterMachine
succF = [INC 0 1, DEC 1 2 3, INC 0 1, HALT]

zeroF :: Natural -> RegisterMachine
zeroF = const [HALT]

projF :: Natural -> Natural -> RegisterMachine
projF n i = copyReg i 0 (n+1)

-- composeC M N takes an M-ary function and M N-ary functions and returns their composition
composeF :: Natural -> Natural -> RegisterMachine -> [RegisterMachine] -> RegisterMachine
composeF m n fM gMs = sequenceMachines $ fmap kthGMachine [1..fromIntegral m] ++ [copyRegs ys [1..m] aux, fM]
  where (xs, rs)    = splitAt (fromIntegral n) $ getUnusedRegs (fM : gMs)
        (ys, rs')   = splitAt (fromIntegral m) rs
        (aux:rs'')  = rs'
        usedRegs = Set.elems . Set.unions . fmap getRegs $ fM : gMs
        kthGMachine k = sequenceMachines [initG, gMs !! (k - 1), copyReg 0 (ys !! (k - 1)) aux, zeroRegs usedRegs]
          where initG = if k == 1 then copyRegs [1..n] xs aux else copyRegs xs [1..n] aux

-- primRecC N takes an N-ary function and an (N+2)-ary function and returns an (N+1)-ary function
primRecF :: Natural -> RegisterMachine -> RegisterMachine -> RegisterMachine
primRecF n fM gM = assembleOneHaltMachine machines "init" "halt"
  where (xs, rs)        = splitAt (fromIntegral n) $ getUnusedRegs [fM, gM]
        (xlast:c:a1:a2:a3:rs')  = rs
        usedRegs = Set.elems . Set.unions . fmap getRegs $ [fM, gM]
        initM = sequenceMachines [copyRegs [1..n] xs a1, copyReg (n+1) xlast a1, zeroReg (n+1), fM]
        gSequence = sequenceMachines [copyRegs (xs ++ [c, 0]) [1..n+2] a1, zeroRegs $ usedRegs \\ [1..n+2], gM, inc c]
        machines = Map.fromList
          [ ("init",    (initM,                         oneOutput "cCheck")),
            ("cCheck",  (areEqual c xlast a1 a2 a3, twoOutput "halt" "doG")),
            ("doG",     (gSequence,                     oneOutput "cCheck"))]

-- minimizeC N takes an (N+1)-ary function and returns N-ary function
minimizeF :: Natural -> RegisterMachine -> RegisterMachine
minimizeF n fM = assembleOneHaltMachine machines "init" "halt"
  where (xs, rs)    = splitAt (fromIntegral n) $ getUnusedRegs [fM]
        (c:aux:rs') = rs
        usedRegs    = Set.elems $ getRegs fM
        setupNextC  = sequenceMachines [inc c, copyRegs (xs ++ [c]) [1..n+1] aux, zeroRegs $ usedRegs \\ [1..n+1]]
        machines = Map.fromList
          [ ("init",        (copyRegs [1..n] xs aux, oneOutput "doF")),
            ("doF",         (fM, oneOutput "check0")),
            ("check0",      (dec 0, twoOutput "setupNextC" "setResult")),
            ("setResult",   (copyReg 0 c aux, oneOutput "halt")),
            ("setupNextC",  (setupNextC, oneOutput "doF"))]