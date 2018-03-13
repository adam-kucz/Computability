module Comp.RegisterMachine.Comp (
) where

import Comp.RegisterMachine.Types
import Comp.RegisterMachine.Machines
import Comp.RegisterMachine.Trans

projF :: Natural -> Natural -> RegisterMachine
projF n i = copyReg i 0

zeroF :: Natural -> RegisterMachine
zeroF = const [HALT]

succF :: RegisterMachine
succF = [INC 0 1, DEC 1 2 3, INC 0 1, HALT]

composeF :: Natural -> Natural -> RegisterMachine -> [RegisterMachine] -> RegisterMachine
composeF m n fM gMs = 
  case gMs of
    []        -> assembleOneHaltMachine machines "setYs" "halt"
    (gM:gMs') -> 
  where (xs, rs)    = splitAt (fromIntegral m) $ getUnusedRegs (fM : gMs)
        (ys, rs')   = splitAt (fromIntegral n) rs
        (aux:rs'')  = rs'
        machines = Map.fromList
          [ ("setYs", (copyRegs ys [1..n] aux,  oneOutput "F")),
            ("F",     (fM,                      oneOutput "halt"))]