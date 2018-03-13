module Comp.RegisterMachine.Machines (
  copyReg, push, pop, zeroReg, dec, inc,
  universal, universalInstance,
  adder, multiplier, proj, constant,
  copyRegs
) where

import qualified Data.List as List

import qualified Data.Map as Map

import Comp.RegisterMachine.Types
import Comp.RegisterMachine.Trans

errHalt :: Location
errHalt = 10000

copyReg :: Register -> Register -> Register -> RegisterMachine
copyReg src dst aux = normalizeHalts
  [ DEC dst 0 1,
    DEC src 2 4,
    INC aux 3,
    INC dst 1,
    DEC aux 5 errHalt,
    INC src 4]

push :: Register -> Register -> Register -> RegisterMachine
push src dst aux = normalizeHalts
  [ INC aux 1,
    DEC dst 2 3,
    INC aux 0,
    DEC aux 4 5,
    INC dst 3,
    DEC src 1 errHalt]

pop :: Register -> Register -> Register -> RegisterMachine
pop src dst aux = ls
  where ls = [  DEC dst 0 1,
                DEC src 2 exit,
                INC src 3,
                DEC src 4 5,
                INC aux 3,
                DEC aux 7 6,
                INC dst 3,
                DEC aux 8 halt,
                INC src 5,
                HALT,
                HALT]
        halt = fromIntegral $ length ls - 2
        exit = fromIntegral $ length ls - 1

zeroReg :: Register -> RegisterMachine
zeroReg r = normalizeHalts
  [ DEC r 0 errHalt]

dec :: Register -> RegisterMachine
dec r = [DEC r 1 2, HALT, HALT]

inc :: Register -> RegisterMachine
inc r = [INC r 1, HALT]

universal :: [Register] -> RegisterMachine
universal [dst, p, a, pc, n, c, r, s, t, z] = assembleOneHaltMachine machines "push0toA" "halt"
  where machines = Map.fromList
          [ ("push0toA",  (zeroReg a,       oneOutput "setTtoP")),
            ("setTtoP",   (copyReg p t z,   oneOutput "popTtoN")),
            ("popTtoN",   (pop t n z,       twoOutput "decPC"     "popAtoR0")),
            ("decPC",     (dec pc,          twoOutput "popTtoN"   "popNtoC")),
            ("popNtoC",   (pop n c z,       twoOutput "popAtoR"   "popAtoR0")),
            ("popAtoR0",  (pop a dst z,     twoOutput "halt"      "halt")),
            ("popAtoR",   (pop a r z,       twoOutput "decC_1"    "decC_1")),
            ("decC_1",    (dec c,           twoOutput "decC_2"    "incR")),
            ("decC_2",    (dec c,           twoOutput "pushRtoS"  "incN")),
            ("pushRtoS",  (push r s z,      oneOutput "popAtoR")),
            ("incR",      (inc r,           oneOutput "setPCtoN")),
            ("incN",      (inc n,           oneOutput "popNtoPC")),
            ("popNtoPC",  (pop n pc z,      twoOutput "decR"      "decR")),
            ("decR",      (dec r,           twoOutput "pushRtoA"  "setPCtoN")),
            ("setPCtoN",  (copyReg n pc z,  oneOutput "pushRtoA")),
            ("pushRtoA",  (push r a z,      oneOutput "popStoR")),
            ("popStoR",   (pop s r z,       twoOutput "pushRtoA"  "setTtoP"))]

universalInstance :: RegisterMachine
universalInstance = universal [0..9]

adder :: RegisterMachine
adder = normalizeHalts
  [ DEC 1 1 2,
    INC 0 0,
    DEC 2 3 errHalt,
    INC 0 2]

multiplier :: RegisterMachine
multiplier = normalizeHalts
  [ DEC 1 1 errHalt,
    DEC 2 2 4,
    INC 0 3,
    INC 3 1,
    DEC 3 5 0,
    INC 2 4]

proj :: RegisterMachine
proj = normalizeHalts
  [ DEC 1 1 errHalt,
    INC 0 0]
    
constant :: Natural -> RegisterMachine
constant 0 = [HALT]
constant n = assembleOneHaltMachine machines "inc1" "halt"
  where machines = Map.fromList
          [ ("inc1",      ([INC 0 1, HALT], oneOutput "recursive")),
            ("recursive", (constant (n-1),  oneOutput "halt"))]

copyRegs :: [Register] -> [Register] -> Register -> Maybe RegisterMachine
copyRegs [] [] aux                    = [HALT]
copyRegs srcs@(x:xs) dsts@(y:ys) aux  = assembleOneHaltMachine machines "setX" "halt"
  where machines = Map.fromList
          [ ("setX",      (copyReg x y aux, oneOutput "recursive")),
            ("recursive", (unsafeCopyRegs xs ys aux,  oneOutput "halt"))]