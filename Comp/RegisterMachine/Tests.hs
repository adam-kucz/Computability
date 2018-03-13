module Comp.RegisterMachine.Tests where

import Data.Map ((!))
import qualified Data.Map as Map

import Text.Printf
import System.CPUTime

import Comp.RegisterMachine.Types
import Comp.RegisterMachine.Interpreter
import Comp.RegisterMachine.Packer
import Comp.RegisterMachine.Machines
import Comp.RegisterMachine.Trans
import Comp.RegisterMachine.Interface

simple1 :: RegisterMachine
simple1 = copyReg 1 2 3

simple2 :: RegisterMachine
simple2 = assembleOneHaltMachine machines "copy" "halt"
  where machines = Map.fromList [("copy", (simple1,  oneOutput "add")), 
                                 ("add",  (adder,    oneOutput "halt"))]

testUniversal :: RegisterMachine -> [Natural] -> Maybe (Natural, Natural)
testUniversal m args = if correct == univ then Nothing else Just (correct, univ)
  where correct = compute m args
        mCode = progToNat m
        argsCode = listToNat args
        univ = compute universalInstance [mCode, argsCode]

showUniversalState :: ComputationState -> Location -> String
showUniversalState (l, i, rs) j = newBlock ++ "\t("++show l++"->"++show j++"),\tstate " ++ registerState
  where newBlock = case j of
                    0   -> "push0toA"
                    1   -> "decC_1"
                    2   -> "decC_2"
                    3   -> "decPC"
                    4   -> "decR"
                    5   -> "incN"
                    6   -> "incR"
                    7   -> "popAtoR"
                    16  -> "popAtoR0"
                    25  -> "popNtoC"
                    34  -> "popNtoPC"
                    43  -> "popStoR"
                    52  -> "popTtoN"
                    61  -> "pushRtoA"
                    67  -> "pushRtoS"
                    73  -> "setPCtoN"
                    79  -> "setTtoP"
                    85  -> "halt"
                    _   -> "**UNKNOWN**"
        registerState = "PC = " ++ show (getRegister 3 rs) ++ 
                        ", instruction: " ++ show (natToInstr $ getRegister 4 rs) ++
                        ", simulated RS: " ++ show (natToList $ getRegister 2 rs) 
 
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v