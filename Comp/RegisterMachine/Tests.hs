module Comp.RegisterMachine.Tests (
) where

import Control.Monad.Trans.State (evalState)

import Data.Monoid (Sum(..), getSum)

import Comp.Theory.Class
import Comp.Theory.Tests
import Comp.RegisterMachine.Types
import Comp.RegisterMachine.Interface
import Comp.RegisterMachine.Interpreter

-- TODO: implement formats
showStep :: (RMState, RMState) -> String
showStep (s@(rm, l, rs), _) | l' < length rm = 
  let i = rm !! l' in
  case i of
    HALT      -> show l ++ ": " ++ show HALT
    INC r _   -> show l ++ ": " ++ show i ++ "   with R" ++ show r ++ " = " ++ show (getRegister r `evalState` s)
    DEC r _ _ -> show l ++ ": " ++ show i ++   " with R" ++ show r ++ " = " ++ show (getRegister r `evalState` s)
  where l' = fromIntegral l
showStep _ = "RUNTIME ERROR: Stepping using an instruction out of bounds"

isNonTerminal :: Num a => (RMState, RMState) -> Sum a
isNonTerminal ((rm, l, rs), _) 
  | l' < length rm = Sum $ if rm !! l' == HALT then 0 else 1
  | otherwise = Sum 0
  where l' = fromIntegral l

main :: IO ()
main = testSuite (stepAndLog isNonTerminal) getSum

{-
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
-}