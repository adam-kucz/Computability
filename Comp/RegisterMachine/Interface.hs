{-# LANGUAGE FlexibleContexts #-}

module Comp.RegisterMachine.Interface (
  readMachine,
  prettyShowMachine,
  arg1,
  args,
  regs,
  computeWithRegs,
  compute,
  traceCompWithRegs,
  traceComp
) where

import qualified Data.Map as Map

import Control.Monad.Writer (runWriter)
import Control.Monad.State (evalState, evalStateT)
import Control.Monad.Error.Class (MonadError, throwError)

import Data.Maybe (fromJust, isJust)
import Data.Text (pack, unpack, strip)

import Comp.RegisterMachine.Types
import Comp.RegisterMachine.Interpreter

readMaybe :: Read a => String -> Maybe a
readMaybe str =
  case reads str of
    [(a,"")]  -> return a
    _         -> Nothing

readInstruction :: MonadError String m => String -> m Instruction
readInstruction str = 
  case reads str :: [(Natural,String)] of
    [(_,':':rest)]  -> case readMaybe (unpack . strip . pack $ rest) of
                        Just instr  -> return instr
                        Nothing     -> invalidInstruction
    []              -> case readMaybe str of
                        Just instr  -> return instr
                        Nothing     -> invalidInstruction
    _               -> invalidInstruction
  where invalidInstruction = throwError $ "Invalid instruction: " ++ str

readMachine :: MonadError String m => String -> m RegisterMachine
readMachine = 
  sequence .
  fmap readInstruction .
  filter (\str -> case str of {"" -> False; '#':_ -> False; _ -> True}) .
  lines

prettyShowMachine :: RegisterMachine -> [String]
prettyShowMachine = fmap (\(i,a) -> show i ++ ": " ++ show a) . zip [0..]

arg1 :: Natural -> RegisterState
arg1 = Map.singleton 1

args :: [Natural] -> RegisterState
args = Map.fromList . zip [1..]

regs :: [Natural] -> RegisterState
regs = Map.fromList . zip [0..]
         
computeWithRegs :: RegisterMachine -> RegisterState -> Natural
computeWithRegs = evalState . run
         
compute :: RegisterMachine -> [Natural] -> Natural
compute rm = computeWithRegs rm . args

-- TODO: implement interface for these
customPrintJumpsInComputation :: 
  Natural -> (ComputationState -> Location -> String) -> RegisterMachine -> [Natural] -> IO Natural
customPrintJumpsInComputation t f rm args = do
    sequence_ (fmap putStrLn log)
    putStrLn (show nJumps ++ " instructions in total")
    return n
  where (n, nJumps, log) = customTraceJumpsInComputation t f rm args

-- TODO: implement formats
showState :: Format -> ComputationState -> String
showState _ (l, HALT,           rs) = 
    show l ++ ": " ++ show HALT
showState _ (l, i@(INC r _),    rs) = 
    show l ++ ": " ++ show i ++ "   with R" ++ show r ++ " = " ++ show (getRegister r rs)
showState _ (l, i@(DEC r _ _),  rs) = 
    show l ++ ": " ++ show i ++ " with R" ++ show r ++ " = " ++ show (getRegister r rs)

traceCompWithRegs :: Format -> RegisterMachine -> RegisterState -> (Natural, [String])
traceCompWithRegs format rm rs = runWriter $ evalStateT (runAndLog (showState format) rm) rs

traceComp :: Format -> RegisterMachine -> [Natural] -> (Natural, [String])
traceComp format rm = traceCompWithRegs format rm . args

-- TODO: clean up and implement interface for that
customShowCodeBlockTransitions :: 
  Natural -> (ComputationState -> Location -> String) -> ComputationState -> Maybe String
customShowCodeBlockTransitions t f a@(l, HALT,           rs) = Just $ f a 0
customShowCodeBlockTransitions t f a@(l, i@(INC r j),    rs) = 
  if abs (fromIntegral l - fromIntegral j) < fromIntegral t 
    then Nothing 
    else Just $ f a j
customShowCodeBlockTransitions t f a@(l, i@(DEC r j1 j2),  rs) = 
  if abs (fromIntegral l - fromIntegral j) < fromIntegral t 
    then Nothing
    else Just $ f a j
  where n = getRegister r rs
        j = if n > 0 then j1 else j2

customTraceJumpsInComputation :: 
  Natural -> (ComputationState -> Location -> String) -> RegisterMachine -> [Natural] -> (Natural, Int, [String])
customTraceJumpsInComputation t f rm = 
      getPresentOnly . 
      runWriter . 
      evalStateT (runAndLog (customShowCodeBlockTransitions t f) rm) . 
      args
  where getPresentOnly (n, ls) = (n, length ls, fmap fromJust $ filter isJust ls)