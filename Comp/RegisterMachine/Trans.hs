module Comp.RegisterMachine.Trans (
  getRegs, getUnusedRegs,
  sequenceMachines,
  assembleOneHaltMachine, assembleTwoHaltMachine,
  simplifyHalts, normalizeHalts,
  oneOutput, twoOutput
) where

import Data.Foldable (fold)

import Data.List ((\\))
import qualified Data.List as List

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Maybe (fromJust, isJust)

import Comp.RegisterMachine.Types

mapInstrLoc :: (Location -> Location) -> Instruction -> Instruction
mapInstrLoc f HALT          = HALT
mapInstrLoc f (INC r j)     = INC r (f j)
mapInstrLoc f (DEC r j1 j2) = DEC r (f j1) (f j2)

mapLoc :: (Location -> Location) -> RegisterMachine -> RegisterMachine
mapLoc = fmap . mapInstrLoc

mapInstrReg :: (Register -> Register) -> Instruction -> Instruction
mapInstrReg f HALT          = HALT
mapInstrReg f (INC r j)     = INC (f r) j
mapInstrReg f (DEC r j1 j2) = DEC (f r) j1 j2

mapReg :: (Register -> Register) -> RegisterMachine -> RegisterMachine
mapReg = fmap . mapInstrReg

getRegs :: RegisterMachine -> Set Register
getRegs = Set.fromList . fmap fromJust . filter isJust . fmap getReg
  where getReg HALT         = Nothing
        getReg (INC r _)    = Just r
        getReg (DEC r _ _)  = Just r

getUnusedRegs :: [RegisterMachine] -> [Register]
getUnusedRegs rms = filter (`Set.notMember` used) [0..]
  where used = Set.unions (fmap getRegs rms)

type MachineAssembler k = Map k Location -> Natural -> RegisterMachine -> RegisterMachine

sequenceMachines :: [RegisterMachine] -> RegisterMachine
sequenceMachines [] = [HALT]
sequenceMachines (m:ms) = assembleOneHaltMachine machines "init" "halt"
  where machines = Map.fromList
          [ ("init",      (m,                   oneOutput "recursive")),
            ("recursive", (sequenceMachines ms, oneOutput "halt"))]

assembleOneHaltMachine :: Ord k => Map k (RegisterMachine, MachineAssembler k) -> k -> k -> RegisterMachine
assembleOneHaltMachine machines startMachine haltName = simplifyHalts $ fold assembled ++ [HALT]
  where names = startMachine : List.delete startMachine (Map.keys machines)
        pos = reverse . foldr (\name ls@(x:_) -> x + fromIntegral (length . fst $ machines!name) : ls) [0] $ reverse names 
        posMap = Map.fromList $ zip (names ++ [haltName]) pos
        assembled = fmap (\name -> (snd $ machines!name) posMap (posMap!name) (fst $ machines!name)) names

assembleTwoHaltMachine :: Ord k => Map k (RegisterMachine, MachineAssembler k) -> k -> k -> k -> RegisterMachine
assembleTwoHaltMachine machines startMachine haltName exitName = fold assembled ++ [HALT, HALT]
  where names = startMachine : List.delete startMachine (Map.keys machines)
        pos' = reverse $ foldr (\name ls@(x:_) -> x + fromIntegral (length . fst $ machines!name) : ls) [0] names
        pos = pos' ++ [last pos' + 1]
        posMap = Map.fromList $ zip (names ++ [haltName, exitName]) pos
        assembled = fmap (\name -> (snd $ machines!name) posMap (posMap!name) (fst $ machines!name)) names
        
shiftCode :: Natural -> RegisterMachine -> RegisterMachine
shiftCode n = mapLoc (+n)

simplifyHalts :: RegisterMachine -> RegisterMachine
simplifyHalts rm = ((fmap shiftIndices $ zip [0..] rm) \\ replicate nHalts HALT) ++ [HALT]
  where is = fmap fromIntegral $ List.elemIndices HALT rm
        nHalts = length is
        halt = fromIntegral $ length rm - nHalts
        shiftIndices (i, instr) = 
            mapInstrLoc (\loc -> if loc `elem` is then halt else adjust loc) instr
          where adjust i = i - (fromIntegral . length $ List.takeWhile (<i) is)

normalizeHalts :: RegisterMachine -> RegisterMachine
normalizeHalts rm = mapLoc (min halt) rm ++ [HALT]
  where halt = fromIntegral $ length rm

plugOneAndShift :: Location -> Natural -> RegisterMachine -> RegisterMachine
plugOneAndShift l shift m 
  | last m == HALT  = mapLoc (\loc -> if loc == halt then l else loc + shift) m
  | otherwise       = undefined
  where halt = fromIntegral $ length m - 1

plugTwoAndShift :: Location -> Location -> Natural -> RegisterMachine -> RegisterMachine
plugTwoAndShift l1 l2 shift m 
  | last m == HALT  = mapLoc (\loc -> if loc == halt then l1 else if loc == exit then l2 else loc + shift) m
  | otherwise       = undefined
  where halt = fromIntegral $ length m - 2
        exit = fromIntegral $ length m - 1

oneOutput :: Ord k => k -> MachineAssembler k
oneOutput halt m = plugOneAndShift (m!halt)

twoOutput :: Ord k => k -> k -> MachineAssembler k
twoOutput halt exit m = plugTwoAndShift (m!halt) (m!exit)