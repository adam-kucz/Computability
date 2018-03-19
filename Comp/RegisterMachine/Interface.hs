{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Comp.RegisterMachine.Interface (
  readMachine,
  prettyShowMachine,
  args,
  stepAndLog,
) where

import Control.Monad (MonadPlus, mzero)
import Control.Monad.State.Class (MonadState, get)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Error.Class (MonadError, throwError)

import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.Text (pack, unpack, strip)

import Comp.Theory.Class
import Comp.RegisterMachine.Types
import Comp.RegisterMachine.Comp
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

args :: [Natural] -> RegisterState
args = Map.fromList . zip [1..]

stepAndLog :: (MonadState RMState m, MonadPlus m, MonadWriter c m, Monoid c) => ((RMState, RMState) -> c) -> m ()
stepAndLog f = do
  before <- get
  step
  after <- get
  tell $ f (before, after)
    
instance Comp RegisterMachine RMState where
  succC = succF
  zeroC = zeroF
  projC = projF
  composeC = composeF
  primRecC = primRecF
  minimizeC = minimizeF
  constC = constant
  
  initC rm ns = (rm, 0, args ns)
  stepC = step
  getResultC = getRegister 0