{-# LANGUAGE FlexibleContexts #-}

module Comp.RegisterMachine.Interpreter (
    doInstruction,
    getRegister,
    runAndLog,
    run
) where 

import Data.Natural (Natural)

import Control.Monad.State.Class (MonadState, modify, get, gets)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriterT)

import Data.Map (adjust, alter, findWithDefault)

import Comp.RegisterMachine.Types

getRegister :: Register -> RegisterState -> Natural
getRegister = findWithDefault 0

doInstruction :: (MonadState RegisterState m) => Instruction -> m (Maybe Location)
doInstruction HALT          = return Nothing
doInstruction (INC r j)     = modify (alter (Just . maybe 1 (+1)) r) >> return (Just j)
doInstruction (DEC r j1 j2) = 
  do  a <- gets (getRegister r)
      case a of
        0 -> return (Just j2)
        n -> modify (adjust (subtract 1) r) >> return (Just j1)

stepAndLog :: (MonadState RegisterState m, MonadReader RegisterMachine m, MonadWriter [a] m) => 
                (ComputationState -> a) -> Location -> m (Maybe Location)
stepAndLog logMaker i = do
  instructions <- ask
  if fromIntegral i >= length instructions
    then return Nothing
    else do let instr = instructions !! fromIntegral i
            rs <- get
            tell [logMaker (i, instr, rs)]
            doInstruction instr
     
runAndLog :: (MonadState RegisterState m, MonadWriter [a] m) => 
                (ComputationState -> a) -> RegisterMachine -> m Natural
runAndLog logMaker rm = runWithRM 0 >> gets (getRegister 0)
  where stepWithRM = flip runReaderT rm . stepAndLog logMaker
        runWithRM n = stepWithRM n >>= maybe (return ()) runWithRM
    
run :: MonadState RegisterState m => RegisterMachine -> m Natural
run = fmap fst . runWriterT . runAndLog (\a b c -> ())