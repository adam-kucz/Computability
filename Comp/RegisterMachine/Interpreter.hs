{-# LANGUAGE FlexibleContexts #-}

module Comp.RegisterMachine.Interpreter (
    getRegister,
    step,
) where 

import Control.Monad (MonadPlus, mzero)
import Control.Monad.State.Class (MonadState, put, get, gets)

import Data.Natural (Natural)

import Data.Map (adjust, alter, findWithDefault)

import Comp.RegisterMachine.Types

getRegister :: (MonadState RMState m) => Register -> m Natural
getRegister r = gets $ \(_, _, rs) -> findWithDefault 0 r rs

step :: (MonadState RMState m, MonadPlus m) => m ()
step = do
  (rm, loc, rs) <- get
  if fromIntegral loc >= length rm
    then mzero
    else case rm !! fromIntegral loc of
          HALT          -> mzero
          (INC r j)     -> put (rm, j, alter (Just . maybe 1 (+1)) r rs)
          (DEC r j1 j2) -> do a <- getRegister r
                              case a of
                                0 -> put (rm, j2, rs)
                                n -> put (rm, j1, adjust (subtract 1) r rs)