{-# OPTIONS_GHC -fno-warn-orphans #-}
module LoggingOrphans where

import ClassyPrelude
import Control.Monad.Logger (MonadLogger, monadLoggerLog)
import Control.Monad.Random (RandT)

instance MonadLogger m => MonadLogger (RandT g m) where
  monadLoggerLog loc src lvl msg = lift $ monadLoggerLog loc src lvl msg