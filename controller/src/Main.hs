module Main where

import ClassyPrelude
import Control.Monad.Logger (runStdoutLoggingT)
import Data.List (cycle)
import Lights (HasScheduler, connect, newScheduler, runScheduler, schedule, numLeds, sleepUntil, ramps)
import System.Random (randomIO, randomRIO)

main :: IO ()
main = do
  [usbPath] <- getArgs
  h <- connect (unpack usbPath)
  scheduler <- newScheduler
  runStdoutLoggingT $
    flip runReaderT scheduler $ do
      runScheduler h scheduler `race_` ultrachristmas

chase :: (MonadReader r m, HasScheduler r, MonadIO m) => m ()
chase = do
  go 0 $ cycle [0..numLeds-1]
  where
    go _ [] = pure ()
    go t (led:leds) = do
      color <- liftIO randomIO
      schedule $ ramps led t [(500, color), (1000, color), (500, minBound)]
      sleepUntil $ t + 500
      go (t+500) leds

ultrachristmas :: (MonadReader r m, HasScheduler r, MonadIO m) => m ()
ultrachristmas  = do
  go 0
  where
    go t = do
      led <- liftIO $ randomRIO (0, numLeds-1)
      color <- liftIO randomIO
      schedule $ ramps led t [(500, color), (3000, color), (250, minBound)]
      sleepUntil $ t + 250
      go (t+250)
