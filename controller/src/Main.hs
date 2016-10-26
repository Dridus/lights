module Main where

import ClassyPrelude
import Control.Monad.Logger (MonadLogger, logDebug, logInfo, runStdoutLoggingT)
import Data.List (cycle)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Lights (HasScheduler, connect, newScheduler, runScheduler, schedule, numLeds, sleepUntil, getSchedulerClock, ramps)
import System.Random (randomIO, randomRIO)

main :: IO ()
main = do
  [usbPath] <- getArgs
  h <- connect (unpack usbPath)
  scheduler <- newScheduler
  runStdoutLoggingT $
    flip runReaderT scheduler $ do
      runScheduler h scheduler `race_` showMessages

chase :: (MonadReader r m, HasScheduler r, MonadIO m) => m ()
chase = do
  go 0 $ cycle [0..numLeds-1]
  where
    go _ [] = pure ()
    go t (led:leds) = do
      color <- liftIO randomIO
      schedule $ ramps led t [(color, 500), (color, 1000), (minBound, 500)]
      sleepUntil $ t + 500
      go (t+500) leds

ultrachristmas :: (MonadReader r m, HasScheduler r, MonadIO m) => m ()
ultrachristmas  = do
  go 0
  where
    go t = do
      led <- liftIO $ randomRIO (0, numLeds-1)
      color <- liftIO randomIO
      schedule $ ramps led t [(color, 500), (color, 3000), (minBound, 250)]
      sleepUntil $ t + 250
      go (t+250)

showMessages :: (MonadLogger m, MonadReader r m, HasScheduler r, MonadIO m) => m ()
showMessages = do
  letterColors <- for letterMapping $ \ led -> (led, ) <$> liftIO randomIO

  sleepUntil 1

  forever $ do
    t0 <- (+1000) <$> getSchedulerClock
    message <- liftIO $ ((NEL.!!) messages) <$> randomRIO (0, length messages - 1)
    $logInfo $ "Showing: " <> message
    let scheduleLetter t = \ case
          (flip lookup letterColors -> Just (led, color)) -> do
            len <- (800+) <$> liftIO (randomRIO (0, 400))
            interval <- (200+) <$> liftIO (randomRIO (0, 100))
            (t + fromIntegral len + interval) <$ schedule (ramps led t [(color, 0), (color, len), (minBound, 0)])
          _ ->
            (4750+) <$> liftIO (randomRIO (0, 500))
    tn <- foldlM scheduleLetter t0 message
    delay <- liftIO $ randomRIO (5000, 60000)
    $logDebug $ "Waiting till " <> tshow (tn + delay)
    sleepUntil $ tn + delay

messages :: NonEmpty Text
messages = NEL.fromList
  [ "RUN"
  , "BOO"
  , "BOOURNS"
  ," HELP"
  , "EEK"
  , "DEVIL"
  , "GET OUT"
  , "DEAD"
  , "HERE"
  , "WITCH"
  , "CURSED"
  , "CANTINA"
  , "BLOOD"
  , "FISHY"
  , "DOG"
  , "SOUL"
  ]

letterMapping :: Map Char Word8
letterMapping = mapFromList
  [ ('A', 27)
  , ('B', 22)
  , ('C', 18)
  , ('D', 15)
  , ('E', 12)
  , ('F', 9 )
  , ('G', 5 )
  , ('H', 1 )
  , ('I', 37)
  , ('J', 40)
  , ('K', 43)
  , ('L', 47)
  , ('M', 50)
  , ('N', 53)
  , ('O', 56)
  , ('P', 60)
  , ('Q', 63)
  , ('R', 97)
  , ('S', 94)
  , ('T', 91)
  , ('U', 86)
  , ('V', 84)
  , ('W', 81)
  , ('X', 76)
  , ('Y', 74)
  , ('Z', 70)
  ]
