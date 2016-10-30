module Main where

import ClassyPrelude
import Control.Monad.Logger (MonadLogger, logDebug, logInfo, runStdoutLoggingT)
import Control.Monad.Random (MonadRandom, getRandomR, getRandom, evalRandT, uniform)
import Data.List (cycle)
import Lights (HasScheduler, Lerp(Lerp), Rgb(Rgb), TimeMs, connect, newScheduler, runScheduler, schedule, numLeds, sleepUntil, getSchedulerClock, ramps, unTimeMs)
import LoggingOrphans ()
import System.Random (newStdGen)
import System.Random.Shuffle (shuffleM)

main :: IO ()
main = do
  [usbPath] <- getArgs
  h <- connect (unpack usbPath)
  scheduler <- newScheduler
  gen <- newStdGen
  runStdoutLoggingT $
    flip runReaderT scheduler $ do
      runScheduler h scheduler `race_` evalRandT master gen

master :: forall r m. (MonadLogger m, MonadRandom m, MonadReader r m, HasScheduler r, MonadIO m) => m ()
master = do
  sleepUntil 1
  forever $ do 
    (label, action) <- uniform masterList
    now <- getSchedulerClock
    $logInfo $ "At " <> tshow (unTimeMs now) <> ": " <> label
    action
  where
    masterList :: [(Text, m ())]
    masterList =
      [ ("chase", chase)
      , ("ultrachristmas", ultrachristmas)
      , ("show a message", showMessages)
      , ("show a message", showMessages)
      , ("pulse", pulse)
      ]

chase :: (MonadRandom m, MonadReader r m, HasScheduler r, MonadIO m) => m ()
chase = do
  t0 <- (+1000) <$> getSchedulerClock
  let rampTime = 150 
      go _ [] = pure ()
      go t _ | t >= t0 + 20000 = pure ()
      go t (led:leds) = do
        color <- getRandom
        schedule $ ramps led t [(color, rampTime), (color, 400), (minBound, rampTime)]
        sleepUntil $ t + fromIntegral rampTime
        go (t + fromIntegral rampTime) leds
  go t0 $ [1..numLeds]

ultrachristmas :: (MonadLogger m, MonadRandom m, MonadReader r m, HasScheduler r, MonadIO m) => m ()
ultrachristmas  = do
  t0 <- (+1000) <$> getSchedulerClock
  let go t | t >= t0 + 30000 = pure ()
      go t = do        
        color <- getRandom
        let elapsed  = t - t0
            fraction = min 1.0 $ fromIntegral elapsed / (20000.0 :: Double)
            delay    = max 50 . truncate $ 50.0 + (1.0-fraction) * 200.0
        led1 <- getRandomR (1, numLeds)
        led2 <- getRandomR (1, numLeds)
        schedule $ ramps led1 t [(color, delay), (color, 3000), (minBound, delay)]
        schedule $ ramps led2 t [(color, delay), (color, 3000), (minBound, delay)]
        sleepUntil $ t + fromIntegral delay
        go $ t + fromIntegral delay
  go t0

pulse :: (MonadLogger m, MonadReader r m, HasScheduler r, MonadIO m) => m ()
pulse = do
  t0 <- (+1000) <$> getSchedulerClock
  schedule $ ramps 0 t0
    [ (Rgb 200 0 0, rampTime), (minBound, rampTime)
    , (Rgb 200 0 0, rampTime), (minBound, rampTime) 
    , (Rgb 200 0 0, rampTime), (minBound, rampTime)
    ]
  sleepUntil $ t0 + (fromIntegral rampTime * 6)
  where rampTime = 5000


showMessages :: forall r m. (MonadLogger m, MonadRandom m, MonadReader r m, HasScheduler r, MonadIO m) => m ()
showMessages = do
  t0 <- (+1000) <$> getSchedulerClock

  let intro :: TimeMs -> [Word8] -> m TimeMs
      intro t _ | t >= t0 + 5000 = t <$ schedule [Lerp 0 t 0 minBound]
      intro t leds = do
        let elapsed         = t - t0
            elapsedFraction = min 1.0 $ fromIntegral elapsed / (5000.0 :: Double)
            fraction | elapsedFraction >= 0.5 = 1 - (elapsedFraction * 2.0 - 1)
                     | otherwise              = elapsedFraction * 2.0
            nleds           = min 5 . truncate $ fraction * 5.0
            (currentLeds, futureLeds) = splitAt nleds leds

        for_ currentLeds $ \ led -> do
          color1    <- getRandom
          color2    <- getRandom
          ramp1Time <- getRandomR (0, 100)
          ramp2Time <- getRandomR (0, 50)

          schedule $ ramps led t [ (color1, ramp1Time), (color2, ramp2Time), (minBound, ramp1Time) ]

        let tn = t + 100
        intro tn futureLeds

      showMessage :: TimeMs -> m ()
      showMessage t1 = do
        message <- uniform messages
        $logInfo $ "Showing: " <> message
        let scheduleLetter t = \ case
              (flip lookup letterMapping -> Just led) -> do
                color     <- getRandom
                holdTime  <- (800+) <$> getRandomR (0, 400)
                delayTime <- (200+) <$> getRandomR (0, 100)

                (t + fromIntegral holdTime + delayTime) <$ schedule (ramps led t [(color, 0), (color, holdTime), (minBound, 0)])
              _ ->
                (t+) . (4750+) <$> getRandomR (0, 500)
        tn <- foldlM scheduleLetter t1 message
        delay <- getRandomR (1000, 5000)
        $logDebug $ "Waiting till " <> tshow (tn + delay)
        sleepUntil $ tn + delay

  t1 <- intro t0 . cycle =<< shuffleM [1..numLeds]
  sleepUntil $ t1
  showMessage $ t1 + 1000


messages :: [Text]
messages =
  [ "RUN"
  , "BOO"
  , "BOOURNS"
  , "HELP"
  , "EEK"
  , "DAMNED"
  , "GETOUT"
  , "DEADINSIDE"
  , "IMHERE"
  , "LOOKBEHINDYOU"
  , "CURSED"
  , "CANTINA"
  , "BLOOD"
  , "FISHY"
  , "WOOF"
  , "DIE"
  ]

letterMapping :: Map Char Word8
letterMapping = mapFromList
  [ ('A', 29)
  , ('B', 24)
  , ('C', 20)
  , ('D', 17)
  , ('E', 14)
  , ('F', 11)
  , ('G', 7 )
  , ('H', 3 )
  , ('I', 38)
  , ('J', 41)
  , ('K', 44)
  , ('L', 48)
  , ('M', 51)
  , ('N', 54)
  , ('O', 57)
  , ('P', 62)
  , ('Q', 65)
  , ('R', 98)
  , ('S', 96)
  , ('T', 91)
  , ('U', 87)
  , ('V', 84)
  , ('W', 80)
  , ('X', 77)
  , ('Y', 75)
  , ('Z', 72)
  ]
