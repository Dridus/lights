module Lights where

import ClassyPrelude
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Monad.Logger (MonadLogger, logDebug, logInfo, logWarn)
import Control.Monad.Loops (unfoldM)
import Control.Monad.State.Strict (StateT, state, runState, runStateT, get, put, modify')
import qualified Data.Map.Strict as StrictMap
import Data.Text (splitOn, strip)
import Data.Word (Word16)
import System.IO (BufferMode(NoBuffering), hSetBuffering, hSetEcho)
import System.Posix.IO
  ( OpenFileFlags(OpenFileFlags, append, exclusive, noctty, nonBlock, trunc)
  , OpenMode(ReadWrite)
  , openFd, fdToHandle
  )
import System.Posix.Terminal
  ( BaudRate(B115200)
  , TerminalMode(ReadEnable, LocalMode, StartStopInput, StartStopOutput, EnableEcho, EnableParity, ProcessInput, ProcessOutput, StripHighBit)
  , TerminalState(Immediately)
  , withBits, withMode, withoutMode, withInputSpeed, withOutputSpeed
  , getTerminalAttributes, setTerminalAttributes
  )
import System.Random (Random(randomR, random))

numLeds :: Word8
numLeds = 100

interWriteDelay :: Int
interWriteDelay = 100000 -- µs

newtype TimeMs = TimeMs { unTimeMs :: Word32 }

deriving instance Bounded TimeMs
deriving instance Enum TimeMs
deriving instance Eq TimeMs
deriving instance Integral TimeMs
deriving instance Num TimeMs
deriving instance Ord TimeMs
deriving instance Random TimeMs
deriving instance Real TimeMs
deriving instance Show TimeMs

data Rgb = Rgb !Word8 !Word8 !Word8

deriving instance Eq Rgb
deriving instance Show Rgb
instance Bounded Rgb where
  minBound = Rgb 0 0 0
  maxBound = Rgb 0xff 0xff 0xff
instance Random Rgb where
  randomR (Rgb lr lg lb, Rgb hr hg hb) =
    runState $
      Rgb
        <$> state (randomR (lr, hr))
        <*> state (randomR (lg, hg))
        <*> state (randomR (lb, hb))
  random = randomR (minBound, maxBound)

mapRgb :: (Word8 -> Word8) -> Rgb -> Rgb
mapRgb f (Rgb r g b) = Rgb (f r) (f g) (f b)

data Report
  = FreeLerpReport Word8
  | CroakReport Word8
  | LogReport Text
deriving instance Eq Report
deriving instance Show Report

data Cmd
  = ResyncCmd
  | LerpCmd Lerp
  | NoLerpCmd
deriving instance Eq Cmd
deriving instance Show Cmd

data Lerp = Lerp
  { lerpLed        :: Word8
  , lerpStartClock :: TimeMs
  , lerpDuration   :: Word16
  , lerpEndColor   :: Rgb
  }

deriving instance Eq Lerp
deriving instance Show Lerp

data Scheduler = Scheduler
  { schedulerLerpChan :: TChan Lerp
  , schedulerClockTv  :: TVar TimeMs
  }

class HasScheduler s where
  schedulerOf :: s -> Scheduler
instance HasScheduler Scheduler where
  schedulerOf = id

connect :: MonadIO m => FilePath -> m Handle
connect fp = liftIO $ do
  fd <- openFd fp ReadWrite Nothing OpenFileFlags { append = True, exclusive = False, noctty = True, nonBlock = False, trunc = False }
  tattrs <- getTerminalAttributes fd
  let tattrs' = tattrs
        `withInputSpeed` B115200
        `withOutputSpeed` B115200
        `withBits` 8
        `withMode` ReadEnable
        `withMode` LocalMode
        `withoutMode` StartStopInput
        `withoutMode` StartStopOutput
        `withoutMode` EnableEcho
        `withoutMode` EnableParity
        `withoutMode` ProcessInput
        `withoutMode` ProcessOutput
        `withoutMode` StripHighBit

  setTerminalAttributes fd tattrs' Immediately
  h <- fdToHandle fd
  hSetBuffering h NoBuffering
  hSetEcho h False
  pure h

writeCmd :: (MonadIO m, MonadLogger m) => Handle -> Cmd -> m ()
writeCmd h cmd = do
  let l = case cmd of
        ResyncCmd ->
          "1"
        LerpCmd (Lerp led (TimeMs time) dur (Rgb r g b)) ->
          "2 " <> intercalate " " [tshow led, tshow time, tshow dur, tshow r, tshow g, tshow b]
        NoLerpCmd ->
          "3"
  when False $ $logDebug $ ">> Arduino: " <> tshow l <> " for: " <> tshow cmd
  hPut h $ l <> "\n"

newScheduler :: MonadIO m => m Scheduler
newScheduler = liftIO $ Scheduler <$> newTChanIO <*> newTVarIO 0

runScheduler :: forall m. (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Handle -> Scheduler -> m ()
runScheduler h (Scheduler {..}) = do
  let readLerpReport :: (MonadIO n, MonadLogger n) => n (TimeMs, Bool, Word8)
      readLerpReport = do
        l <- liftIO $ strip <$> hGetLine h
        case splitOn " " l of
          [readMay -> Just t, "x", readMay -> Just n] -> do
            -- $logDebug $ "<< Arduino: " <> l
            pure (TimeMs t, False, n)
          [readMay -> Just t, readMay -> Just n] -> do
            -- $logDebug $ "<< Arduino: " <> l
            pure (TimeMs t, True, n)
          _ -> do
            $logInfo $ "<< Arduino: " <> l
            readLerpReport

      handshake :: m TimeMs
      handshake = do
        $logInfo "Waiting for Arduino..."
        (_, _, initialLerps) <- readLerpReport
        $logInfo $ "Arduino initial report " <> tshow initialLerps <> ". Sending resync."
        writeCmd h ResyncCmd
        $logInfo $ "Waiting for Arduino to acknowlege resync..."
        (epochMs, _, revisedLerps) <- readLerpReport
        if revisedLerps == 0
          then do
            $logInfo $ "Acknowledged, setting epoch and resyncing again."
            writeCmd h ResyncCmd
            $logInfo $ "Acknowledged, epoch set at " <> tshow epochMs
            pure epochMs
          else do
            $logWarn $ tshow revisedLerps <> " reported. Trying again."
            handshake

  epochMs <- handshake

  let sendNextLerp :: TimeMs -> StateT (Map (TimeMs, Word8) Lerp) m ()
      sendNextLerp nowMs = get >>= \ case
        work | null work ->
          writeCmd h NoLerpCmd
        (StrictMap.deleteFindMin -> (((t, _), l), rest)) | t <= nowMs + 1000 -> do
          let l' = l { lerpStartClock = lerpStartClock l + epochMs }
          when False $ $logDebug $ "sending lerp " <> tshow l'
          writeCmd h (LerpCmd l') >> put rest
        _ ->
          writeCmd h NoLerpCmd

      ingestNewWork :: StateT (Map (TimeMs, Word8) Lerp) m ()
      ingestNewWork = do
        newLerps <- atomically (unfoldM (tryReadTChan schedulerLerpChan))
        modify' $ StrictMap.union (mapFromList $ map (keyForLerp &&& id) newLerps)
        where keyForLerp = lerpStartClock &&& lerpLed

      schedulerStep :: StateT (Map (TimeMs, Word8) Lerp) m ()
      schedulerStep = do
        (realNowMs, wantLerps, usedLerps) <- readLerpReport
        let nowMs = if realNowMs < epochMs then 0 else realNowMs - epochMs
        when False $ $logDebug $ "Arduino: used lerps = " <> tshow usedLerps
        atomically $ writeTVar schedulerClockTv nowMs
        if not wantLerps
          then writeCmd h NoLerpCmd
          else sendNextLerp nowMs

        ingestNewWork

  void $ runStateT (forever schedulerStep) mempty

getSchedulerClock :: (MonadReader r m, HasScheduler r, MonadIO m) => m TimeMs
getSchedulerClock = asks schedulerOf >>= atomically . readTVar . schedulerClockTv

sleepUntil :: (MonadReader r m, HasScheduler r, MonadIO m) => TimeMs -> m ()
sleepUntil target = do
  Scheduler { schedulerClockTv } <- asks schedulerOf
  atomically (readTVar schedulerClockTv >>= guard . (>= target))

schedule :: (MonadReader r m, HasScheduler r, MonadIO m) => [Lerp] -> m ()
schedule ls = do
  Scheduler { schedulerLerpChan } <- asks schedulerOf
  atomically $ traverse_ (writeTChan schedulerLerpChan) ls

ramps :: Word8 -> TimeMs -> [(Rgb, Word16)] -> [Lerp]
ramps led t0 = go t0 Nothing
  where
    go t (Just c) ((c2, d):rest) | c == c2 = go (t + fromIntegral d) (Just c2) rest
    go t _ ((c, d):rest) = let tn = t + fromIntegral d in Lerp led t d c : go tn (Just c) rest
    go _ _ [] = []
