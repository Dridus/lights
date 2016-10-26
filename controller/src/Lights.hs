module Lights where

import ClassyPrelude
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Monad.Logger (MonadLogger, logDebug, logInfo, logWarn)
import Control.Monad.Loops (unfoldM)
import Control.Monad.State.Strict (StateT, state, runState, runStateT, get, put, modify')
import qualified Data.Map.Strict as StrictMap
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
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
deriving instance Eq TimeMs
deriving instance Num TimeMs
deriving instance Ord TimeMs
deriving instance Serialize TimeMs
deriving instance Show TimeMs

data Rgb = Rgb !Word8 !Word8 !Word8

deriving instance Eq Rgb
deriving instance Show Rgb
instance Serialize Rgb where
  put (Rgb r g b) = Serialize.putWord8 r >> Serialize.putWord8 g >> Serialize.putWord8 b
  get = Rgb <$> Serialize.getWord8 <*> Serialize.getWord8 <*> Serialize.getWord8
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

data Report
  = FreeLerpReport Word8
  | CroakReport Word8
  | LogReport Text
deriving instance Eq Report
deriving instance Show Report
instance Serialize Report where
  put = \ case
    FreeLerpReport n -> Serialize.putWord8 1 >> Serialize.putWord8 n
    CroakReport n -> Serialize.putWord8 2 >> Serialize.putWord8 n
    LogReport t -> Serialize.putWord8 (fromIntegral $ length t) >> Serialize.put (encodeUtf8 t)
  get = Serialize.getWord8 >>= \ case
    1 -> FreeLerpReport <$> Serialize.getWord8
    2 -> CroakReport <$> Serialize.getWord8
    3 -> LogReport <$> (Serialize.getWord8 >>= \ l -> decodeUtf8 <$> Serialize.getByteString (fromIntegral l))
    o -> fail $ "unknown report " <> show o

data Cmd
  = ResyncCmd
  | LerpCmd Lerp
  | NoLerpCmd
deriving instance Eq Cmd
deriving instance Show Cmd
instance Serialize Cmd where
  put = \ case
    ResyncCmd -> Serialize.putWord8 1
    LerpCmd l -> Serialize.putWord8 2 >> Serialize.put l
    NoLerpCmd -> Serialize.putWord8 3
  get = Serialize.getWord8 >>= \ case
    1 -> pure ResyncCmd
    2 -> LerpCmd <$> Serialize.get
    3 -> pure NoLerpCmd
    o -> fail $ "invalid cmd " <> show o

data Lerp = Lerp
  { lerpLed        :: Word8
  , lerpStartClock :: TimeMs
  , lerpDuration   :: Word16
  , lerpEndColor   :: Rgb
  }

deriving instance Eq Lerp
deriving instance Show Lerp
instance Serialize Lerp where
  put (Lerp {..}) = do
    Serialize.putWord8 lerpLed
    Serialize.putWord32le (unTimeMs lerpStartClock)
    Serialize.putWord16le lerpDuration
    Serialize.put lerpEndColor
  get = Lerp
    <$> Serialize.getWord8
    <*> (TimeMs <$> Serialize.getWord32le)
    <*> Serialize.getWord16le
    <*> Serialize.get

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
        let nowMs = realNowMs - epochMs
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

ramps :: Word8 -> TimeMs -> [(Word16, Rgb)] -> [Lerp]
ramps led t0 = go t0 Nothing
  where
    go t (Just c) ((d, c2):rest) | c == c2 = go (t + fromIntegral d) (Just c2) rest
    go t _ ((d, c):rest) = let tn = t + fromIntegral d in Lerp led tn d c : go tn (Just c) rest
    go _ _ [] = []
