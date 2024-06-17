{-# LANGUAGE StrictData #-}

module Distribution.Client.TimingLog
  ( closeTimingLog
  , initTimingLog
  , timingLogBracket
  ) where

import Control.Monad (unless)

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import System.FilePath ((</>))
import System.IO (BufferMode (..), IOMode (..), Handle, hClose, hPutStrLn, hSetBuffering, openFile)
import System.IO.Unsafe (unsafePerformIO)

data LogEntry = LogEntry
  { leStart :: UTCTime
  , leLabal :: String
  , leDuration :: NominalDiffTime
  }

closeTimingLog :: IO ()
closeTimingLog = do
  readIORef logHandleRef >>= \ mLogHandle ->
    case mLogHandle of
      Just lh -> do
        hClose lh
        writeIORef logHandleRef Nothing
      Nothing -> pure ()

initTimingLog :: Bool -> FilePath -> IO ()
initTimingLog enable fpath =
  unless enable $ do
    let logFile = fpath </> "timing-log.txt"
    logHandle <- openFile logFile WriteMode
    hSetBuffering logHandle NoBuffering
    writeIORef logHandleRef $ Just logHandle
    putStrLn $ "Logging timing information to " ++ logFile

timingLogBracket :: String -> IO a -> IO a
timingLogBracket str action =
  readIORef logHandleRef >>= \ mLogHandle ->
    case mLogHandle of
      Nothing -> action
      Just lh -> do
        before <- getCurrentTime
        result <- action
        after <- getCurrentTime
        renderLogEntry lh $ LogEntry before str (diffUTCTime after before)
        pure result

-- -------------------------------------------------------------------------------------------------

{-# NOINLINE logHandleRef #-}
logHandleRef :: IORef (Maybe Handle)
logHandleRef = unsafePerformIO $ newIORef Nothing

renderLogEntry :: Handle -> LogEntry -> IO ()
renderLogEntry hdl le = do
  hPutStrLn hdl $ mconcat [ renderStartTime (leStart le), ": ", leLabal le, " took ", show (leDuration le) ]

renderStartTime :: UTCTime -> String
renderStartTime = 
  formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S UTC"
