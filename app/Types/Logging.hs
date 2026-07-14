{- | Logging helpers used by the UI event stream. The formatting
stays here so all log output is consistent.
-}
module Types.Logging (
  formatLog,
  logEv,
  logInfo,
  logWarn,
  logError,
  logDebug,
  logReq,
  logReqInfo,
  logReqError,
  logReqWarn,
  logReqDebug,
) where

import Brick.BChan (BChan, writeBChan)
import Brick.Types (EventM)
import Control.Monad.IO.Class
import Data.Time
import Lens.Micro.Mtl
import Text.Printf (printf)
import Types.Core
import Types.Identity (MName)
import Types.Model (St, stLogs)

-- | Format a log line with a timestamp and source label.
formatLog :: String -> String -> IO String
formatLog from msg = do
  timestamp <- getZonedTime
  let timeFormat = "%H:%M:%S"
      timeStr = formatTime defaultTimeLocale timeFormat timestamp
  pure $ printf "[%s] [%s]: %s" timeStr from msg

-- | Emit a log event into the Brick channel.
logEv :: BChan (Event' a) -> LogLevel -> String -> String -> IO ()
logEv chan level from msg = do
  formatted <- formatLog from msg
  writeBChan chan (Log (level, formatted))

-- | Emit an informational log line.
logInfo :: BChan (Event' a) -> String -> String -> IO ()
logInfo chan from msg = logEv chan Info from msg

-- | Emit an error log line.
logError :: BChan (Event' a) -> String -> String -> IO ()
logError chan from msg = logEv chan Error from msg

-- | Emit a warning log line.
logWarn :: BChan (Event' a) -> String -> String -> IO ()
logWarn chan from msg = logEv chan Warn from msg

-- | Emit a debug log line.
logDebug :: BChan (Event' a) -> String -> String -> IO ()
logDebug chan from msg = logEv chan Debug from msg

-- | Emit a log request.
logReq :: LogLevel -> String -> String -> EventM (MName St) St ()
logReq level from msg = do
  formatted <- liftIO $ formatLog from msg
  stLogs %= ((level, formatted) :)

-- | Emit an informational log request.
logReqInfo :: String -> String -> EventM (MName St) St ()
logReqInfo from msg = logReq Info from msg

-- | Emit an error log request.
logReqError :: String -> String -> EventM (MName St) St ()
logReqError from msg = logReq Error from msg

-- | Emit a warning log request.
logReqWarn :: String -> String -> EventM (MName St) St ()
logReqWarn from msg = logReq Warn from msg

-- | Emit a debug log request.
logReqDebug :: String -> String -> EventM (MName St) St ()
logReqDebug from msg = logReq Debug from msg