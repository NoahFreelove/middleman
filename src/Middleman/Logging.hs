module Middleman.Logging
  ( Logger
  , newLogger
  , closeLogger
  , logInfo
  , logWarn
  , logError
  , logRequest
  , logResponse
  , logScriptExec
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified System.Log.FastLogger as FL

-- | Opaque logger type wrapping fast-logger
data Logger = Logger
  { loggerSet :: FL.TimedFastLogger
  , loggerCleanup :: IO ()
  }

-- | Create a new logger that writes to stdout
newLogger :: IO Logger
newLogger = do
  timeCache <- FL.newTimeCache FL.simpleTimeFormat'
  (logger, cleanup) <- FL.newTimedFastLogger timeCache (FL.LogStdout FL.defaultBufSize)
  pure (Logger logger cleanup)

-- | Close the logger and flush pending output
closeLogger :: Logger -> IO ()
closeLogger = loggerCleanup

-- | Log at INFO level
logInfo :: Logger -> Text -> IO ()
logInfo logger msg = logWithLevel logger "INFO" msg

-- | Log at WARN level
logWarn :: Logger -> Text -> IO ()
logWarn logger msg = logWithLevel logger "WARN" msg

-- | Log at ERROR level
logError :: Logger -> Text -> IO ()
logError logger msg = logWithLevel logger "ERROR" msg

-- | Log an incoming request
logRequest :: Logger -> ByteString -> Text -> IO ()
logRequest logger method path =
  logInfo logger ("-> " <> decodeUtf8 method <> " " <> path)

-- | Log an outgoing response
logResponse :: Logger -> Int -> Text -> IO ()
logResponse logger status path =
  logInfo logger ("<- " <> pack (show status) <> " " <> path)

-- | Log script execution
logScriptExec :: Logger -> Text -> FilePath -> IO ()
logScriptExec logger phase scriptPath' =
  logInfo logger ("script [" <> phase <> "] " <> pack scriptPath')

-- Internal helpers

logWithLevel :: Logger -> String -> Text -> IO ()
logWithLevel Logger{..} level msg =
  loggerSet (\time ->
    FL.toLogStr time
      <> FL.toLogStr (" [" :: String)
      <> FL.toLogStr level
      <> FL.toLogStr ("] " :: String)
      <> FL.toLogStr (unpack msg)
      <> FL.toLogStr ("\n" :: String)
  )
