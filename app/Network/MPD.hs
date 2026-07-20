{-# LANGUAGE PackageImports #-}

-- | Local fixes for libmpd's public API.
module Network.MPD (module LibMPD, listMounts, readPicture, albumArt) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.UTF8 qualified as UTF8
import Data.Maybe (listToMaybe)
import Network.Socket qualified as Socket
import System.IO (BufferMode (NoBuffering), IOMode (ReadWriteMode), hClose, hFlush, hSetBuffering)
import Text.Read (readMaybe)
import "libmpd" Network.MPD as LibMPD hiding (listMounts)
import "libmpd" Network.MPD.Core (getResponse)

{- | MPD replies with @mount:@ and @storage:@ records.  libmpd's parser
rejects otherwise valid records when the server includes newer fields.
-}
listMounts :: (LibMPD.MonadMPD m) => m [(String, String)]
listMounts = finish . foldl step (Nothing, Nothing, []) <$> getResponse "listmounts"
 where
  step (mountPath, storage, mounts) line = case (BS8.unpack key, field value) of
    ("mount", Just name) -> (Just name, Nothing, flush mountPath storage mounts)
    ("storage", Just storage') -> (mountPath, Just storage', mounts)
    _ -> (mountPath, storage, mounts)
   where
    (key, value) = BS8.break (== ':') line

  field value
    | BS8.isPrefixOf (BS8.pack ":") value = Just . BS8.unpack $ BS8.dropWhile (== ' ') (BS8.drop 1 value)
    | otherwise = Nothing

  finish (mountPath, storage, mounts) = reverse $ flush mountPath storage mounts
  flush (Just mountPath) (Just storage) mounts = (mountPath, storage) : mounts
  flush _ _ mounts = mounts

-- | Fetch embedded art through MPD's native binary protocol.
readPicture :: String -> String -> FilePath -> IO (Either String BS.ByteString)
readPicture = readArtwork "readpicture"

-- | Fallback for MPD versions that only support album-art extraction.
albumArt :: String -> String -> FilePath -> IO (Either String BS.ByteString)
albumArt = readArtwork "albumart"

readArtwork :: String -> String -> String -> FilePath -> IO (Either String BS.ByteString)
readArtwork command host port uri = do
  result <- try $ Socket.withSocketsDo $ do
    address <-
      maybe (ioError $ userError "MPD host did not resolve") pure . listToMaybe
        =<< Socket.getAddrInfo (Just Socket.defaultHints{Socket.addrSocketType = Socket.Stream}) (Just host) (Just port)
    bracket (open address) hClose $ \handle -> do
      greeting <- BS8.hGetLine handle
      unless (BS8.isPrefixOf (BS8.pack "OK MPD ") greeting) $ ioError $ userError "MPD did not send a protocol greeting"
      fetch handle 0 []
  pure $ either (Left . show) Right (result :: Either IOException BS.ByteString)
 where
  open address = do
    socket <- Socket.socket (Socket.addrFamily address) Socket.Stream Socket.defaultProtocol
    Socket.connect socket (Socket.addrAddress address)
    handle <- Socket.socketToHandle socket ReadWriteMode
    hSetBuffering handle NoBuffering
    pure handle

  fetch handle offset chunks = do
    BS8.hPutStr handle . UTF8.fromString $ command <> " \"" <> concatMap escape uri <> "\" " <> show offset <> "\n"
    hFlush handle
    (total, chunk) <- readChunk handle Nothing
    let nextOffset = offset + BS.length chunk
    if nextOffset >= total
      then pure $ BS.concat $ reverse (chunk : chunks)
      else
        if BS.null chunk
          then ioError $ userError "MPD returned an empty artwork chunk"
          else fetch handle nextOffset (chunk : chunks)

  readChunk handle total = do
    line <- BS8.hGetLine handle
    case () of
      _ | BS8.isPrefixOf (BS8.pack "ACK ") line -> ioError . userError $ BS8.unpack line
      _ | Just size <- field "size" line -> readChunk handle (Just size)
      _ | Just amount <- field "binary" line -> do
        chunk <- BS.hGet handle amount
        finish <- BS8.hGetLine handle
        unless (finish == BS8.pack "OK" || BS8.null finish) $
          ioError . userError $
            "unexpected MPD artwork response: " <> BS8.unpack finish
        whenBlank finish $
          BS8.hGetLine handle >>= \ok ->
            unless (ok == BS8.pack "OK") $
              ioError . userError $
                "unexpected MPD artwork response: " <> BS8.unpack ok
        maybe (ioError $ userError "MPD artwork response has no size") (\size -> pure (size, chunk)) total
      _ | line == BS8.pack "OK" -> ioError $ userError "MPD artwork response has no binary data"
      _ -> readChunk handle total

  field key line = BS8.stripPrefix (BS8.pack $ key <> ": ") line >>= readMaybe . BS8.unpack
  whenBlank line action = if BS8.null line then action else pure ()
  escape '\\' = "\\\\"
  escape '"' = "\\\""
  escape char = [char]
