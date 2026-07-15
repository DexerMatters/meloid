{-# LANGUAGE PackageImports #-}

-- | Local fixes for libmpd's public API.
module Network.MPD (module LibMPD, listMounts) where

import Data.ByteString.Char8 qualified as BS
import "libmpd" Network.MPD as LibMPD hiding (listMounts)
import "libmpd" Network.MPD.Core (getResponse)

-- | MPD replies with @mount:@ and @storage:@ records.  libmpd's parser
-- rejects otherwise valid records when the server includes newer fields.
listMounts :: LibMPD.MonadMPD m => m [(String, String)]
listMounts = finish . foldl step (Nothing, Nothing, []) <$> getResponse "listmounts"
 where
  step (mountPath, storage, mounts) line = case (BS.unpack key, field value) of
    ("mount", Just name) -> (Just name, Nothing, flush mountPath storage mounts)
    ("storage", Just storage') -> (mountPath, Just storage', mounts)
    _ -> (mountPath, storage, mounts)
   where
    (key, value) = BS.break (== ':') line

  field value
    | BS.isPrefixOf (BS.pack ":") value = Just . BS.unpack $ BS.dropWhile (== ' ') (BS.drop 1 value)
    | otherwise = Nothing

  finish (mountPath, storage, mounts) = reverse $ flush mountPath storage mounts
  flush (Just mountPath) (Just storage) mounts = (mountPath, storage) : mounts
  flush _ _ mounts = mounts
