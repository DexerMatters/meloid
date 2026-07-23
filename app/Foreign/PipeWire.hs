{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.PipeWire (
  startEQ,
  stopEQ,
  setEQ,
  readSpectrum,
) where

import Data.Vector qualified as Vec
import Foreign.C
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Ptr (Ptr)

foreign import ccall unsafe "meloid_eq_start"
  c_startEQ :: IO CInt

foreign import ccall unsafe "meloid_eq_stop"
  c_stopEQ :: IO ()

foreign import ccall unsafe "meloid_eq_set_gains"
  c_setEQ :: Ptr CDouble -> CSize -> IO CInt

foreign import ccall safe "meloid_eq_spectrum"
  c_spectrum :: Ptr CDouble -> CSize -> IO CInt

foreign import ccall unsafe "meloid_eq_error"
  c_error :: IO CString

startEQ :: IO (Either String ())
startEQ = result =<< c_startEQ

stopEQ :: IO ()
stopEQ = c_stopEQ

setEQ :: [Double] -> IO (Either String ())
setEQ gains = withArray (realToFrac <$> gains) $ \ptr -> result =<< c_setEQ ptr (fromIntegral $ length gains)

readSpectrum :: IO (Maybe (Vec.Vector Double))
readSpectrum =
  allocaArray 64 $ \ptr ->
    c_spectrum ptr 64 >>= \case
      1 -> Just . Vec.fromList . fmap realToFrac <$> peekArray 64 ptr
      _ -> pure Nothing

result :: CInt -> IO (Either String ())
result 0 = pure $ Right ()
result _ = Left <$> (c_error >>= peekCString)
