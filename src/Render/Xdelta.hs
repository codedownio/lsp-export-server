{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- Binary diffs with the vendored xdelta3 (cbits/), in VCDIFF
module Render.Xdelta (
  encode
  , decode
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Foreign
import Foreign.C.Types


foreign import ccall "xdelta3.h xd3_encode_memory"
  c_xd3_encode_memory :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> CSize -> CInt -> IO CInt

foreign import ccall "xdelta3.h xd3_decode_memory"
  c_xd3_decode_memory :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> CSize -> CInt -> IO CInt

-- | @encode new old@ is the patch taking @old@ to @new@; empty when they are equal.
encode :: ByteString -> ByteString -> IO (Either String ByteString)
encode new old
  | new == old = return (Right B.empty)
  | otherwise = call c_xd3_encode_memory new old (max 1024 (B.length new + B.length old + 1024))

-- | @decode patch old@ rebuilds the bytes @encode@ was given.
decode :: ByteString -> ByteString -> IO (Either String ByteString)
decode patch old
  | B.null patch = return (Right old)
  | otherwise = call c_xd3_decode_memory patch old (max 1024 (B.length old + B.length patch + 1024))

call :: (Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> CSize -> CInt -> IO CInt)
     -> ByteString -> ByteString -> Int -> IO (Either String ByteString)
call f input source maxOutput =
  BU.unsafeUseAsCStringLen input $ \(inputPtr, inputLen) ->
    BU.unsafeUseAsCStringLen source $ \(sourcePtr, sourceLen) ->
      allocaBytes maxOutput $ \outputPtr ->
        alloca $ \outputSizePtr -> do
          poke outputSizePtr (fromIntegral maxOutput)
          result <- f (castPtr inputPtr) (fromIntegral inputLen) (castPtr sourcePtr) (fromIntegral sourceLen)
                      outputPtr outputSizePtr (fromIntegral maxOutput) 0
          case result of
            0 -> do
              n <- peek outputSizePtr
              Right <$> B.packCStringLen (castPtr outputPtr, fromIntegral n)
            code -> return (Left ("xdelta3 error " <> show code))
