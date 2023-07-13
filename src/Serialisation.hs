module Serialisation where

import Numskull
import Data.Vector.Storable (unsafeWith)

-- built in numpy serialisation descriptions
getNumpyDType :: NdArray -> String
getNumpyDType _ = "<i8" --int64 explicitly for now

-- tuple of ints
getNumpyShape :: NdArray -> String
getNumpyShape _ "(3,)" --3x1 explicitly for now


-- Thanks Chris! https://github.com/cchalmers/dense/blob/6eced9f5a3ab6b5026fe4f7ab4f67a8bce4d6262/src/Data/Dense/Storable.hs#L686
-- see https://numpy.org/doc/stable/reference/generated/numpy.lib.format.html
saveNpy :: FilePath -> NdArray -> IO ()
saveNpy path nd = withBinaryFile path WriteMode $ \h -> do
  let
    -- Unpack vector
    (NdArray v) = nd
    -- Header string without length
    header = 
      "{'descr': '"++   getNumpyDType nd  ++ "', " ++
      "'fortran_order': False, "          ++
      "'shape': "++     getNumpyShape nd  ++ " }"
    -- Calculate header length (& padding)
    unpaddedLen = 6 + 2 + 2 + length header + 1
    paddedLen = ((unpaddedLen + 63) `div` 64) * 64
    padding = paddedLen - unpaddedLen
    headerLen = length header + padding + 1
  -- Put header & padding
  hPutStr h "\x93NUMPY\x01\x00"
  alloca $ \ptr -> poke ptr (fromIntegral headerLen :: Word16) >> hPutBuf h ptr 2
  hPutStr h header
  hPutStr h (List.replicate padding ' ')
  hPutChar h '\n'
  -- Put vector body
  --unsafeWithPtr a $ \ptr -> hPutBuf h ptr (size nd * sizeOf (undefined :: (NumpyType a)=>a))
  unsafeWith v (\ptr -> hPutBuf h ptr (size v * sizeOf (undefined :: Int)))

saveNpz = undefined

loadNpy = undefined

loadNpz = undefined

testsave = do saveNpy "./testout/idk.npy" (NdArray (fromList [1,2,3]))