module Serialisation where

import Numskull
import DType

import System.IO
import qualified Data.Vector.Storable as S
import Data.Word (Word16)
import Data.List as List
import Foreign (ForeignPtr, Ptr, alloca)
import Foreign.Storable (poke, sizeOf)

import Data.List.Split

-- built in numpy serialisation descriptions
getNumpyDType :: NdArray -> String
getNumpyDType (NdArray v _) = case show $ ty v of
  "Vector Int"      -> "<i8"
  "Vector Int32"    -> "<i4"
  "Vector Integer"  -> "<i8"
  "Vector Float"    -> "<f4"
  "Vector Double"   -> "<f8"
  "Vector Bool"     -> "<?"
  "Vector Char"     -> "<U1"
  _                 -> error "Non-standard types cannot be serialised. Yet."
  
getNumpyShape :: NdArray -> String
getNumpyShape (NdArray _ s) = "(" <> (take (length lshape -1) $ drop 1 $ lshape) <> ")"
  where lshape = show s

getElemSize :: NdArray -> Int
getElemSize (NdArray v _) = S.maximum $ S.map sizeOf v

-- Thanks Chris! https://github.com/cchalmers/dense/blob/6eced9f5a3ab6b5026fe4f7ab4f67a8bce4d6262/src/Data/Dense/Storable.hs#L686
-- see https://numpy.org/doc/stable/reference/generated/numpy.lib.format.html
saveNpy :: FilePath -> NdArray -> IO ()
saveNpy path (NdArray v s) = withBinaryFile path WriteMode $ \h -> do
  let
    -- Unpacked specs
    nd = (NdArray v s)
    dtype = getNumpyDType nd
    shape = getNumpyShape nd
    vectorSize = (fromInteger $ product s) :: Int
    elemSize = getElemSize nd
    -- Header string without length
    header = 
      "{'descr': '"<>   dtype   <> "', " <>
      "'fortran_order': False, "<>
      "'shape': "<>     shape   <> " }"
    -- Calculate header length (& padding)
    unpaddedLen = 6 + 2 + 2 + List.length header + 1
    paddedLen = ((unpaddedLen + 63) `Prelude.div` 64) * 64
    padding = paddedLen - unpaddedLen
    headerLen = List.length header + padding + 1
  -- Put header & padding
  hPutStr h "\x93NUMPY\x01\x00"
  alloca $ \ptr -> poke ptr (fromIntegral headerLen :: Word16) >> hPutBuf h ptr 2
  hPutStr h header
  hPutStr h (List.replicate padding ' ')
  hPutChar h '\n'
  -- Put vector body
  --unsafeWithPtr a $ \ptr -> hPutBuf h ptr (size nd * sizeOf (undefined :: (NumpyType a)=>a))
  S.unsafeWith v (\ptr -> hPutBuf h ptr (vectorSize * elemSize))

loadNpy :: FilePath -> IO ()
loadNpy path = withBinaryFile path ReadMode $ \h -> do
  descr <- hGetLine h
  let 
    attrs = splitOn ":" $ (splitOn "{" descr) !! 1
    dtype = takeWhile (/='\'') $ drop 2 $ attrs !! 1
    shapeStrs = splitOn "," $ takeWhile (/=')') $ drop 2 $ attrs !! 3
    shape = map (\x -> read x :: Integer) $ take (length shapeStrs -1) shapeStrs
    temp = sizeOf (undefined ::Int)
  alloca $ \ptr -> do
    buff <- hGetBuf h ptr temp
    putStrLn $ show $ buff
--  alloca $ \ptr -> poke ptr (fromIntegral headerLen :: Word16) >> hPutBuf h ptr 2

loadNpz = undefined

saveNpz = undefined

testsave :: IO ()
testsave = do saveNpy "./testout/idk.npy" (NdArray (S.fromList [1,2,3 :: Int]) [3])

testload :: IO ()
testload = do loadNpy "./src/testout/idk.npy"