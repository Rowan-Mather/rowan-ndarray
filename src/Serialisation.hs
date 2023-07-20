module Serialisation where

import Numskull
import DType
import NdArray

import System.IO
import Data.List as List
import Data.List.Split
import qualified Data.Vector.Storable as S
import Foreign (Ptr, alloca, mallocBytes)
import Foreign.Storable (poke, peek, sizeOf)
import Data.Word (Word16)

-- | HASKELL TO PYTHON | -- 

-- Built in numpy serialisation descriptions
getNumpyDType :: NdArray -> String
getNumpyDType (NdArray _ v) = case show $ ty v of
  "Vector Int"      -> "<i8"
  "Vector Int32"    -> "<i4"
  "Vector Integer"  -> "<i8"
  "Vector Float"    -> "<f4"
  "Vector Double"   -> "<f8"
  "Vector Bool"     -> "<?"
  "Vector Char"     -> "<U1"
  _                 -> error "Non-standard types cannot be serialised. Yet."
  
-- Converts shape list to a string of the Numpy tuple form e.g. (3,2,)  
getNumpyShape :: NdArray -> String
getNumpyShape (NdArray s _) = "(" <> (drop 1 $ take (length lshape -1) $ lshape) <> ",)"
  where lshape = show s

-- Maximum memory required for any single element in the array
getElemSize :: NdArray -> Int
getElemSize (NdArray _ v) = S.maximum $ S.map sizeOf v

-- Saves any of the standard types defined above as a .npy
-- Thanks Chris! https://github.com/cchalmers/dense/blob/6eced9f5a3ab6b5026fe4f7ab4f67a8bce4d6262/src/Data/Dense/Storable.hs#L686
-- see https://numpy.org/doc/stable/reference/generated/numpy.lib.format.html
saveNpy :: FilePath -> NdArray -> IO ()
saveNpy path (NdArray s v) = withBinaryFile path WriteMode $ \h -> do
  let
    -- Unpacked specs
    nd = (NdArray s v)
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
  S.unsafeWith v (\ptr -> hPutBuf h ptr (vectorSize * elemSize))


-- | PYTHON TO HASKELL | -- 

-- Reads an Int value from the binary accessed via the handle
buffInt :: Handle -> IO Int
buffInt h = do
  ptr <- mallocBytes 4
  _ <- hGetBuf h (ptr :: Ptr Int) (sizeOf (undefined ::Int))
  val <- peek ptr
  pure val

buffInts :: Handle -> Integer -> [IO Int]
buffInts _ 0 = []
buffInts h i = do (buffInt h) : buffInts h (i-1)

-- NB: Currently only works with Ints specifically but this is easy to extend :) Working on it
-- Loads an NdArray from a .npy file
loadNpy :: FilePath -> IO NdArray
loadNpy path = withBinaryFile path ReadMode $ \h -> do
  -- Unpacks and parses the header to get the array type and size
  descr <- hGetLine h
  let 
    attrs = splitOn ":" $ (splitOn "{" descr) !! 1
    dtype = takeWhile (/='\'') $ drop 2 $ attrs !! 1
    shapeStrs = splitOn "," $ takeWhile (/=')') $ drop 2 $ attrs !! 3
    shape = map (\x -> read x :: Integer) $ take (length shapeStrs -1) shapeStrs
    size = product shape
  -- Reads the array itself into a list
  let lio = buffInts h size
  l <- traverse id lio
  -- Converts the list & shape to an NdArray
  pure $ NdArray shape (S.fromList l :: S.Vector Int) 


-- Try it! It will probably break easily

testsave :: IO ()
testsave = do saveNpy "./src/testout/test123.npy" (NdArray [3] (S.fromList [1,2,3 :: Int]) )

testload :: IO ()
testload = do 
  nd <- loadNpy "./src/testout/test123.npy"
  putStrLn $ show $ nd