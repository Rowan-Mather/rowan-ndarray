module Serialisation where

import Numskull
import DType

import System.IO
import qualified Data.Vector.Storable as S
import Data.Word (Word16)
import Data.List as List
import Foreign (ForeignPtr, Ptr, alloca, mallocBytes)
import Foreign.Storable (poke, peek, sizeOf)

import Data.Binary
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
getNumpyShape (NdArray _ s) = "(" <> (drop 1 $ take (length lshape -1) $ lshape) <> ",)"
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

buffInt :: Handle -> IO Int
buffInt h = do
  ptr <- mallocBytes 3
  hGetBuf h (ptr :: Ptr Int) (sizeOf (undefined ::Int))
  val <- peek ptr
  pure val

buffInts :: Handle -> Integer -> [IO Int]
buffInts _ 0 = []
buffInts h i = do (buffInt h) : buffInts h (i-1)

loadNpy :: FilePath -> IO NdArray
loadNpy path = withBinaryFile path ReadMode $ \h -> do
  descr <- hGetLine h
  let 
    attrs = splitOn ":" $ (splitOn "{" descr) !! 1
    dtype = takeWhile (/='\'') $ drop 2 $ attrs !! 1
    shapeStrs = splitOn "," $ takeWhile (/=')') $ drop 2 $ attrs !! 3
    shape = map (\x -> read x :: Integer) $ take (length shapeStrs -1) shapeStrs
    size = product shape
    temp = sizeOf (undefined ::Int)
  let lio = buffInts h size
  l <- traverse id lio
  pure $ NdArray (S.fromList l :: S.Vector Int) shape
  
  {-
  t <- alloca (\ptr -> hGetBuf h (ptr :: Ptr Int) temp)
  t2 <- alloca (\ptr -> hGetBuf h (ptr :: Ptr Int) temp)
  t3 <- alloca (\ptr -> hGetBuf h (ptr :: Ptr Int) temp)
  --rest <- thing
  let ts = show t ++ show t2 ++ show t3
  putStrLn $ ts
--  alloca $ \ptr -> poke ptr (fromIntegral headerLen :: Word16) >> hPutBuf h ptr 2
-}

--f :: Handle -> Int -> IO Int
--f h temp = malloc $ \ptr -> hGetBuf h (ptr :: Ptr Int) (temp :: Int)

loadNpz = undefined

saveNpz = undefined

testsave :: IO ()
testsave = do saveNpy "./src/testout/test123.npy" (NdArray (S.fromList [1,2,3 :: Int]) [3])

testload :: IO ()
testload = do 
  nd <- loadNpy "./src/testout/test123.npy"
  putStrLn $ show $ nd