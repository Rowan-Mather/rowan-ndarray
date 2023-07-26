{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}


module Serialisation where

import Numskull
import DType
import NdArray

import Data.Int
import System.IO
import Data.List as List
import Data.List.Split
import qualified Data.Vector.Storable as V
import qualified Data.Map as M
import Type.Reflection
import Foreign (Ptr, alloca, mallocBytes)
import Foreign.Storable (poke, peek, sizeOf)
import Data.Word (Word16)

-- | HASKELL TO PYTHON | -- 

-- | Built in numpy serialisation descriptions
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
  
-- | Converts shape list to a string of the Numpy tuple form e.g. (3,2,)  
getNumpyShape :: NdArray -> String
getNumpyShape (NdArray s _) = "(" <> (drop 1 $ take (length lshape -1) $ lshape) <> ",)"
  where lshape = show s

-- | Gets the maximum memory required for any single element in an array
getElemSize :: NdArray -> Int
getElemSize (NdArray _ v) = V.maximum $ V.map sizeOf v

-- | Saves any of the standard types defined above as a .npy
-- Thanks Chris! https://github.com/cchalmers/dense/blob/6eced9f5a3ab6b5026fe4f7ab4f67a8bce4d6262/src/Data/Dense/Storable.hs#L686
-- See https://numpy.org/doc/stable/reference/generated/numpy.lib.format.html
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
  V.unsafeWith v (\ptr -> hPutBuf h ptr (vectorSize * elemSize))


-- | PYTHON TO HASKELL | -- 

-- METADATA
listDict :: String -> [String]
listDict x = splitOn " " $ (splitOneOf "{}" (filter (/='\'') x)) !! 1

pairDict :: [String] -> [(String, String)]
pairDict [] = []
pairDict (_:[]) = []
pairDict (k:v:ls) = (k, v) : pairDict ls

--pyToTypeRep "<i8" = typeRep @Int
--pyToTypeRep "<f4" = typeRep @Float 
--pyToTypeRep :: DType a => String -> TypeRep a

{-
pyToTypeRep :: forall a . String -> (DType a => TypeRep a)
pyToTypeRep dtype = case dtype of 
  "<i4" -> typeRep @Int :: TypeRep a
  --"<i8" ->
  "<f4" -> typeRep @Float
  --"<f8" ->
  --"<?"  ->
  --"<U1" -> 
  _     -> error "Unsupported dtype."
-}

-- PAYLOAD
-- Read in an element from the handle
buffElement :: forall a . DType a => Handle -> IO a
buffElement h = do
  let elemSize = sizeOf (undefined :: a)
  ptr <- mallocBytes elemSize
  _ <- hGetBuf h ptr elemSize
  val <- peek ptr
  pure val

-- Read in the complete array as a list from the handle
buffArray :: forall a . DType a => TypeRep a -> Handle -> Integer -> [IO a]
buffArray _ _ 0 = []
buffArray t h i = do
  let buffed = (buffElement h) : buffArray t h (i-1)
  case eqTypeRep (typeOf buffed) (typeRep @[IO a]) of
    Just HRefl -> buffed
    _ -> error "Given TypeRep does not match data type."

loadPayload :: forall a . DType a => Handle -> [Integer] -> TypeRep a -> IO NdArray
loadPayload h sh t = do
  l <- traverse id $ buffArray (typeRep @a) h (product sh)
  pure $ NdArray sh (V.fromList l)

-- todo check unicode UTF
reifyDType :: String -> (forall a . DType a => TypeRep a -> r) -> r
reifyDType dtype cont =
  case dtype of 
    -- "<i8" -> cont (typeRep @Int64)
    "<i8" -> cont (typeRep @Int)
    -- "<i4" -> cont (typeRep @Int32)
    "<f4" -> cont (typeRep @Float)
    -- "<f8" -> cont (typeRep @Double)
    -- "<U1" -> cont (typeRep @Char)
    "<?"  -> cont (typeRep @Bool)
    _     -> error "Unsupported dtype."


-- | Loads an NdArray from a .npy file
loadNpy :: FilePath -> IO NdArray
loadNpy path = withBinaryFile path ReadMode $ \h -> do
  -- Unpacks and parses the header to get the array type and size
  descr <- hGetLine h
  let
    -- Places the dtype description, fortran order and shape in a map 
    metadata = (M.fromList . pairDict . listDict) descr
    -- Extracts the dtype description e.g. <i8
    dtype = filter (/=',') (metadata M.! "descr:")
    -- Converts the shape to an Integer list
    shapeStrs = filter (/= "") $ splitOn "," $ filter (`notElem`"()") (metadata M.! "shape:")
    sh = map (read @Integer) shapeStrs
    -- Calculates the total number of elements in the array
    --sz = product sh
  -- Reads the array itself into a list
  reifyDType dtype (loadPayload h sh)

-- Try it! It will probably break easily

testsave :: IO ()
testsave = do saveNpy "./src/testout/test123.npy" (NdArray [3] (V.fromList [1,2,3 :: Float]) )

testload :: IO ()
testload = do
  nd <- loadNpy "./src/testout/test123.npy"
  putStrLn $ show $ nd

{-
-- Reads the array itself into a list
  l <- traverse id $ case dtype of 
    "<i8" -> buffInts h sz
    --"<i4" -> 
    --"<i8" ->
    "<f4" -> buffFloats h sz
    --"<f8" ->
    --"<?"  ->
    --"<U1" -> 
    _     -> error "Unsupported dtype."
  -- Converts the list & shape to an NdArray
  pure $ NdArray sh (V.fromList l)
-}