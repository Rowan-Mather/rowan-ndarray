{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Indexing where

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import qualified Data.Map as M
import Type.Reflection

import NdArray
import Typing
import qualified DType
import DType (DType)

data IndexRange = I Integer | R Integer Integer deriving (Show, Eq)

-- * Indexing & Slicing
{- | Arrays are stored as vectors with a shape. Since vectors only have one dimension,
we convert between the vector index, i, and multi-dimension index, [x,y,z,...], using the 
shape of the array, [sx,sy,sz,...], as follows: 
  
  i = x + y*sx + z*sx*sy + ...
  
  x = i/(1) % sx
  y = i/(sx) % sy
  z = i/(sx*sy) % sz 
  ...
-}

generateIndicies :: [Integer] -> [[Integer]]
generateIndicies = foldr (\x xs -> [ (i:t) | i <- [0..(x-1)], t <- xs]) [[]]
-- foldr (\x xs -> [ (i:t) | i <- [0..x], t <- xs]) [[]] [2,3,2]

mapIndicies :: [Integer] -> (M.Map Int [Integer], M.Map [Integer] Int)
mapIndicies sh = (M.fromList oneDkey, M.fromList twoDkey)
  where 
    twoDinds = generateIndicies sh
    oneDkey = zip [0..] twoDinds
    twoDkey = zip twoDinds [0..]

-- Unsafe: Indexes the vector with the multi-index using a mapping 
vecInd :: forall a . DType a => M.Map [Integer] Int -> Vector a -> [Integer] -> a
vecInd mapp v i = v V.! (mapp M.! i)
--vecInd mapp v i = case v =@= (undefined :: Vector a) of
--  Just HRefl -> v V.! (mapp M.! i)

-- | Converts a shape and multi-index to a 1D index.
collapseInd :: [Integer] -> [Integer] -> Integer
collapseInd sh indicies = collapseRun (reverse$sh) (reverse$indicies) 1

-- Helper for collapseInd
collapseRun :: [Integer] -> [Integer] -> Integer -> Integer
collapseRun _ [] _ = 0
collapseRun [] _ _ = 0
collapseRun (s:ss) (x:xs) runSize = x*runSize + collapseRun ss xs (s*runSize)

-- | Converts a shape and 1D index to a multi-index.
expandInd :: [Integer] -> Integer -> [Integer]
expandInd sh i = reverse $ expandRun (reverse$sh) i 1

-- Helper for expandInd
expandRun :: [Integer] -> Integer -> Integer -> [Integer]
expandRun [] _ _ = []
expandRun (s:ss) i runSize = x : expandRun ss i (s*runSize)
  where x = (i `div` runSize) `mod` s

-- | Converts the multi-index for one shape to another
map1DIndex :: [Integer] -> [Integer] -> Integer -> Integer
map1DIndex s r i = collapseInd r (expandInd s i)

-- | Checks an index does not exceed the shape
validIndex :: NdArray -> [Integer] -> Bool
validIndex (NdArray s _) i = (length i == length s) && (and $ zipWith lessAbs i s)
  where lessAbs x y = (0 <= x && x < y) || (0 < -x && -x <= y)

{- | Takes a multi-dimensional index and returns the value in the NdArray at that position.
Indicies can be negative, where -1 is the row in that dimension.
If an index exceeds the size of its dimension, a value will still be returned, the identity
value for the array e.g. 0. To avoid this use !?.
-} 
-- >>> m = fromListFlat [2,4,8 :: Int]
-- >>> m #! [1] :: Int
-- 4
-- >>> m #! [50] :: Int
-- 0
(#!) :: DType a => NdArray -> [Integer] -> a
(NdArray s v) #! i = case (NdArray s v) !? i of
  Just val -> val
  Nothing -> DType.addId :: DType a => a

{- | The safer version of #! which returns Nothing if an index exceeds the shape bounds. -}
-- >>> m = fromListFlat [2,4,8 :: Int]
-- >>> m !? [1] :: Maybe Int
-- Just 4
-- >>> m !? [50] :: Maybe Int
-- Nothing
(!?) :: forall a . DType a => NdArray -> [Integer] -> Maybe a
(NdArray s v) !? i =
  let
    -- Converts any negative indicies to their equivalent positives
    positives = zipWith positiveInd s i
    flatInd = fromIntegral $ collapseInd s positives :: Int
  in
    -- The type comparison should always hold
    if validIndex (NdArray s v) i then
      case ty v `eqTypeRep` typeRep @(Vector a) of
        Just HRefl -> Just (v V.! flatInd) :: Maybe a -- Indexing the vector
        Nothing -> Nothing
    else Nothing

(#!+) :: NdArray -> [IndexRange] -> NdArray
(#!+) (NdArray sh v) irs = sliceWithMap m 0 (map forceRange irs) (NdArray sh v)
  where (m,_) = mapIndicies sh

forceRange :: IndexRange -> (Integer, Integer)
forceRange (I i) = (i,i)
forceRange (R s t) = (s,t)

positiveInd :: (Ord a, Num a) => a -> a -> a
positiveInd s i = if i < 0 then s+i else i

--(!?+) :: NdArray -> [IndexRange] -> Maybe NdArray
--(!?+) = undefined

{- | Takes a series of ranges corresponding to each dimension in the array and returns
the sub-array. -}
slice :: [(Integer, Integer)] -> NdArray -> NdArray
slice ss (NdArray sh v) = sliceWithMap m 0 ss (NdArray sh v)
  where (m,_) = mapIndicies sh

(!/) :: NdArray -> [(Integer, Integer)] -> NdArray
(!/) nd ss = slice ss nd

-- helper
sliceWithMap :: M.Map Int [Integer] -> Int -> [(Integer, Integer)] -> NdArray -> NdArray
sliceWithMap _ _ [] nd = nd
sliceWithMap _ d _ (NdArray sh v) | d >= length sh = (NdArray sh v)
sliceWithMap m d (s : ss) (NdArray sh v) = sliceWithMap m (d+1) ss $ 
  sliceDim s d m (NdArray sh v)

-- inclusive, supports negatives
sliceDim :: (Integer, Integer) -> Int -> M.Map Int [Integer] -> NdArray -> NdArray
sliceDim (x,y) d m (NdArray sh v) = 
  if d >= length sh then error "Given dimension does not exist in array."
  else NdArray
    (if y' < x' then [] else shrinkNth d (y'-x'+1) sh)
    (V.ifilter
      (\i _ ->
        let dimInd = (m M.! i) !! d
        in x' <= dimInd && dimInd <= y') 
      v
    )
  where
    dimSize = sh !! d 
    (x', y') = (positiveInd dimSize x, positiveInd dimSize y)

-- Replaces the nth value of an array if the newValue is smaller.
-- https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
shrinkNth :: Ord a => Int -> a -> [a] -> [a]
shrinkNth _ _ [] = []
shrinkNth n newVal (x:xs)
  | n == 0 = if newVal < x then newVal:xs else x:xs
  | otherwise = x:shrinkNth (n-1) newVal xs
