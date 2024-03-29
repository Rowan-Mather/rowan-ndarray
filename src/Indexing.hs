{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Indexing where

import Control.Exception
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import qualified Data.Map as M
import Type.Reflection
import Text.ParserCombinators.Parsec

import NdArray
import Typing
import qualified DType
import DType (DType)
import NdArrayException
import QuasiSlice
import QuasiSlice.Quote


{- | Arrays are stored as vectors with a shape. Since vectors only have one dimension,
we convert between the vector index, i, and multi-dimension index, [x,y,z,...], using the 
shape of the array, [sx,sy,sz,...], as follows: 
  
  i = x + y*sx + z*sx*sy + ...
  
  x = i/(1) % sx
  y = i/(sx) % sy
  z = i/(sx*sy) % sz 
  ...
-}

-- * INDEXING

-- | Generates the list of all multi-dimensional indices for a given shape
generateIndices :: [Integer] -> [[Integer]]
generateIndices = foldr (\x xs -> [ i:t | i <- [0..(x-1)], t <- xs]) [[]]

{- | Generates two maps to convert between the single dimension index of the 
underlying vector and the multi-dimensional index of the NdArray and back, 
given the NdArray shape.
-}
mapIndices :: [Integer] -> (M.Map Int [Integer], M.Map [Integer] Int)
mapIndices sh = (M.fromList oneDkey, M.fromList twoDkey)
  where 
    twoDinds = generateIndices sh
    oneDkey = zip [0..] twoDinds
    twoDkey = zip twoDinds [0..]

-- Indexes a vector with an NdArray multi-index using a mapping (unsafe).
vecInd :: forall a . DType a => M.Map [Integer] Int -> Vector a -> [Integer] -> a
vecInd mapp v i = v V.! (mapp M.! i)

-- | Converts a shape and multi-index to a 1D index.
collapseInd :: [Integer] -> [Integer] -> Integer
collapseInd sh indices = collapseRun (reverse sh) (reverse indices) 1

-- Helper for collapseInd
collapseRun :: [Integer] -> [Integer] -> Integer -> Integer
collapseRun _ [] _ = 0
collapseRun [] _ _ = 0
collapseRun (s:ss) (x:xs) runSize = x*runSize + collapseRun ss xs (s*runSize)

-- | Converts a shape and 1D index to a multi-index.
expandInd :: [Integer] -> Integer -> [Integer]
expandInd sh i = reverse $ expandRun (reverse sh) i 1

-- Helper for expandInd
expandRun :: [Integer] -> Integer -> Integer -> [Integer]
expandRun [] _ _ = []
expandRun (s:ss) i runSize = x : expandRun ss i (s*runSize)
  where x = (i `div` runSize) `mod` s

-- | Converts the multi-index for one shape to another
map1DIndex :: [Integer] -> [Integer] -> Integer -> Integer
map1DIndex s r i = collapseInd r (expandInd s i)

-- | Checks an index does not exceed the shape.
validIndex :: NdArray -> [Integer] -> Bool
validIndex (NdArray s _) i = (length i == length s) && and (zipWith lessAbs i s)
  where lessAbs x y = (0 <= x && x < y) || (0 < -x && -x <= y)

{- | Takes a multi-dimensional index and returns the value in the NdArray at that position.
Indices can be negative, where -1 is the row in that dimension.
If an index exceeds the size of its dimension, a value will still be returned, the identity
value for the array e.g. 0. To avoid this use !?.
-} 
-- >>> m = fromListFlat [2,4,8 :: Int]
-- >>> m #! [1] :: Int
-- 4
-- >>> m #! [50] :: Int
-- 0
(#!) :: DType a => NdArray -> [Integer] -> a
(NdArray s v) #! i = case NdArray s v #? i of
  Just val -> val
  Nothing -> DType.addId :: DType a => a

{- | The safer version of #! which returns Nothing if an index exceeds the shape bounds. -}
-- >>> m = fromListFlat [2,4,8 :: Int]
-- >>> m !? [1] :: Maybe Int
-- Just 4
-- >>> m !? [50] :: Maybe Int
-- Nothing
(#?) :: forall a . DType a => NdArray -> [Integer] -> Maybe a
(NdArray s v) #? i =
  let
    -- Converts any negative indices to their equivalent positives
    positives = zipWith positiveInd s i
    flatInd = fromIntegral $ collapseInd s positives :: Int
  in
    -- The type comparison should always hold
    if validIndex (NdArray s v) i then
      case ty v `eqTypeRep` typeRep @(Vector a) of
        Just HRefl -> Just (v V.! flatInd) :: Maybe a -- Indexing the vector
        Nothing -> Nothing
    else Nothing

-- Converts negative indices to their positive equivalents, counting back
-- from the end of the array (i.e. -1 is the last element).
positiveInd :: (Ord a, Num a) => a -> a -> a
positiveInd s i = if i < 0 then s+i else i

-- * SLICING

positiveRanges :: [Integer] -> [(Integer, Integer)] -> [(Integer, Integer)]
positiveRanges = zipWith (\s (x,y) -> (positiveInd s x, if y < 0 then s+y else y-1))

-- Converts an IndexRange to a range of indices in the standard pair form.
forceRange :: Integer -> IndexRange -> (Integer, Integer)
forceRange sh (I i) = (positiveInd sh i, positiveInd sh i)
forceRange sh (R s t) = (positiveInd sh s, if t < 0 then positiveInd sh t else t-1)

-- | The concise operator for slicing. Instead of providing an IndexRange, 
-- You may QuasiQuote a NumPy-like index e.g. myArray /! [q|5,2:6,:3|].
-- Unspecified values in ranges denote the start/end.
(/!) :: NdArray -> QuasiSlice -> NdArray
(/!) nd sl = nd #!+ (evalSlice sl)

-- Integrated indexing and slicing. For each dimension you can provide either a single value
-- or a range of values where a slice will be taken.
(#!+) :: NdArray -> [IndexRange] -> NdArray
(#!+) (NdArray s v) irs = slice (zipWith forceRange s irs) (NdArray s v)

{- | Takes a series of ranges corresponding to each dimension in the array and returns
the sub-array. Indices are inclusive and can be negative. -}
slice :: [(Integer, Integer)] -> NdArray -> NdArray
slice sl (NdArray s v) =
  let
    pad = sl ++ replicate (length s - length sl) (0,-1)
    sl' = zipWith (\(x,y) sk -> (positiveInd sk x, positiveInd sk y)) pad s 
    inds = sequence $ map (\(x, y) -> [x..y]) sl'
    flatinds = V.fromList $ map (fromInteger @Int . collapseInd s) inds
    newshape = map (\(x,y) -> y-x+1) sl'
  in NdArray newshape $ V.map (v V.!) flatinds

{-
slicing with maps
--slice :: [(Integer, Integer)] -> NdArray -> NdArray
--slice ss (NdArray sh v) = sliceWithMap m 0 ss (NdArray sh v)
--  where (m,_) = mapIndices sh

--(#!+) (NdArray sh v) irs = sliceWithMap m 0 (map forceRange irs) (NdArray sh v)
--  where (m,_) = mapIndices sh

-- Equivalent slicing operator.
--(!/) :: NdArray -> [(Integer, Integer)] -> NdArray
--(!/) nd ss = slice ss nd

-- Takes a slice on an NdArray given the mapping from the vector index to NdArray index.
-- Iterates through each dimension of the slice one at a time. 
sliceWithMap :: M.Map Int [Integer] -> Int -> [(Integer, Integer)] -> NdArray -> NdArray
sliceWithMap _ _ [] nd = nd
sliceWithMap _ d _ (NdArray sh v) | d >= length sh = NdArray sh v
sliceWithMap m d (s : ss) (NdArray sh v) = sliceWithMap m (d+1) ss $ 
  sliceDim s d m (NdArray sh v)

-- Takes a slice of an NdArray at a particular dimension, no -ve indices.
sliceDim :: (Integer, Integer) -> Int -> M.Map Int [Integer] -> NdArray -> NdArray
sliceDim (x,y) d m (NdArray sh v) = 
  if d >= length sh then throw (ExceededShape (fromIntegral d) sh)
  else NdArray
    (if y < x then [] else shrinkNth d (y-x+1) sh)
    (V.ifilter
      (\i _ ->
        let dimInd = (m M.! i) !! d
        in x <= dimInd && dimInd <= y) 
      v
    )
  where
    dimSize = sh !! d 

-- Replaces the nth value of an array if the newValue is smaller.
-- https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
shrinkNth :: Ord a => Int -> a -> [a] -> [a]
shrinkNth _ _ [] = []
shrinkNth n newVal (x:xs)
  | n == 0 = if newVal < x then newVal:xs else x:xs
  | otherwise = x:shrinkNth (n-1) newVal xs
-}