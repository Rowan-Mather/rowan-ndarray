{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Indexing where


import Control.Exception
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import qualified Data.Map as M
import Type.Reflection

import NdArray
import Typing
import qualified DType
import DType (DType)
import NdArrayException

preciseDiv x y = fromIntegral @Int @Float x / fromIntegral @Int @Float y

-- Takes the shape assuming you want to move directly between dimensions.
defStride :: Vector Int -> Vector Int
defStride sh = V.scanr' (*) 1 $ V.drop 1 sh

stride :: NdArray -> NdArray
stride (NdArray sh st v) = 
  let
    -- shape
    dimAcc = V.scanr' (*) 1 sh
    --newshape = V.map (i -> ((dim' V.!(i+1)) * (sh V.!i -1) +1) / st V.! i) (enumFromN 0 (V.length sh))  
    newshape =
      V.map (\i ->
          ceiling $ preciseDiv (1 + (dimAcc V.! (i+1)) * (sh V.!i -1)) (st V.! i) :: Int)
        (V.enumFromN 0 (V.length sh))
    --V.zipWith (\d s -> ceiling (preciseDiv d s) :: Int) [0..] st
    -- stride
    singleStride = defStride newshape
    -- vector
    grab i = V.and $ V.zipWith (\t h -> i `mod` t < h) st (V.drop 1 dimAcc)
    newV = V.force (V.ifilter (\i _ -> grab i) v)
  in
    if V.all (==1) st then (NdArray sh st v)
    else NdArray newshape singleStride newV

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
generateIndices :: [Int] -> [[Int]]
generateIndices = foldr (\x xs -> [ i:t | i <- [0..(x-1)], t <- xs]) [[]]

{- | Generates two maps to convert between the single dimension index of the 
underlying vector and the multi-dimensional index of the NdArray and back, 
given the NdArray shape.
-}
mapIndices :: [Int] -> (M.Map Int [Int], M.Map [Int] Int)
mapIndices sh = (M.fromList oneDkey, M.fromList twoDkey)
  where 
    twoDinds = generateIndices sh
    oneDkey = zip [0..] twoDinds
    twoDkey = zip twoDinds [0..]

-- Indexes a vector with an NdArray multi-index using a mapping (unsafe).
--vecInd :: forall a . DType a => M.Map [Int] Int -> Vector a -> [Int] -> a-
--vecInd mapp v i = v V.! (mapp M.! i)

{-
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
-}
vGet :: DType a => Vector a -> Vector Int -> [Int] -> a
vGet v t is = v V.! (collapseInd t $ V.fromList is)

collapseInd :: Vector Int -> Vector Int -> Int
collapseInd st ind = V.sum $ V.zipWith (*) st ind

expandInd :: Vector Int -> Int -> Vector Int
expandInd st ind = 
  let st' = V.toList st 
  in V.fromList $ expandRun st' ind

expandRun :: [Int] -> Int -> [Int]
expandRun [] _ = []
expandRun (s:sts) x =
  if s == 0 then (0 : expandRun sts x)
  else x `div` s : expandRun sts (x `mod` s)
  {-
  let st' = V.map (/= 0) st
  V.zipwith (/) (V.drop 1 $ V.scanl mod ind st) st

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f z xs = foldr go (const []) xs z
  where
    go x continue acc = let next = f acc x in next : continue next
-}

-- | Converts the multi-index for one shape to another
map1DIndex :: Vector Int -> Vector Int -> Int -> Int
map1DIndex t d i = collapseInd d (expandInd t i)

-- | Checks an index does not exceed the shape.
validIndex :: NdArray -> [Int] -> Bool
validIndex (NdArray _ sh _) i = (length i == length s) && and (zipWith lessAbs i s)
  where
    s = V.toList sh 
    lessAbs x y = (0 <= x && x < y) || (0 < -x && -x <= y)

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
(#!) :: DType a => NdArray -> [Int] -> a
(NdArray st sh v) #! i = case NdArray sh sh v !? i of
  Just val -> val
  Nothing -> DType.addId :: DType a => a

{- | The safer version of #! which returns Nothing if an index exceeds the shape bounds. -}
-- >>> m = fromListFlat [2,4,8 :: Int]
-- >>> m !? [1] :: Maybe Int
-- Just 4
-- >>> m !? [50] :: Maybe Int
-- Nothing
(!?) :: forall a . DType a => NdArray -> [Int] -> Maybe a
(NdArray st sh v) !? i =
  let
    -- Converts any negative indices to their equivalent positives
    positives = V.zipWith positiveInd sh (V.fromList i)
    flatInd = fromIntegral $ collapseInd st positives :: Int
  in
    -- The type comparison should always hold
    if validIndex (NdArray st sh v) i then
      case ty v `eqTypeRep` typeRep @(Vector a) of
        Just HRefl -> Just (v V.! flatInd) :: Maybe a -- Indexing the vector
        Nothing -> Nothing
    else Nothing

-- * SLICING
{-
-- | Type which allows you to provide only a single index or a range of indices.
data IndexRange = I Integer | R Integer Integer deriving (Show, Eq)

-- | Integrated indexing and slicing. For each dimension you can provide either a single value
-- or a range of values where a slice will be taken.
(#!+) :: NdArray -> [IndexRange] -> NdArray
(#!+) (NdArray sh v) irs = sliceWithMap m 0 (map forceRange irs) (NdArray sh v)
  where (m,_) = mapIndices sh

-- Converts an IndexRange to a range of indices in the standard pair form.
forceRange :: IndexRange -> (Integer, Integer)
forceRange (I i) = (i,i)
forceRange (R s t) = (s,t)
-}
-- Converts negative indices to their positive equivalents, counting back
-- from the end of the array (i.e. -1 is the last element).
positiveInd :: (Ord a, Num a) => a -> a -> a
positiveInd s i = if i < 0 then s+i else i

{- | Takes a series of ranges corresponding to each dimension in the array and returns
the sub-array. Indices are inclusive and can be negative. -}
slice :: [(Int, Int)] -> NdArray -> NdArray
slice sl (NdArray sh st v) =
  let -- todo: -ve indices
    ranges = zipWith (\(i,j) t -> [t*i, t*(i+1) .. t*j]) sl (V.toList st)
    indices = V.fromList $ (map sum . sequence) ranges
    sh' = V.fromList $ map (\(i,j) -> j-i+1) sl
    v' = V.map (v V.!) indices
  in 
     NdArray sh' (defStride sh') v'
--slice ss (NdArray sh v) = sliceWithMap m 0 ss (NdArray sh v)
--  where (m,_) = mapIndices sh
-- https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Haskell

{-
-- | Equivalent slicing operator.
(!/) :: NdArray -> [(Integer, Integer)] -> NdArray
(!/) nd ss = slice ss nd

-- Takes a slice on an NdArray given the mapping from the vector index to NdArray index.
-- Iterates through each dimension of the slice one at a time. 
sliceWithMap :: M.Map Int [Integer] -> Int -> [(Integer, Integer)] -> NdArray -> NdArray
sliceWithMap _ _ [] nd = nd
sliceWithMap _ d _ (NdArray sh v) | d >= length sh = NdArray sh v
sliceWithMap m d (s : ss) (NdArray sh v) = sliceWithMap m (d+1) ss $ 
  sliceDim s d m (NdArray sh v)

-- Takes a slice of an NdArray at a particular dimension.
sliceDim :: (Integer, Integer) -> Int -> M.Map Int [Integer] -> NdArray -> NdArray
sliceDim (x,y) d m (NdArray sh v) = 
  if d >= length sh then throw (ExceededShape (fromIntegral d) sh)
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
  -}