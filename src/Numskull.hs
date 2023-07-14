{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Numskull where

import DType

import Prelude as P
import Data.Vector.Storable as V
import Data.Dynamic -- Not needed?
import Type.Reflection

-- | Typing shorthand | -- 
ty :: Typeable a => a -> TypeRep a
ty x = typeOf x

(=@=) :: (Typeable a, Typeable b) => a -> b -> Maybe (a :~~: b)
(=@=) v u = eqTypeRep (ty v) (ty u)

-- Todo: Should shapes be [Integer] or [Int] or maybe even another vector?
-- NdArray --
-- The core of this module. NdArrays can be of any type (a) and size/shape (list of dimensions) but these are
-- hidden by the type. Both attributes can be inferred using the library constructors (TODO!).
data NdArray where
  NdArray :: DType a => Vector a -> [Integer] -> NdArray

-- Todo: show in a nicer shapely form :)
instance Show NdArray where
  show (NdArray v s) = show v <> show s

instance Eq NdArray where
  (NdArray v s) == (NdArray u r) = (r == s) && 
    case v =@= u of
      Just HRefl -> v == u
      Nothing    -> False
  (NdArray v s) /= (NdArray u r) = (r /= s) || 
    case v =@= u of
      Just HRefl -> v /= u
      Nothing    -> True

-- Investigate how Vector implements further
-- Todo: check max and min work properly on all dtypes, probably use a map instead
instance Ord NdArray where
  (NdArray v s) `compare` (NdArray u r) = if s == r then case v =@= u of
      Just HRefl -> compare v u
      Nothing    -> error $ typeMismatch (show v) (show u)
    else error $ shapeMismatch (show s) (show r)
  
  (NdArray v s) <= (NdArray u r) = if s == r then case v =@= u of
      Just HRefl -> v <= u
      Nothing    -> error $ typeMismatch (show v) (show u)
    else error $ shapeMismatch (show s) (show r)
  --  (>)     (NdArray v s) (NdArray u r) = u > v
  --  (>=)    (NdArray v s) (NdArray u r) = u >= v
  --  max     (NdArray v s)               = V.maximum v
  --  min     (NdArray v s)               = V.minimum v

-- To do: matrix multiplication :O
-- To do: change fromInteger to return an integer array rather than int
instance Num NdArray where
  (+) = pointwiseZip add
  (-) = pointwiseZip DType.subtract
  (*) = undefined -- Matrix multiplication
  negate (NdArray v s) = NdArray (V.map DType.invert v) s
  abs (NdArray v s) = NdArray (V.map DType.abs v) s
  signum (NdArray v s) = NdArray (V.map DType.signum v) s
  fromInteger x = NdArray (fromList [(fromInteger x) :: Int]) [1]

  
-- | Indexing & Slicing | -- 
-- Since vectors are 1D arrays but the matricicies can have n-dimensions, index conversion is neccessary
-- The index i will be the 1D index
-- Then x y z... for each dimension index
-- i = x + y*xsize + z*xsize*ysize + ...
-- x = i % xsize;   y = i/(xsize) % ysize;   z = i/(xsize*ysize) % zsize;   ...
-- As described: https://softwareengineering.stackexchange.com/questions/212808/treating-a-1d-data-structure-as-2d-grid

-- helper for collapseInd
-- can this be folded? its over two things so i dont think so... only if i zip it
collapseRun :: [Integer] -> [Integer] -> Integer -> Integer
collapseRun _ [] _ = 0
collapseRun [] _ _ = 0
collapseRun (s:ss) (x:xs) runSize = x*runSize + collapseRun ss xs (s*runSize)

collapseInd :: [Integer] -> [Integer] -> Integer
collapseInd shape indicies = collapseRun shape indicies 1

-- helper for expandInd
expandRun :: [Integer] -> Integer -> Integer -> [Integer]
expandRun [] _ _ = []
expandRun (s:ss) i runSize = x : expandRun ss i (s*runSize)
  where x = (i `P.div` runSize) `P.mod` s

expandInd :: [Integer] -> Integer -> [Integer]
expandInd shape i = expandRun shape i 1

-- The actual indexing bit todo
-- Slicing Todo :)

-- | Pointwise Functions | -- 
-- All the numpy-like functions not defined within the Eq, Ord or Num instances
-- Single Argument

-- To do ;)

-- Two Arguments
-- https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Monad-ST.html#v:runST
pointwiseZip :: (forall t . DType t => t -> t -> t) -> NdArray -> NdArray -> NdArray
pointwiseZip zipfunc (NdArray v s) (NdArray u r) = if s == r then 
  case v =@= u of
    Just HRefl -> NdArray (V.zipWith zipfunc v u) s -- Types match
    Nothing    -> error $ typeMismatch (show$ty v) (show$ty u)
  else error $ shapeMismatch (show s) (show r)

elemMultiply :: NdArray -> NdArray -> NdArray
elemMultiply = pointwiseZip multiply

-- Todo: Needs to operate on doubles
--elemDivide :: NdArray -> NdArray -> NdArray
--elemDivide = pointwiseZip divide

elemDiv :: NdArray -> NdArray -> NdArray
elemDiv = pointwiseZip DType.div

-- Todo: Needs to operate on doubles
--elemPower :: NdArray -> NdArray -> NdArray
--elemPower = pointwiseZip power

elemPow :: NdArray -> NdArray -> NdArray
elemPow = pointwiseZip pow
  
-- | Type & Shape Conversion | --
-- Converting between the standard dtypes and changing the shapes of matricies

-- To do: add many more possible types you can convert to
-- Use the TypeApplications syntax: 
-- case typeOf x `eqTypeRep` typeRep @Integer of 
matchDType :: NdArray -> NdArray -> Maybe NdArray
matchDType (NdArray v _) (NdArray u r) = case v =@= fromList [1::Int] of
  Just HRefl  -> Just $ NdArray (V.map dtypeToInt u) r
  _           -> Nothing

-- Check that the matrix isn't larger than the shape but if so truncate it
constrainSize :: DType a => Vector a -> [Integer] -> (Bool, Vector a)
constrainSize v s =
  if size < len then (False, V.take size v)
  else (True, v)
  where 
    size = fromInteger (P.product s) :: Int
    len = V.length v

-- Fill out any spaces in a vector smaller than the shape with 0s (or whatever the dtype 'identity' is)
padSize :: DType a => Vector a -> [Integer] -> Vector a
padSize v s = v V.++ V.replicate (size - len) identity
  where 
    size = fromInteger (P.product s) :: Int
    len = V.length v

-- Contrain or pad the vector to match the size
setSize :: DType a => Vector a -> [Integer] -> Vector a
setSize v s = let (unchanged, u) = constrainSize v s in
  if unchanged then padSize u s else u

-- Constrain or pad the NdArray to match the new given size
resize :: NdArray -> [Integer] -> NdArray
resize (NdArray v _) r = NdArray (setSize v r) r

--NB: reshape will pad/truncate individual dimensions whereas resize keeps as many values as possible but they might switch position
-- a matrix being reshaped must already match the size correctly

map1DIndex s r i = collapseInd r (expandInd s i)

-- ok then what im gonna do is make an array of all the mapping and value pairs and // it

padShape :: NdArray -> [Integer] -> NdArray
padDimension (NdArray v s) r = undefined
  
-- | Common Errors | -- 
shapeMismatch :: String -> String -> String
shapeMismatch s1 s2 = "Cannot match first array of shape '" <> s1 <> "' with array of shape '" <> s2 <> "'."

typeMismatch :: String -> String -> String
typeMismatch t1 t2 = "Cannot match first array of type '" <> t1 <> "' with array of type '" <> t2 <> "'."





---- Testing

-- Helper trying to simplify the type checking....

{- I think this is a dead end.... just do the case by case
eqDType :: (Typeable a, Typeable b) => a -> b -> Bool
eqDType x y = case eqTypeRep (typeOf x) (typeOf y) of 
  Just HRefl  -> True
  _           -> False

equey :: NdArray -> NdArray -> Vector Bool
equey (NdArray v1 s1) (NdArray v2 s2) =
    if eqDType (NdArray v1 s1) (NdArray v2 s2)
    then V.zipWith (==) v1 v2 
    else (fromList [True]) :: Vector Bool
-}

{- Pointwise zip with type conversion (TODO)
pointwiseZip (NdArray v s) (NdArray u r) = case (eqTypeRep xtype ytype, matchDType (NdArray v s) (NdArray u r)) of
        (Just HRefl, _)     -> NdArray (V.zipWith add x y) -- Types match
        -- Code to auto-cast types
        --(_, Just casted)    -> (NdArray v s) + casted -- Second type can be converted to first
        _                   -> error ("Cannot match second matrix of type '" P.++ show ytype P.++ "' to type '" P.++ show xtype P.++ "'.")
        where
            xtype = ty x; ytype = ty y
-}

{-
unwrapND :: NdArray -> (String, Vector Dynamic)
unwrapND (NdArray v s) = case typeOf x of
    vecTypeInt      -> ("Int", V.map toDyn x)
    vecTypeBool     -> ("Bool", V.map toDyn x)
-}

nd1 :: NdArray
nd2 :: NdArray
nd1 = NdArray (fromList [1,2,3::Int]) [3]
nd2 = NdArray (fromList [10,11,12::Int]) [3]
