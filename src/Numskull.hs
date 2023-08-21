{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Numskull (
  -- * Metadata
  DType
  , size
  , shape
  , getVector
  , ndType
  , checkNdType
  , isEmpty

  -- * Creation
  , NdArray
  , fromList
  , fromListFlat
  , TreeMatrix (..)
  , fromMatrix
  , fromVector
  , singleton
  , arange
  , zeros
  , squareArr

  -- * Modification
  , update

  -- * General Mapping, Folding & Zipping
  , foldrA
  , mapA
  , mapTransform
  , pointwiseZip
  , pointwiseBool
  , zipArrayWith

  -- * Summaries
  , origin
  , maxElem
  , minElem

  -- * Mathematical constant
  , scale
  , absA
  , signumA
  , ceilA
  , floorA
  , sinA
  , cosA
  , tanA
  , invertA
  , shiftleftA
  , shiftrightA

  -- * Mathematical pointwise
  , elemDivide
  , elemDiv
  , elemPow
  , elemPower
  , Numskull.sum
  , mean

  -- * Bounds
  , clip

  -- * Type Conversions
  , convertDTypeTo
  , matchDType

  -- * Size Conversions
  , resize

  -- * Shape Conversions/Manipulations
  , reshape
  , padShape
  , constrainShape
  , broadcast
  , concatAlong
  , gather

  -- * Matrix Manipulation
  , swapRows
  , diagonal
  , transpose
  , transposePerm

  -- * Matrix Multiplication
  , dot
  , matMul
  , upperTriangle
  , determinant
  , determinant2D
  , swapRowsWith0Pivot
  , gemm

  -- * Indexing
  , IndexRange
  , collapseInd
  , expandInd
  , map1DIndex
  , validIndex
  , (#!)
  , (#?)
  , (#!+)
  , slice
  , (/!)
  , evalSlice
  , q


  -- * Pretty Printing
  , printArray
  , prettyShowArray

  -- Typing
  , (=@=)

  -- Numpy Serialisation
  , saveNpy
  , loadNpy
) where

import qualified DType
import           DType                (DType)
import           Indexing
import           MatrixForm
import           NdArray
import           NdArrayException
import           Serialisation
import           Typing
import           QuasiSlice
import           QuasiSlice.Quote

import           Control.Exception
import           Control.Monad        (zipWithM)
import           Data.List            (elemIndex, intersect, sort, zipWith4)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import           Type.Reflection

-- $setup
-- >>> import Numskull as N
-- >>> import qualified Vector

-- * Numeric & Comparative NdArray instances:
--------------------------------------------------------------------------------

instance Eq NdArray where
  -- | Arrays are equal if their elements and shape exactly match.
  (NdArray s v) == (NdArray r u) = (r == s) &&
    case v =@= u of
      Just HRefl -> v == u
      Nothing    -> False
  (NdArray s v) /= (NdArray r u) = (r /= s) ||
    case v =@= u of
      Just HRefl -> v /= u
      Nothing    -> True

instance Ord NdArray where
  {- | Arrays are only comparable when they are the same shape. Then they are
  ordered by pointwise comparison.
  -}
  (NdArray s v) `compare` (NdArray r u) = if s == r then case v =@= u of
      Just HRefl -> compare v u
      Nothing    -> throw (DTypeMismatch (NdArray s v) (NdArray r u) "compare")
    else throw (ShapeMismatch (NdArray s v) (NdArray r u) "compare")

  (NdArray s v) <= (NdArray r u) = if s == r then case v =@= u of
      Just HRefl -> v <= u
      Nothing    -> throw (DTypeMismatch (NdArray s v) (NdArray r u) "'<='")
    else throw (ShapeMismatch (NdArray s v) (NdArray r u) "'<='")

instance Num NdArray where
  -- | Adds elements pointwise
  (+) = pointwiseZip DType.add
  -- | Subtracts elements pointwise
  (-) = pointwiseZip DType.subtract
  -- | Multiplies elements pointwise
  (*) = pointwiseZip DType.multiply
  -- | Inverts all elements according to their DType instance
  negate (NdArray s v) = NdArray s (V.map DType.invert v)
  -- | Absolute value of each element
  abs (NdArray s v) = NdArray s (V.map DType.abs v)
  -- | Signum of each element
  signum (NdArray s v) = NdArray s (V.map DType.signum v)
  -- Creates a singleton array. NB: must be converted to a storable Int.
  fromInteger = singleton . fromInteger @Int

-- * General & Creation
--------------------------------------------------------------------------------

-- | Gets the total number of elements in a given array shape.
-- >>> size [2,3]
-- 6
size :: [Integer] -> Int
size sh = (fromIntegral $ product sh) :: Int

-- | Returns the shape list of an array.
shape :: NdArray -> [Integer]
shape (NdArray s _) = s

-- | Gets the vector of an array. Requires a type specification to output safely.
getVector :: forall a . DType a => NdArray -> Vector a
getVector (NdArray _ v) = v <-@ typeRep @(Vector a)

-- | Gets the TypeRep String representation of the NdArray elements
ndType :: NdArray -> String
ndType (NdArray _ v) = show $ vecType v

-- | Compares the type of the array elements to the given TypeRep.
checkNdType :: forall a b . (DType a, DType b) => NdArray -> TypeRep a -> Maybe (a :~~: b)
checkNdType (NdArray _ v) _ =
  let tv = vecType v
  in case eqTypeRep tv (typeRep @b) of
    Just HRefl -> eqTypeRep (typeRep @a) (tv :: TypeRep b)
    _ -> error "Impossibly mismatching types."

-- | Helper to get the vector typeRep.
vecType :: forall a . DType a => Vector a -> TypeRep a
vecType _ = typeRep @a

-- | Checks if the undelying vector has any elements.
isEmpty :: NdArray -> Bool
isEmpty (NdArray _ v) = V.null v

-- | Convert a list of arrays to a list of vectors, provided they are all of the specified type.
extractVectors :: forall a . DType a => [NdArray] -> TypeRep a -> Maybe [Vector a]
extractVectors [] _ = Just []
extractVectors ((NdArray _ v) : nds) t =
  case v =@= (undefined :: Vector a) of
    Just HRefl ->
      case extractVectors nds t of
        Just vs -> Just (v:vs)
        _ -> Nothing
    Nothing -> Nothing

-- Gets the DType additive identity matching the element type of a vector.
identityElem :: forall a . DType a => Vector a -> a
identityElem _ = DType.addId :: DType a => a

-- | Creates an NdArray from a given shape and list. The number of elements must match.
-- >>> printArray $ fromList [2,2] [1,2,3,4::Int]
-- 1 2
-- 3 4
fromList :: DType a => [Integer] -> [a] -> NdArray
fromList sh l =
  if length l /= size sh then throw $ CreationSize (fromIntegral $ length l) sh
  else NdArray sh (V.fromList l)

-- | Creates a 1xn NdArray from a list.
-- >>> printArray $ fromListFlat [1,2,3,4::Int]
-- 1 2 3 4
fromListFlat :: DType a => [a] -> NdArray
fromListFlat l = NdArray [toInteger$length l] (V.fromList l)

{-| Creates an NdArray from an explicitly given matrix such as the example 2x3. -}
-- >>> m :: TreeMatrix Int
-- >>> m = A [A [B 1,  B 2],
-- >>>        A [B 3,  B 4],
-- >>>        A [B 5,  B 6]]
-- >>> printArray $ fromMatrix m
-- 1 2
-- 3 4
-- 5 6
fromMatrix :: DType a => TreeMatrix a -> NdArray
fromMatrix m = NdArray (matrixShape m) (V.fromList l)
  where l = flattenToList $ matrixToTree m

-- | The safe standard constructor. Returns Nothing if the
-- shape does not match the given vector length.
fromVector :: DType a => [Integer] -> Vector a -> Maybe NdArray
fromVector sh v = if V.length v == fromIntegral (product sh)
  then Just $ NdArray sh v
  else Nothing

-- | Creates a 1x1 matrix
-- >>> printArray $ singleton (3::Int)
-- 3
singleton :: DType a => a -> NdArray
singleton x = NdArray [1] (V.fromList [x])

-- | Creates a flat array over the specified range.
arange :: (Enum a, DType a) => a -> a -> NdArray
arange mini maxi =
  if mini <= maxi
    then NdArray [fromIntegral $ fromEnum maxi - fromEnum mini + 1] $ V.fromList [mini..maxi]
    else NdArray [] (V.fromList [] :: Vector Int)

{- | Creates the smallest possible square matrix from the given list,
padding out any required space with the identity element for the DType -}
squareArr :: forall a . DType a => [a] -> NdArray
squareArr [] = NdArray [] (V.fromList [] :: Vector Int)
squareArr xs =
  let
    l = length xs
    d = ceiling (sqrt $ fromIntegral @Int @Float l)
    d' = fromIntegral @Int @Integer d
    p = V.replicate (d^(2::Int) - l) (DType.addId :: a)
  in NdArray [d', d'] (V.fromList xs V.++ p)

{- | Creates an array of the given shape of the identity element for the given type. -}
zeros :: forall a . DType a => TypeRep a -> [Integer] -> NdArray
zeros _ s = NdArray s zerovec
  where
    ident = DType.addId :: (DType a => a)
    zerovec = V.replicate (size s) ident :: DType a => Vector a

update :: forall a . DType a => NdArray -> [Integer] -> a -> NdArray
update (NdArray s v) ind val = 
  NdArray s $ V.force (v V.// [(ind', val')])
  where
    ind' = fromIntegral $ collapseInd s ind :: Int
    val' = matchVecType v val

matchVecType :: forall a b . (DType a, DType b) => Vector a -> b -> a
matchVecType _ x = DType.rationalToDtype (DType.dtypeToRational x) :: a

-- * Pointwise Functions
--------------------------------------------------------------------------------

-- * One Argument

{- | Near identical to a standard foldr instance, expect NdArrays do not have an explicit type.
Folds in row-major order.
-}
foldrA :: forall a b . DType a => (a -> b -> b) -> b -> NdArray -> b
foldrA f z (NdArray _ v) =
  case v =@= (undefined :: Vector a) of
    Just HRefl -> V.foldr f z v
    _ -> throw $ TypeMismatch "Fold starting value type does not match array type."

-- | Near identical to a standard map implementation in row-major order.
mapA :: forall a . forall b . (DType a, DType b) => (a -> b) -> NdArray -> NdArray
mapA f (NdArray s v) = case v =@= (undefined :: Vector a) of
  Just HRefl -> NdArray s (V.map f v)
  _ -> throw $ TypeMismatch "Map function input does not match array type."

-- | Maps functions which return the same type.
mapTransform :: (forall a . DType a => a -> a) -> NdArray -> NdArray
mapTransform f (NdArray s v) = NdArray s (V.map f v)

-- | Multiplies all elements by a scalar.
scale :: forall a . DType a => a -> NdArray -> NdArray
scale x = mapA (DType.multiply x)

-- | Takes the absolute value of all elements.
absA :: NdArray -> NdArray
absA = mapTransform DType.abs

-- | Replaces all elements by their signum.
-- >>> printArray $ signumA (fromList [5] [-50, -25, 0, 1, 10::Int])
-- -1 -1  0  1  1
signumA :: NdArray -> NdArray
signumA = mapTransform DType.signum

-- | Mathematical ceiling of each element (preserving DType).
ceilA :: NdArray -> NdArray
ceilA = mapTransform DType.ceil

-- | Mathematical floor of each element (preserving DType).
floorA :: NdArray -> NdArray
floorA = mapTransform DType.floor

-- | Sine of each element (preserving DType).
sinA :: NdArray -> NdArray
sinA = mapTransform DType.sin

-- | Cosine of each element (preserving DType).
cosA :: NdArray -> NdArray
cosA = mapTransform DType.cos

-- | Tangent of each element (preserving DType).
tanA :: NdArray -> NdArray
tanA = mapTransform DType.tan

-- | Either elementwise NOT or NEG depending on the DType.
invertA :: NdArray -> NdArray
invertA = mapTransform DType.invert

-- | Multiply each element by 2.
shiftleftA :: NdArray -> NdArray
shiftleftA = mapTransform DType.shiftleft

-- | Divide each element by 2.
shiftrightA :: NdArray -> NdArray
shiftrightA = mapTransform DType.shiftright

-- | Returns the element at the 0th position of the array.
origin :: forall a . DType a => NdArray -> a
origin (NdArray _ v) = (v V.! 0) <-@ typeRep @a

-- | Returns the largest element.
maxElem :: forall a . DType a => NdArray -> a
maxElem nd = foldrA max (origin nd) nd

-- | Returns the smallest element.
minElem :: forall a . DType a => NdArray -> a
minElem nd = foldrA min (origin nd) nd

-- | Constrains all elements of the array to the range specified by [mini, maxi].
-- If they are given as Nothing, the range is infinite in that direction.
-- NB: must still specify type for Nothing i.e. clip (Nothing :: Maybe Int) Nothing myNd
clip :: forall a . DType a => Maybe a -> Maybe a -> NdArray -> NdArray
clip mini maxi (NdArray s v) = case v =@= (undefined :: Vector a) of
  Just HRefl ->
    case (mini, maxi) of
      (Just mn, Just mx) -> mapA (\x -> if x <= mn then mn else if x >= mx then mx else x) (NdArray s v)
      (Just mn, Nothing) -> mapA (\x -> if x <= mn then mn else x) (NdArray s v)
      (Nothing, Just mx) -> mapA (\x -> if x >= mx then mx else x) (NdArray s v)
      (Nothing, Nothing) -> NdArray s v
  _ -> throw (TypeMismatch $ "Min and max types do not match array type of " <> show (vecType v) <> ".")

-- * Two Arguments

-- | The generic function for operating on two matching DType arrays with the same shape
-- in an element-wise/pointwise way. Errors if mismatching
-- >>> x = fromList [2,2] [1,2,3,4 :: Int]
-- >>> y = fromList [2,2] [5,2,2,2 :: Int]
-- >>> printArray $ pointwiseZip (DType.multiply) x y
-- 5 4
-- 6 8
pointwiseZip :: (forall t . DType t => t -> t -> t) -> NdArray -> NdArray -> NdArray
pointwiseZip zipfunc (NdArray s v) (NdArray r u) = if s == r then
  case v =@= u of
    Just HRefl -> NdArray s (V.zipWith zipfunc v u)
    Nothing    -> throw (DTypeMismatch (NdArray s v) (NdArray r u) "pointwiseZip")
  else throw (ShapeMismatch (NdArray s v) (NdArray r u) "pointwiseZip")

-- | A slightly specialised version of pointwise zip intended for comparative functions.
pointwiseBool :: (forall t . DType t => t -> t -> Bool) -> NdArray -> NdArray -> NdArray
pointwiseBool zipfunc (NdArray s v) (NdArray r u) = if s == r then
  case v =@= u of
    Just HRefl -> NdArray s (V.zipWith zipfunc v u)
    Nothing    -> throw (DTypeMismatch (NdArray s v) (NdArray r u) "pointwiseZip")
  else throw (ShapeMismatch (NdArray s v) (NdArray r u) "pointwiseZip")

-- | Completely generic zip on two NdArrays. If the shapes mismatch, they are truncated as with
-- standard zips. Function inputs must match the DTypes.
zipArrayWith :: forall a b c . (DType a, DType b, DType c) => (a -> b -> c) -> NdArray -> NdArray -> NdArray
zipArrayWith zipfunc (NdArray s v) (NdArray r u) =
  let
    -- Truncate the shapes to match each other
    ndC1 = constrainShape r (NdArray s v)
    ndC2 = constrainShape s (NdArray r u)
    s' = shape ndC1
  in
    -- Type check the function
    case (v =@ typeRep @(Vector a), u =@ typeRep @(Vector b)) of
      (Just HRefl, Just HRefl) ->
        let
          v' = getVector ndC1 :: Vector a
          u' = getVector ndC2 :: Vector b
        in NdArray s' (V.zipWith zipfunc v' u' :: Vector c)
      _    -> throw (TypeMismatch "Cannot zip NdArrays with different dtypes to the zip function.")

-- | Pointwise integer division. Will return an NdArray of type Int.
elemDiv :: NdArray -> NdArray -> NdArray
elemDiv (NdArray s v) (NdArray r u) = if s == r then
  case v =@= u of
    Just HRefl -> elemDivVec s v r u
    Nothing    -> throw (DTypeMismatch (NdArray s v) (NdArray r u) "elemDiv")
  else throw (ShapeMismatch (NdArray s v) (NdArray r u) "elemDiv")

elemDivVec :: forall a . DType a => [Integer] -> Vector a -> [Integer] -> Vector a -> NdArray
elemDivVec s v r u = zipArrayWith (DType.div :: a -> a -> Int) (NdArray s v) (NdArray r u)

-- | Pointwise division
elemDivide :: NdArray -> NdArray -> NdArray
elemDivide = pointwiseZip DType.divide

-- | Pointwise exponentiation (preserving DType)
elemPow :: NdArray -> NdArray -> NdArray
elemPow = pointwiseZip DType.pow

-- | Pointwise exponentiation which forces precision.
-- Takes some NdArray of bases, an array of Double exponents and returns an array of Doubles.
elemPower :: NdArray -> NdArray -> NdArray
elemPower (NdArray s v) (NdArray r u) = if s == r then
  case u =@ typeRep @(Vector Double) of
    Just HRefl -> elemPowerVec s v r u
    Nothing    -> throw (DTypeMismatch (NdArray s v) (NdArray r u) "elemPower")
  else throw (ShapeMismatch (NdArray s v) (NdArray r u) "elemPower")

elemPowerVec :: forall a . DType a => [Integer] -> Vector a -> [Integer] -> Vector Double -> NdArray
elemPowerVec s v r u = zipArrayWith (DType.power :: a -> Double -> Double) (NdArray s v) (NdArray r u)

-- * Many Arguments

-- | Takes the pointwise sum over all the given NdArrays. If they are different shapes,
-- the smaller dimensions are padded out with the identity element.
-- The sum of the empty list is the singleton 0.
sum :: [NdArray] -> NdArray
sum [] = singleton (0::Int)
sum [nd] = nd
sum (NdArray s v : nds) = foldr (\x acc -> padShape sh x + acc) (zeros (vecType v) sh) (NdArray s v : nds)
  where sh = maximiseShape (map shape nds)

-- Takes the maximum of each element pointwise matching from the end.
maximiseShape :: [[Integer]] -> [Integer]
maximiseShape [] = []
maximiseShape [sh] = sh
maximiseShape (sh : shs) =
  let
    m = maximiseShape shs
    diff = length sh - length m
  in
    if diff > 0
      then zipWith max sh (take diff sh ++ m)
      else zipWith max (take (-diff) m ++ sh) m

-- | Finds the mean pointwise over the list of arrays. Smaller arrays are padded out with
-- the identity element.
mean :: [NdArray] -> NdArray
mean [] = NdArray [] $ V.fromList ([] :: [Int])
mean nds = s `elemDivide` NdArray sh (V.replicate (size sh) (length nds))
  where
    s = Numskull.sum nds
    sh = shape s

-- * Type & Shape Conversion
--------------------------------------------------------------------------------

{- | Converting between the standard dtypes and changing the shapes of arrays.
NB the difference between 'size' and 'shape'. The shape is an Integer list
describing the width of each dimension. Size refers to the total number of
elements in the array, i.e. the product of the shape.
-}

-- | Converts an NdArray of one type to any other with a DType instance.
convertDTypeTo :: forall a . DType a => TypeRep a -> NdArray -> NdArray
convertDTypeTo t (NdArray s v) = convertDTFromTo (vecType v) t (NdArray s v)

-- Helper with additional typing information
convertDTFromTo :: forall a b . (DType a, DType b) =>
  TypeRep a -> TypeRep b -> NdArray -> NdArray
convertDTFromTo _ _ (NdArray s v) = case v =@= (undefined :: Vector a) of
  Just HRefl -> NdArray s (V.map convert v)
  _ -> error "Impossible type mismatch."
  where
    convert :: (DType a, DType b) => a -> b
    convert x = DType.rationalToDtype (DType.dtypeToRational x)

-- | Converts the second NdArray to be the same DType as the first.
matchDType :: NdArray -> NdArray -> NdArray
matchDType (NdArray _ v) = convertDTypeTo (vecType v)

{- Helper which checks that the array isn't larger than the shape contraints.
If it is valid the Boolean in the pair will be true and the vector is returned.
If it is invalid the vector is truncated first.
-}
constrainSize :: DType a => Integer -> Vector a -> (Bool, Vector a)
constrainSize s v =
  if si < V.length v then (False, V.take si v)
  else (True, v)
  where
    si = fromIntegral s :: Int

-- Fill out any spaces in a vector smaller than the shape with 0s (or whatever the dtype 'identity' is)
padSize :: DType a => Integer -> Vector a -> Vector a
padSize s v = v V.++ V.replicate ((fromIntegral s ::Int) - len) DType.addId
  where len = V.length v

-- Contrain or pad the vector to match the size
setSize :: DType a => Integer -> Vector a -> Vector a
setSize s v = let (unchanged, u) = constrainSize s v in
  if unchanged then padSize s u else u

{- | Truncate or pad the NdArray to match the new given size.
The shape will be collapsed to 1xn.
-}
-- >>> x = fromList [2,2] [1,2,3,4 :: Int]
-- >>> printArray $ resize 6 x
-- 1 2 3 4 0 0
-- >>> printArray $ resize 2 x
-- 1 2
resize :: Integer -> NdArray -> NdArray
resize s (NdArray _ v) = NdArray [s] (setSize s v)

-- | Shape-shift one array to another of the same size (Nothing otherwise).
-- >>> x = fromList [2,3] [1,2,3,4,5,6 :: Int]
-- >>> printArray x
-- 1 2
-- 3 4
-- 5 6
-- >>> printArray $ fromJust $ reshape [3,2] x
-- 1 2 3
-- 4 5 6
reshape :: [Integer] -> NdArray -> Maybe NdArray
reshape r (NdArray s v) = if product s == product r
  then Just $ NdArray r v
  else Nothing

-- Checks that the first shape is smaller or equal to the second.
smallerShape :: [Integer] -> [Integer] -> Bool
smallerShape s r = (length s <= length r) && and (zipWith (<=) s r)

-- | Adds zero-rows to an array. Will error if you map to a smaller shape.
-- >>> x = fromList [2,2] [1,2,3,4 :: Int]
-- >>> printArray $ padShape [4,3] x
-- 1 2 0 0
-- 3 4 0 0
-- 0 0 0 0
padShape :: [Integer] -> NdArray -> NdArray
padShape r (NdArray s v) =
  let
    nullVec = V.replicate (size r) (identityElem v)
    newIndices = V.imap (\i _ -> fromIntegral $ map1DIndex s r (toInteger i) :: Int) v
  in
    if smallerShape s r
    then NdArray r (V.unsafeUpdate_ nullVec newIndices v)
    else error "Cannot map to a smaller shape."

-- | Truncates the array to be no larger than the specified dimensions.
constrainShape :: [Integer] -> NdArray -> NdArray
constrainShape r (NdArray s v) =
  let
    s' = zipWith min r s
    sPad = s' ++ replicate (length s - length r) 1
  in NdArray s' $
    V.ifilter (\i _ -> and $ zipWith (<) (expandInd s (toInteger i)) sPad) v

-- | Takes a pair of NdArrays and attempts to copy slices so that they are size matched.
-- Arrays are broadcastable if they either match in corresponding dimensions or one is
-- of dimension size 1 e.g. [2,5,1] and [2,1,6]. Missing dimensions are padded with 1s
-- e.g. [1,2,3] and [3] are broadcastable.
broadcast :: (NdArray, NdArray) -> Maybe (NdArray, NdArray)
broadcast (NdArray s v, NdArray r u) =
  let
    (s',v',r',u') = broadcastDimensions s v r u
    newshape = zipWithM (\x y -> if x == y || x == 1 || y == 1
      then Just (max x y) else Nothing) s' r'
  in
    case newshape of
      Nothing -> Nothing
      Just ns -> Just (
        NdArray ns $ padRepeats ns m s' v',
        NdArray ns $ padRepeats ns m r' u')
        where m = fst $ mapIndicies ns

-- Pads out dimensions for broadcasting if one array is dimensionally smaller than another.
-- e.g. [1,2,3] and [3].
broadcastDimensions :: (DType a, DType b) =>
  [Integer] -> Vector a -> [Integer] -> Vector b ->
    ([Integer], Vector a, [Integer], Vector b)
broadcastDimensions s v r u
  | sl == rl = (s,v,
                r,u)
  |  sl > rl = (s,v,
                sdiff ++ r,
                V.concat $ replicate (fromIntegral $ product sdiff) u)
  |  sl < rl = (rdiff ++ s,
                V.concat $ replicate (fromIntegral $ product rdiff) v,
                r,u)
  where
    sl = length s
    rl = length r
    diff = Prelude.abs (sl - rl)
    sdiff = take diff s
    rdiff = take diff r

-- Pads out a newshape with repetitions of the existing values
-- Takes the newshape, its map, the old shape and the vector.
padRepeats :: DType a =>
  [Integer] -> M.Map Int [Integer] -> [Integer] -> Vector a -> Vector a
padRepeats newshape oneDmap s v =
  let (_, multiMap) = mapIndicies s
  in V.generate (fromIntegral $ product newshape) (\i ->
    let
        multiI = oneDmap M.! i -- equivalent multi-index
        multiWrap = zipWith mod multiI s -- wrap the index over dimensions of size 1
        flatWrap = multiMap M.! multiWrap -- collapse the index over the vector
    in v V.! flatWrap)

-- | Concatenate a list of tensors into a single tensor. All input tensors must have the
-- same shape, except for the dimension size of the axis to concatenate on.
-- Returns Nothing if the arrays are not all of the same type or matching shapes.
concatAlong :: Int -> [NdArray] -> Maybe NdArray
concatAlong _ [] = Nothing
concatAlong _ [nd] = Just nd
concatAlong axis ((NdArray s v):nds) =
  case extractVectors (NdArray s v : nds) (vecType v) of
    Nothing -> Nothing
    Just vs ->
      case concatAlongVec vs (map shape (NdArray s v : nds)) axis of
          Nothing -> Nothing
          Just (ns, c) -> Just $ NdArray ns c

-- Helper for concatenation of vectors and their associated shapes.
concatAlongVec :: forall a . DType a => [Vector a] -> [[Integer]] -> Int -> Maybe ([Integer], Vector a)
concatAlongVec vs shs axis =
  if not (checkShapeLengths shs) || not (checkAxis axis shs) then Nothing
  else
    let
      -- Calculates the newshape by adding up all the dimensions along the axis
      axDim = axisDimensions axis shs
      newshape = replaceNth axis (Prelude.sum axDim) (head shs)
      -- Each array to be concatenated is given a number to index it with
      -- Values are indexed by array number, then by position in the array
      arrayPlot = concat $ zipWith (\arr dim -> [(arr, x) | x <- [0..dim-1]]) [0..] axDim
      (newMultiInds, _) = mapIndicies newshape
      subArrayMaps = map (snd . mapIndicies) shs
    in
      Just (newshape,
        V.generate (length newMultiInds) (\i ->
          let
            -- Generating the new vector by converting the new flat index to a multi-index
            -- then mapping it to a sub-array and index and reading the value.
            multiI = newMultiInds M.! i
            (arrayNo, arrayAxInd) = arrayPlot !! fromIntegral (multiI !! axis)
            array = vs !! arrayNo
            arrayMap = subArrayMaps !! arrayNo
            arrayMultiI = replaceNth axis arrayAxInd multiI
          in
            vecInd arrayMap array arrayMultiI <-@ typeRep @a
        )
      )

-- Swaps in a value at the given index
replaceNth :: Int -> a -> [a] -> [a]
replaceNth n x l = take n l ++ [x] ++ drop (n+1) l

-- Checks for the same number of dimensions
checkShapeLengths :: [[Integer]] -> Bool
checkShapeLengths [] = False
checkShapeLengths shapes = all (\sh -> length sh == baseLen) shapes
  where baseLen = length $ head shapes

-- Checks that each dimension is the same save perhaps the axis one
checkAxis :: Int -> [[Integer]] -> Bool
checkAxis _ [] = False
checkAxis axis shapes =
  let
    dropAxis = map (\sh -> take axis sh ++ drop (axis+1) sh) shapes
    base = head dropAxis
  in 0 <= axis && axis <= length base &&
      foldr intersect base dropAxis == base

-- Gets the size of the dimension of the axis over all the shapes
axisDimensions :: Int -> [[Integer]] -> [Integer]
axisDimensions axis = map (!! axis)

-- | Takes an array, set of sub-indicies and axis and repeatedly takes slices
-- of the array restricted to that index along the specified axis.
-- The slices are then concatenated into the final array.
gather :: NdArray -> [Integer] -> Integer -> NdArray
gather nd is axis = fromJust $ concatAlong ax (map (\i -> slice (sliceLead ++ [(i,i)]) nd) is)
  where
    ax = fromIntegral axis
    sliceLead = replicate ax (0,-1)

-- * Matrix Operations
--------------------------------------------------------------------------------

-- * Rows, Columns and Diagonals

{- | Switches the rows at the two given indicies over.
NB: designed for 2x2 matricies so will only make swaps in the 'front' matrix of a tensor.
-}
swapRows :: Integer -> Integer -> NdArray -> NdArray
swapRows r1 r2 (NdArray s v)
  | r1 == r2 = NdArray s v
  | length s < 2 = error "Too few rows to make swaps."
  | r1 >= numRows || r2 >= numRows = error "Row index exceeds number of rows."
  | otherwise =
      let
        lenRows = fromIntegral @Integer @Int $ s !! (colI+1)
        rowInd1 = fromIntegral @Integer @Int $ collapseInd s $ replicate colI 0 ++ [r1,0]
        rowInds1 = V.iterateN lenRows succ rowInd1
        rowInd2 = fromIntegral @Integer @Int $ collapseInd s $ replicate colI 0 ++ [r2,0]
        rowInds2 = V.iterateN lenRows succ rowInd2
        row1 = V.slice rowInd1 lenRows v
        row2 = V.slice rowInd2 lenRows v
      in
        NdArray s $ V.update_ v (rowInds2 V.++ rowInds1) (row1 V.++ row2)
  where
    colI = length s -2
    numRows = s !! colI

{- | Gets the flat array of the leading diagonal of the 'front' matrix of the tensor. -}
diagonal :: NdArray -> NdArray
diagonal (NdArray s v) = NdArray [fromIntegral $ V.length v'] v'
  where v' = diagonalVec s v

-- Helper to take the leading diagonal in the vector form.
diagonalVec :: forall a . DType a => [Integer] -> Vector a -> Vector a
diagonalVec s = V.ifilter (\i _ -> i `mod` (rowLen+1) == 0 && i < rowLen*columns)
  where
    rowLen = fromIntegral @Integer @Int $ s!!(length s -1)
    columns = fromIntegral @Integer @Int $ s!!(length s -2)

-- * Transposition

-- | Reverses the order of axes and switches the elements accordingly.
transpose :: NdArray -> NdArray
transpose (NdArray sh v) = transposePerm dec (NdArray sh v)
  where
    l = length sh
    dec = [l-1, l-2 .. 0]

-- | Transposes the axes of an array according to the given permutation (e.g. [2,0,1])
transposePerm :: [Int] -> NdArray -> NdArray
transposePerm perm (NdArray sh v) =
  let
    sh' = permuteList perm sh
    perm' = invertPermutation perm
    (_, toV) = mapIndicies sh
    (fromU, _) = mapIndicies sh'
    sz = V.length v
  in NdArray sh' $ V.generate sz (\i ->
      let
        multU = fromU M.! i
        flatV = toV M.! permuteList perm' multU
      in v V.! flatV)

-- Applies a permutation to a list
permuteList :: [Int] -> [a] -> [a]
permuteList perm l = if sort perm /= [0 .. length l -1]
  then error "Invalid permutation given."
  else map (l!!) perm

-- Finds the inverse of a permutation
invertPermutation :: [Int] -> [Int]
invertPermutation perm = map (\i -> fromJust $ elemIndex i perm) [0..length perm -1]

-- * Multiplication

-- | Dot product over matricies of the same shape.
dot :: forall a. DType a => NdArray -> NdArray -> a
dot (NdArray s v) nd2 = foldrA DType.add (identityElem v <-@ typeRep @a) ((NdArray s v)*nd2)

-- | Standard matrix multiplication following NumPy conventions.
-- 1D arrays have the extra dimension pre/appended
-- 2D arrays are multiplied as expected
-- ND-arrays are broadcast to match each other where possible and treated as stacks of nxm/pxq arrays.
matMul :: NdArray -> NdArray -> NdArray
matMul (NdArray s v) (NdArray r u) =
  case v =@= u of
    Just HRefl ->
      case (reverse s, reverse r) of
        -- Standard matrix multiplication
        ([m, n], [o, p]) | m == p -> NdArray [n,o] (matMulVec s v r u)
        -- 1D arrays have the extra dimension pre/appended then result collapses back to 1D
        ([m], [o, p])   | m == p -> NdArray [o] (matMulVec [1,m] v r u)
        ([m, n], [p])   | m == p -> NdArray [n] (matMulVec s v [p,1] u)
        -- ND-arrays are broadcast to match each other where possible and treated as
        -- stacks of nxm/pxq arrays.
        (m : n : _, o : p : _) | m == p ->
          let
            (s', v', _r', u') = broadcastDimensions s v r u
            stackA = vectorChunksOf (fromIntegral @Integer @Int $ m * n) v'
            stackB = vectorChunksOf (fromIntegral @Integer @Int $ o * p) u'
            stackAB = zipWith4 matMulVec (repeat [n,m]) stackA (repeat [p,o]) stackB
          in
            NdArray (take (length s' -2) s' ++ [n,o]) $ V.concat stackAB
        _ -> throw (ShapeMismatch (NdArray s v) (NdArray r u) "matMul")
    _ -> throw (DTypeMismatch (NdArray s v) (NdArray r u) "matMul")

-- Splits a vector into a list of vectors of the given size.
vectorChunksOf :: V.Storable a => Int -> Vector a -> [Vector a]
vectorChunksOf _ v | V.null v = []
vectorChunksOf n v = first : vectorChunksOf n rest
  where (first, rest) = V.splitAt n v

-- Returning the vector result of the standard nxm matMul
matMulVec :: forall a . DType a =>
  [Integer] -> Vector a -> [Integer] -> Vector a -> Vector a
matMulVec s v r u =
  let
    oneDkey = fst $ mapIndicies [s!!0, r!!1]
    sz = M.size oneDkey
    map1 = vecInd (snd $ mapIndicies s) v
    map2 = vecInd (snd $ mapIndicies r) u
    ks = [0 .. (s!!1 -1)]
  in
    V.generate sz (matMulElem map1 map2 ks . (M.!) oneDkey)

-- Calculates the element at position [i,j] in the resultant nxp matrix of a matMul
matMulElem :: forall a . DType a =>
  ([Integer] -> a) -> ([Integer] -> a) -> [Integer] -> [Integer] -> a
matMulElem map1 map2 ks (i:j:_) =
  foldr (\k acc -> DType.add acc $ DType.multiply (map1 [i,k]) (map2 [k,j])) DType.addId ks
matMulElem _ _ _ _ = DType.multId :: a

{- | General matrix multiplication. Calculates alpha*AB + beta*C with the option
to transpose A and B first.
Takes A, B, C, A transpose?, B transpose?, alpha, beta
Returns nothing if the matrix types/sizes do not match.
Will attempt to broadcast the shape of C and convert the types of alpha & beta.

For more information see:
https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms#Level_3
NB: if the matricies are integers the scalars will also become integers so you should convert the matricies first
-}
gemm :: (DType a, DType b) =>
  NdArray -> NdArray -> NdArray -> Bool -> Bool -> a -> b -> Maybe NdArray
gemm (NdArray sA vA) (NdArray sB vB) (NdArray sC vC) transA transB alpha beta =
  let
    -- Apply transposition to A and B if specified
    (sAT, vAT) = applyTransposition (sA, vA) transA
    (sBT, vBT) = applyTransposition (sB, vB) transB
  in
    -- Check all the types match
    case gemmTyping vAT vBT vC alpha beta of
      Nothing -> Nothing
      Just (vA', vB', vC', alpha', beta') ->
        -- Check A and B have shapes (M,K) and (K, N)
        if (length sAT /= 2) || (length sBT /= 2) || (length sC /= 2) || sAT!!1 /= sBT!!0 then Nothing
        else
          let
            alphaAB = scale alpha' (matMul (NdArray sAT vA') (NdArray sBT vB'))
            sAB = shape alphaAB
          in
            -- Check if C dimension matches or is broadcastable
            if (sC!!0 /= 1 && sC!!0 /= sAB!!0) || (sC!!1 /= 1 && sC!!1 /= sAB!!1) then Nothing
            else
              let betaC = scale beta' $ if (sC!!0 /= sAB!!0) || (sC!!1 /= sAB!!1)
                  then snd $ fromJust $ broadcast (alphaAB, NdArray sC vC')
                  else NdArray sC vC'
              in
                -- Finally, combine the two
                Just (alphaAB + betaC)

-- Transpose the shape-vector pair if the boolean is true, otherwise return the original.
applyTransposition :: forall a . DType a => ([Integer], Vector a) -> Bool -> ([Integer], Vector a)
applyTransposition (s, v) b =
  let
    ndT = Numskull.transpose (NdArray s v)
    sT = shape ndT
    vT = getVector ndT :: Vector a
  in
    if b then (sT, vT) else (s, v)

-- Checking all mats are same type & converting scalars if neccersary
gemmTyping :: forall a b c d e . (DType a, DType b, DType c, DType d, DType e) =>
  Vector a -> Vector b -> Vector c -> d -> e ->
    Maybe (Vector a, Vector a, Vector a, a, a)
gemmTyping vA vB vC alpha beta =
  case vA =@= vB of
    Just HRefl ->
      case vA =@= vC of
        Just HRefl ->
          -- All matricies match types
          let
            vA' = vA :: Vector a
            vB' = vB :: Vector a
            vC' = vC :: Vector a

            -- Convert scalar types
            alpha' =
              case alpha =@= (undefined :: a) of
                Just HRefl -> alpha :: a
                _ -> DType.rationalToDtype (DType.dtypeToRational alpha) :: a
            beta' =
              case beta =@= (undefined :: a) of
                Just HRefl -> beta :: a
                _ -> DType.rationalToDtype (DType.dtypeToRational beta) :: a
          in
            Just (vA', vB', vC', alpha', beta')
        _ -> Nothing
    _ -> Nothing

-- * Determinants and Inverses

-- | Converts a nxn matrix to upper triangle form. O(n^3).
upperTriangle :: NdArray -> NdArray
upperTriangle (NdArray [] v) = NdArray [] v
upperTriangle (NdArray (c:rs) v) =
  let
    (_, fromMulti) = mapIndicies (c:rs)
    traversals = [(i,j,k) | i <- [0..c-1], j <- [i+1..c-1], k <- [0..c-1]]
  in
    NdArray (c:rs) $ triangulateVec fromMulti v traversals (identityElem v)

-- Upper triangle form on the hidden vector.
triangulateVec :: DType a => M.Map [Integer] Int -> Vector a -> [(Integer,Integer,Integer)] -> a -> Vector a
triangulateVec _ v [] _ = v
triangulateVec m v ((i,j,k) : trv) r =
  let
    jk = m M.! [j,k]
    ratio = if k == 0 then DType.divide (vecInd m v [j,i]) (vecInd m v [i,i]) else r
    scaled = DType.multiply ratio (vecInd m v [i,k])
    newVjk = DType.subtract (vecInd m v [j,k]) scaled
  in
    triangulateVec m (v V.// [(jk, newVjk)]) trv ratio

{- | Finds the determinant(s) of a tensor. Over matricies of more than two dimensions
each 2D matrix's determinant is individually calculated and concatenated together (as in numpy:
https://numpy.org/doc/stable/reference/generated/numpy.linalg.det.html ).
If the matrix is non-square it is assumed to be padded out and will have determinant of 0
-}
determinant :: forall a . DType a => NdArray -> [a]
determinant (NdArray s v) = case s of
  [] -> []
  [_] -> [identityElem v <-@ typeRep @a]
  [_,_] -> [determinant2D (NdArray s v)] :: [a]
  _ | V.null v -> []
  _ ->
    let
      (c,r) = (s!!(length s -2), last s)
      (twoDim, rest) = V.splitAt (fromIntegral$c*r) v
    in (determinant2D (NdArray [c,r] twoDim) : determinant (NdArray s rest))

{- | Calculates the determinant of a 2D matrix using LU decomposition as described in the
below paper. O(n^3).
https://informatika.stei.itb.ac.id/~rinaldi.munir/Matdis/2016-2017/Makalah2016/Makalah-Matdis-2016-051.pdf
-}
determinant2D :: forall a . DType a => NdArray -> a
determinant2D nd =
  case shape nd of
    -- 2x2 matricies are calculated quickly with the standard ad-bc
    [2,2] -> determinant2x2 nd :: a
    -- nxn matricies are row-swapped to find an arrangement with no zeros/identity elements
    -- in the leading diagonal (pivots) then put into upper triangle form
    [c,r] | c == r && not (zeroRow nd) -> case swapRowsWith0Pivot nd of
            Just (NdArray s v) ->
              let
                upperTri = upperTriangle (NdArray s v)
                upperTriV = getVector upperTri :: Vector a
                pivots = diagonalVec s upperTriV
              in
                -- determinant is the product of the pivots in upper triangle form
                V.foldr DType.multiply (DType.multId :: a) pivots :: a
    -- If the matrix is non-square or has a zero-row/column, it is singular.
            Nothing -> DType.addId :: a
    [_,_] -> DType.addId :: a
    _ -> error "Given matrix is not 2D."

-- 2x2 quick determinant calculation of ad-bc
determinant2x2 :: forall a . DType a => NdArray -> a
determinant2x2 (NdArray _ v) =
  let
    mulI i1 i2 = DType.multiply (v V.! i1) (v V.! i2)
    det = mulI 0 3 `DType.subtract` mulI 1 2
  in det <-@ (typeRep @a)

-- | Checks the whole array for the prescence of a zero-row.
zeroRow :: NdArray -> Bool
zeroRow (NdArray s v) = zeroRowVec (fromIntegral $ last s) v

-- Checks the array in vector form for a zero-row.
zeroRowVec :: forall a . DType a => Int -> Vector a -> Bool
zeroRowVec r v =
  let
    ident = DType.addId :: a
    (row, rest) = V.splitAt r v
  in
    not (V.null v)        &&
    (V.all (==ident) row  ||
    zeroRowVec r rest)

{- Repeatedly swaps rows until the matrix is found to be singular or
there are no pivots which are zero/identity elem. If singular, returns Nothing.
Note: hangs if given a matrix with a zero-row.
-}
swapRowsWith0Pivot :: NdArray -> Maybe NdArray
swapRowsWith0Pivot (NdArray s v) =
  let
    diag = diagonalVec s v
    ident = identityElem diag
  in
    case V.elemIndex ident diag of
      -- x is the column-index of the 0 pivot
      Just c -> case V.findIndex (/= ident) (frontColumn c s v) of
        -- Swap 0-pivot and non-0 rows & try again
        Just x -> swapRowsWith0Pivot $
          swapRows (fromIntegral x) (fromIntegral c) (NdArray s v)
        -- The matrix is singular
        Nothing -> Nothing
      -- There is no 0-pivot
      Nothing -> Just (NdArray s v)

{- Extracts the indexed column from the front matrix of a tensor given its shape and vector. -}
frontColumn :: forall a . DType a => Int -> [Integer] -> Vector a  -> Vector a
frontColumn col s v = V.ifilter
    (\i _ -> i `mod` rowLen == col && i < rowLen*columns) $
    v <-@ (typeRep @(Vector a))
  where
    rowLen = fromIntegral @Integer @Int $ s!!(length s -1)
    columns = fromIntegral @Integer @Int $ s!!(length s -2)

