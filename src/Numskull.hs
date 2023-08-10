{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Numskull (
  {-
  -- Metadata
  DType
  , size
  , shape
  , getVector
  , ndType
  , checkNdType
  , isEmpty

  -- Creation
  , NdArray
  , fromList
  , fromListFlat
  , TreeMatrix
  , fromMatrix
  , fromVector
  , singleton
  , arange
  , zeros
  , squareArr

  -- General mapping, folding & zipping
  , foldrA
  , mapA
  , mapTransform
  , pointwiseZip
  , pointwiseBool
  , zipArrayWith

  -- Summaries
  , origin
  , maxElem
  , minElem

  -- Mathematical constant
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

  -- Mathematical pointwise
  , elemDivide
  , elemDiv
  , elemPow
  , elemPower
  , Numskull.sum
  , mean

  -- Bounds
  , clip

  -- Type Conversions
  , convertDTypeTo
  , matchDType

  -- Size conversions
  , resize

  -- Shape conversions/manipulations
  , reshape
  , padShape
  , constrainShape
  , broadcast
  , concatAlong
  , gather

  -- Matrix manipulation
  , swapRows
  , diagonal
  , transpose
  , transposePerm

  --Matrix multiplication
  , dot
  , matMul
  , upperTriangle
  , determinant
  , determinant2D
  , swapRowsWith0Pivot
  , gemm

  -- Indexing
  , IndexRange
  , collapseInd
  , expandInd
  , map1DIndex
  , validIndex
  , (#!)
  , (!?)
  , (#!+)
  , slice
  , (!/)

  -- Pretty printing
  , printArray
  , prettyShowArray

  -- typing
  , (=@=)

  -- numpy serialisation
  , saveNpy
  , loadNpy
-}
) where

import qualified DType
import           DType                (DType)
import           Indexing
import           MatrixForm
import           NdArray
import           NdArrayException
import           Serialisation
import           Typing

import           Control.Exception
import           Control.Monad        (zipWithM)
import           Data.List            (elemIndex, intersect, sort, zipWith6)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust, isNothing)
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import           Type.Reflection

-- $setup
-- >>> import Numskull as N
-- >>> import qualified Vector

-- * Numeric & Comparative NdArray instances:
--------------------------------------------------------------------------------
--calculateNewshape :: Vector Int -> Vector Int -> Vector Int
--c-alculateNewshape sh st = V.generate (V.length sh) $ i ->
--  (sh V.! i + 1) / (st V.! i)

--V.scanr' (*) 1 (V.drop 1 $ V.fromList [10,5,5::Int])



--force $ and also check for emptyness

{-
grab :: Int -> Vector Int -> Vector Int -> Bool 
grab i sh st = 
  let
    dimAccSml = V.scanr' (*) 1 $ V.drop 1 sh
    p = V.zipWith (\t h -> i `mod` t < h) st dimAccSml
  in V.and p


stride :: NdArray -> Vector Int
stride (NdArray sh st v) = --force $ and also check for emptyness
  let
    dimAcc = V.scanr1' (*) sh
    fl = fromIntegral @Int @Float
    newshape = V.zipWith (\d s -> ceiling (fl d / fl s) ::Int) dimAcc st
    coef = V.drop 1 dimAcc
  in
    newshape -}
    {-V.generate (V.head dimAcc) $ i -> 
      let 
        x = 4
        V.zipWith 
      in 
        undefined
  -}

n1 = NdArray (V.fromList [2,2]) (V.fromList [2,1]) (V.fromList [1,2,3,4::Int])
n2 = NdArray (V.fromList [3,3]) (V.fromList [3,1]) (V.fromList [1..9::Int])
n3 = NdArray (V.fromList [4,4]) (V.fromList [4,2]) (V.fromList [1..16::Int])
n4 = NdArray (V.fromList [4,2]) (V.fromList [2,1]) (V.fromList [1,3,5,7,9,11,13,15::Int])
n5 = NdArray (V.fromList [2]) (V.fromList [1]) (V.fromList [1,2::Int])

instance Eq NdArray where
  -- | Arrays are equal if their elements and shape exactly match.
  nd1 == nd2 =
    case (stride nd1, stride nd2) of 
      (NdArray s _ v, NdArray r _ u) -> 
        case v =@= u of
          Just HRefl -> s == r && v == u
          Nothing    -> False
  nd1 /= nd2 =
    case (stride nd1, stride nd2) of 
      (NdArray s _ v, NdArray r _ u) -> 
        case v =@= u of
          Just HRefl -> s /= r || v /= u
          Nothing    -> True

instance Ord NdArray where
  {- | Arrays are only comparable when they are the same shape. Then they are
  ordered by pointwise comparison.
  -}
  nd1 `compare` nd2 = 
     case (stride nd1, stride nd2) of 
      (NdArray s t v, NdArray r d u) -> 
        if s == r then case v =@= u of
          Just HRefl -> compare v u
          Nothing    -> throw (DTypeMismatch (NdArray s t v) (NdArray r d u) "compare")
        else throw (ShapeMismatch (NdArray s t v) (NdArray r d u) "compare")
  
  nd1 <= nd2 = 
     case (stride nd1, stride nd2) of 
      (NdArray s t v, NdArray r d u) -> 
        if s == r then case v =@= u of
          Just HRefl -> v <= u
          Nothing    -> throw (DTypeMismatch (NdArray s t v) (NdArray r d u) "<=")
        else throw (ShapeMismatch (NdArray s t v) (NdArray r d u) "<=")

instance Num NdArray where
  -- | Adds elements pointwise
  (+) = broadcastZipTyped DType.add
  -- | Subtracts elements pointwise
  (-) = broadcastZipTyped DType.subtract
  -- | Multiplies elements pointwise
  (*) = broadcastZipTyped DType.multiply
  -- | Inverts all elements according to their DType instance
  negate (NdArray sh st v) = NdArray sh st (V.map DType.invert v)
  -- | Absolute value of each element
  abs (NdArray sh st v) = NdArray sh st (V.map DType.abs v)
  -- | Signum of each element
  signum (NdArray sh st v) = NdArray sh st (V.map DType.signum v)
  -- Creates a singleton array. NB: must be converted to a storable Int.
  fromInteger = singleton . fromInteger @Int


-- * General & Creation
--------------------------------------------------------------------------------

-- | Gets the total number of elements in a given array shape.
-- >>> size [2,3]
-- 6
size :: Vector Int -> Int
size sh = V.product sh

-- | Returns the shape list of an array.
shape :: NdArray -> Vector Int
shape (NdArray s _ _) = s

-- | Gets the vector of an array. Requires a type specification to output safely.
getVector :: forall a . DType a => NdArray -> Vector a
getVector (NdArray _ _  v) = v <-@ typeRep @(Vector a)

-- | Gets the TypeRep String representation of the NdArray elements
ndType :: NdArray -> String
ndType (NdArray _ _ v) = show $ vecType v

-- | Compares the type of the array elements to the given TypeRep.
checkNdType :: forall a b . (DType a, DType b) => NdArray -> TypeRep a -> Maybe (a :~~: b)
checkNdType (NdArray _ _ v) _ =
  let tv = vecType v
  in case eqTypeRep tv (typeRep @b) of
    Just HRefl -> eqTypeRep (typeRep @a) (tv :: TypeRep b)
    _ -> error "Impossibly mismatching types."

-- | Helper to get the vector typeRep.
vecType :: forall a . DType a => Vector a -> TypeRep a
vecType _ = typeRep @a

-- | Checks if the undelying vector has any elements.
isEmpty :: NdArray -> Bool
isEmpty (NdArray _ _ v) = V.null v

-- | Convert a list of arrays to a list of vectors, provided they are all of the specified type.
unpackArrays :: forall a . DType a => [NdArray] -> TypeRep a -> Maybe ([Vector Int],[Vector Int],[Vector a])
unpackArrays [] _ = Just ([],[],[])
unpackArrays ((NdArray sh st v) : nds) t =
  case v =@ typeRep @(Vector a) of
    Just HRefl ->
      case unpackArrays nds t of
        Just (shs, sts, vs) -> Just (sh:shs, st:sts, v:vs)
        _ -> Nothing
    _ -> Nothing

-- Gets the DType additive identity matching the element type of a vector.
identityElem :: forall a . DType a => Vector a -> a
identityElem _ = DType.addId :: DType a => a

-- | Creates an NdArray from a given shape and list. The number of elements must match.
-- >>> printArray $ fromList [2,2] [1,2,3,4::Int]
-- 1 2
-- 3 4
fromList :: DType a => [Int] -> [a] -> NdArray
fromList sh l =
  if length l /= product sh then throw $ CreationSize (length l) (V.fromList sh)
  else NdArray h (defStride h) (V.fromList l)
  where h = V.fromList sh

-- | Creates a 1xn NdArray from a list.
-- >>> printArray $ fromListFlat [1,2,3,4::Int]
-- 1 2 3 4
fromListFlat :: DType a => [a] -> NdArray
fromListFlat l = NdArray sh (defStride sh) (V.fromList l)
  where sh = V.fromList [length l]

genStride :: DType a => [Int] -> Vector a -> NdArray
genStride shL v = let sh = V.fromList shL in NdArray sh (defStride sh) v 

{-| Creates an NdArray from an explicitly given matrix such as the example 2x3. -}
-- >>> m :: TreeMatrix Int
-- >>> m = A [A [B 1,  B 2],
-- >>>        A [B 3,  B 4],
-- >>>        A [B 5,  B 6]]
-- >>> printArray $ fromMatrix m
-- 1 2
-- 3 4
-- 5 6
{-
fromMatrix :: DType a => TreeMatrix a -> NdArray
fromMatrix m = NdArray (matrixShape m) (V.fromList l)
  where l = flattenToList $ matrixToTree m

-- | The safe standard constructor. Returns Nothing if the
-- shape does not match the given vector length.
fromVector :: DType a => [Integer] -> Vector a -> Maybe NdArray
fromVector sh v = if V.length v == fromIntegral (product sh)
  then Just $ NdArray sh v
  else Nothing
-}

-- | Creates a 1x1 matrix
-- >>> printArray $ singleton (3::Int)
-- 3
singleton :: DType a => a -> NdArray
singleton x = NdArray (V.singleton 1) (V.singleton 1) (V.singleton x)

-- | Creates a flat array over the specified range.
arange :: (Enum a, DType a) => a -> a -> NdArray
arange mini maxi =
  if mini <= maxi
    then NdArray (V.fromList [fromIntegral $ fromEnum maxi - fromEnum mini + 1]) (V.singleton 1) (V.fromList [mini..maxi])
    else NdArray e e e
  where e = V.empty :: Vector Int

{- | Creates the smallest possible square matrix from the given list,
padding out any required space with the identity element for the DType -}
squareArr :: forall a . DType a => [a] -> NdArray
squareArr [] = NdArray e e e
  where e = V.empty :: Vector Int
squareArr xs =
  let
    l = length xs
    d = ceiling (sqrt $ fromIntegral @Int @Float l)
    p = V.replicate (d^(2::Int) - l) (DType.addId :: a)
    sh = V.fromList [d, d]
  in NdArray sh (defStride sh) (V.fromList xs V.++ p)

{- | Creates an array of the given shape of the identity element for the given type. -}
zeros :: forall a . DType a => TypeRep a -> Vector Int -> NdArray
zeros _ s = NdArray s (defStride s) zerovec
  where
    ident = DType.addId :: (DType a => a)
    zerovec = V.replicate (size s) ident :: DType a => Vector a

-- * Pointwise Functions
--------------------------------------------------------------------------------

-- * One Argument

{- | Near identical to a standard foldr instance, expect NdArrays do not have an explicit type.
Folds in row-major order.
-}
foldrA :: forall a b . DType a => (a -> b -> b) -> b -> NdArray -> b
foldrA f z nd =
  case stride nd of 
    (NdArray _ _ v) -> 
      case v =@= (undefined :: Vector a) of
        Just HRefl -> V.foldr f z v
        _ -> throw $ TypeMismatch "Fold starting value type does not match array type."

-- | Near identical to a standard map implementation in row-major order.
mapA :: forall a . forall b . (DType a, DType b) => (a -> b) -> NdArray -> NdArray
mapA f (NdArray sh st v) = case v =@= (undefined :: Vector a) of
  Just HRefl -> NdArray sh st (V.map f v)
  _ -> throw $ TypeMismatch "Map function input does not match array type."

-- | Maps functions which return the same type.
mapTransform :: (forall a . DType a => a -> a) -> NdArray -> NdArray
mapTransform f (NdArray sh st v) = NdArray sh st (V.map f v)

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
origin (NdArray _ _ v) = (v V.! 0) <-@ typeRep @a

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
clip mini maxi (NdArray sh st v) = case v =@= (undefined :: Vector a) of
  Just HRefl ->
    case (mini, maxi) of
      (Just mn, Just mx) -> mapA (\x -> if x <= mn then mn else if x >= mx then mx else x) (NdArray sh st v)
      (Just mn, Nothing) -> mapA (\x -> if x <= mn then mn else x) (NdArray sh st v)
      (Nothing, Just mx) -> mapA (\x -> if x >= mx then mx else x) (NdArray sh st v)
      (Nothing, Nothing) -> NdArray sh st v
  _ -> throw (TypeMismatch $ "Min and max types do not match array type of " <> show (vecType v) <> ".")

-- * Two Arguments

broadcastZipTyped :: (forall t . DType t => t -> t -> t) -> NdArray -> NdArray -> NdArray
broadcastZipTyped zipfunc (NdArray s t v) (NdArray r d u) = 
  case v =@= u of
    Nothing -> throw (DTypeMismatch (NdArray s t v) (NdArray r d u) "broadcastZipTyped")  
    Just HRefl ->
      case broadcastConfig (NdArray s t v) (NdArray r d u) of
        Nothing -> throw (NotBroadcastable (NdArray s t v) (NdArray r d u) " in some function")
        Just (newshape, t', d') -> 
          let newstride = defStride newshape  
          in NdArray newshape newstride $ V.generate (size newshape) (\i ->
            let 
              multi = expandInd newstride i
              -- collapse the multi index over the two arrays
              -- apply the operation to the fetched values
              v1 = v V.! collapseInd t' multi
              v2 = u V.! collapseInd d' multi
            in
              zipfunc v1 v2
          )

broadcastZipUntyped :: forall a b c . (DType a, DType b, DType c) => (a -> b -> c) -> NdArray -> NdArray -> NdArray
broadcastZipUntyped zipfunc (NdArray s t v) (NdArray r d u) =
  case (v =@ typeRep @(Vector a), u =@ typeRep @(Vector b)) of
    (Just HRefl, Just HRefl) ->
      case broadcastConfig (NdArray s t v) (NdArray r d u) of
        Nothing -> throw (NotBroadcastable (NdArray s t v) (NdArray r d u) " in some function")
        Just (newshape, t', d') ->
          let newstride = defStride newshape
          in NdArray newshape newstride $ V.generate (size newshape) (\i ->
            let 
              multi = expandInd newstride i
              -- collapse the multi index over the two arrays
              -- apply the operation to the fetched values
              v1 = v V.! collapseInd t' multi
              v2 = u V.! collapseInd d' multi
            in
              zipfunc v1 v2 :: c
          )
    _ -> throw (TypeMismatch "Cannot zip NdArrays with different dtypes to the zip function.")

-- | The generic function for operating on two matching DType arrays with the same shape
-- in an element-wise/pointwise way. Errors if mismatching
-- >>> x = fromList [2,2] [1,2,3,4 :: Int]
-- >>> y = fromList [2,2] [5,2,2,2 :: Int]
-- >>> printArray $ pointwiseZip (DType.multiply) x y
-- 5 4
-- 6 8
pointwiseZip :: (forall t . DType t => t -> t -> t) -> NdArray -> NdArray -> NdArray
pointwiseZip zipfunc nd1 nd2 = 
  case (stride nd1, stride nd2) of 
    (NdArray s t v, NdArray r d u) -> 
      if s == r then
        case v =@= u of
          Just HRefl -> NdArray s t (V.zipWith zipfunc v u)
          Nothing    -> throw (DTypeMismatch (NdArray s t v) (NdArray r d u) "pointwiseZip")
      else throw (ShapeMismatch (NdArray s t v) (NdArray r d u) "pointwiseZip")

-- | A slightly specialised version of pointwise zip intended for comparative functions.
pointwiseBool :: (forall t . DType t => t -> t -> Bool) -> NdArray -> NdArray -> NdArray
pointwiseBool zipfunc nd1 nd2 =
  case (stride nd1, stride nd2) of 
    (NdArray s t v, NdArray r d u) ->
      if s == r then
        case v =@= u of
          Just HRefl -> NdArray s t (V.zipWith zipfunc v u)
          Nothing    -> throw (DTypeMismatch (NdArray s t v) (NdArray r d u) "pointwiseBool")
        else throw (ShapeMismatch (NdArray s t v) (NdArray r d u) "pointwiseBool")

-- | Completely generic zip on two NdArrays. If the shapes mismatch, they are truncated as with
-- standard zips. Function inputs must match the DTypes.
zipArrayWith :: forall a b c . (DType a, DType b, DType c) => (a -> b -> c) -> NdArray -> NdArray -> NdArray
zipArrayWith zipfunc (NdArray s t v) (NdArray r d u) =
  let
    nd1' = stride (NdArray s t v)
    nd2' = stride (NdArray r d u)
    -- Truncate the shapes to match each other
    ndC1 = constrainShape (shape nd1') nd1' 
    ndC2 = constrainShape (shape nd2') nd2'
    s' = shape ndC1
  in
    -- Type check the function
    case (v =@ typeRep @(Vector a), u =@ typeRep @(Vector b)) of
      (Just HRefl, Just HRefl) ->
        let
          v' = getVector ndC1 :: Vector a
          u' = getVector ndC2 :: Vector b
        in NdArray s' (defStride s') (V.zipWith zipfunc v' u' :: Vector c)
      _ -> throw (TypeMismatch "Cannot zip NdArrays with different dtypes to the zip function.")

-- | Pointwise integer division. Will return an NdArray of type Int.
elemDiv :: NdArray -> NdArray -> NdArray
elemDiv nd1 nd2 = 
  case (nd1, nd2) of
    (NdArray s t v, NdArray r d u) -> 
      if s == r then
        case v =@= u of
          Just HRefl -> elemDivVec s v r u
          Nothing    -> throw (DTypeMismatch (NdArray s t v) (NdArray r d u) "elemDiv")
      else throw (ShapeMismatch (NdArray s t v) (NdArray r d u) "elemDiv")

elemDivVec :: forall a . DType a => Vector Int -> Vector a -> Vector Int -> Vector a -> NdArray
elemDivVec s v r u = broadcastZipUntyped (DType.div :: a -> a -> Int) (NdArray s (defStride s) v) (NdArray r (defStride s) u)

-- | Pointwise division
elemDivide :: NdArray -> NdArray -> NdArray
elemDivide = broadcastZipTyped DType.divide

-- | Pointwise exponentiation (preserving DType)
elemPow :: NdArray -> NdArray -> NdArray
elemPow = broadcastZipTyped DType.pow

-- | Pointwise exponentiation which forces precision.
-- Takes some NdArray of bases, an array of Double exponents and returns an array of Doubles.
elemPower :: NdArray -> NdArray -> NdArray
elemPower nd1 nd2 = 
  case (nd1, nd2) of 
    (NdArray s t v, NdArray r d u) ->
      if s == r then
      case u =@ typeRep @(Vector Double) of
        Just HRefl -> elemPowerVec s v r u
        Nothing    -> throw (DTypeMismatch (NdArray s t v) (NdArray r d u) "elemPower")
      else throw (ShapeMismatch (NdArray s t v) (NdArray r d u) "elemPower")

elemPowerVec :: forall a . DType a => Vector Int -> Vector a -> Vector Int -> Vector Double -> NdArray
elemPowerVec s v r u = broadcastZipUntyped (DType.power :: a -> Double -> Double) (NdArray s (defStride s) v) (NdArray r (defStride r) u)

-- * Many Arguments

-- | Takes the pointwise sum over all the given NdArrays. If they are different shapes,
-- the smaller dimensions are padded out with the identity element.
-- The sum of the empty list is the singleton 0.
sum :: [NdArray] -> NdArray
sum [] = singleton (0::Int)
sum [nd] = nd
sum (NdArray sh1 st1 v1 : nds) = foldr (\x acc -> padShape sh (NdArray sh1 st1 v1) + acc) (zeros (vecType v1) sh) (NdArray sh1 st1 v1 : nds)
  where sh = maximiseShape (map shape nds)

sumStride :: [NdArray] -> NdArray
sumStride xs = Numskull.sum $ map stride xs

-- Takes the maximum of each element pointwise matching from the end.
maximiseShape :: [Vector Int] -> Vector Int
maximiseShape [] = V.empty :: Vector Int
maximiseShape [sh] = sh
maximiseShape (sh : shs) =
  let
    m = maximiseShape shs
    diff = V.length sh - V.length m
  in
    if diff > 0
      then V.zipWith max sh (V.take diff sh V.++ m)
      else V.zipWith max (V.take (-diff) m V.++ sh) m

-- | Finds the mean pointwise over the list of arrays. Smaller arrays are padded out with
-- the identity element.
mean :: [NdArray] -> NdArray
mean [] = let e = V.fromList ([] :: [Int]) in NdArray e e e
mean nds = s `elemDivide` NdArray sh (defStride sh) (V.replicate (size sh) (length nds))
  where
    s = Numskull.sum nds
    sh = shape s

meanStride :: [NdArray] -> NdArray
meanStride xs = mean $ map stride xs


-- * Type & Shape Conversion
--------------------------------------------------------------------------------

{- | Converting between the standard dtypes and changing the shapes of arrays.
NB the difference between 'size' and 'shape'. The shape is an Integer list
describing the width of each dimension. Size refers to the total number of
elements in the array, i.e. the product of the shape.
-}

-- | Converts an NdArray of one type to any other with a DType instance.
convertDTypeTo :: forall a . DType a => TypeRep a -> NdArray -> NdArray
convertDTypeTo t (NdArray sh st v) = convertDTFromTo (vecType v) t (NdArray sh st v)

-- Helper with additional typing information
convertDTFromTo :: forall a b . (DType a, DType b) =>
  TypeRep a -> TypeRep b -> NdArray -> NdArray
convertDTFromTo _ _ (NdArray sh st v) = case v =@= (undefined :: Vector a) of
  Just HRefl -> NdArray sh st (V.map convert v)
  _ -> error "Impossible type mismatch."
  where
    convert :: (DType a, DType b) => a -> b
    convert x = DType.rationalToDtype (DType.dtypeToRational x)

-- | Converts the second NdArray to be the same DType as the first.
matchDType :: NdArray -> NdArray -> NdArray
matchDType (NdArray _ _ v) = convertDTypeTo (vecType v)

{- Helper which checks that the array isn't larger than the shape contraints.
If it is valid the Boolean in the pair will be true and the vector is returned.
If it is invalid the vector is truncated first.
-}
constrainSize :: DType a => Int -> Vector a -> (Bool, Vector a)
constrainSize s v =
  if s < V.length v then (False, V.take s v)
  else (True, v)
 
-- Fill out any spaces in a vector smaller than the shape with 0s (or whatever the dtype 'identity' is)
padSize :: DType a => Int -> Vector a -> Vector a
padSize s v = v V.++ V.replicate (s - len) DType.addId
  where len = V.length v

-- Contrain or pad the vector to match the size
setSize :: DType a => Int -> Vector a -> Vector a
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
resize :: Int -> NdArray -> NdArray
resize s (NdArray _ _ v) = 
  NdArray (V.singleton s) (V.singleton 1) (setSize s v)

-- | Shape-shift one array to another of the same size (Nothing otherwise).
-- >>> x = fromList [2,3] [1,2,3,4,5,6 :: Int]
-- >>> printArray x
-- 1 2
-- 3 4
-- 5 6
-- >>> printArray $ fromJust $ reshape [3,2] x
-- 1 2 3
-- 4 5 6
reshape :: Vector Int -> NdArray -> Maybe NdArray
reshape r (NdArray sh st v) = if V.product sh == V.product r
  then Just $ NdArray r st v
  else Nothing

-- Checks that the first shape is smaller or equal to the second.
smallerShape :: Vector Int -> Vector Int -> Bool
smallerShape s r = (V.length s <= V.length r) && V.and (V.zipWith (<=) s r)

-- | Adds zero-rows to an array. Will error if you map to a smaller shape.
-- >>> x = fromList [2,2] [1,2,3,4 :: Int]
-- >>> printArray $ padShape [4,3] x
-- 1 2 0 0
-- 3 4 0 0
-- 0 0 0 0

padShape :: Vector Int -> NdArray -> NdArray
padShape r nd =
  case stride nd of
    NdArray sh st v ->
      let
        nullVec = V.replicate (size r) (identityElem v)
        newIndices = V.imap (\i _ -> map1DIndex st (defStride r) i) v
      in
        if smallerShape sh r
        then NdArray r (defStride r) (V.unsafeUpdate_ nullVec newIndices v)
        else error "Cannot map to a smaller shape."

{-
setDimensions :: Int -> Vector Int -> Vector Int
setDimensions d ind = let diff = d - V.length ind in
  if diff >= 0 then V.replicate diff 0 V.++ ind
  else V.drop (-diff) ind
-}

strictSmallerShape :: Vector Int -> Vector Int -> Bool
strictSmallerShape s r = (V.length s <= V.length r) && V.and (V.zipWith (<) s r)

addDimensions :: Int -> Vector Int -> Vector Int
addDimensions d ind = V.replicate (d - V.length ind) 1 V.++ ind

padShape' :: Vector Int -> NdArray -> NdArray
padShape' r (NdArray sh st v) =
  NdArray r d $ V.generate (size r) (\i ->
    let 
      multi = expandInd d i
    in
      if V.and $ V.zipWith (<) multi p
        then v V.! (collapseInd st $ V.drop (V.length multi - V.length st) multi)
        else identityElem v
    )
    where
      d = defStride r
      p = addDimensions (V.length r) sh


-- | Truncates the array to be no larger than the specified dimensions.
constrainShape :: Vector Int -> NdArray -> NdArray
constrainShape r nd =
  case stride nd of
    NdArray sh _ v ->
      let
        s' = V.zipWith min r sh
        sPad = s' V.++ V.replicate (V.length sh - V.length r) 1
      in NdArray s' (defStride s') $
        V.ifilter (\i _ -> V.and $ V.zipWith (<) (expandInd sh i) sPad) v


-- generate the strides & new shape for two maybe broadcastable arrays
broadcastConfig :: NdArray -> NdArray -> Maybe (Vector Int, Vector Int, Vector Int)
broadcastConfig (NdArray s t v) (NdArray r d u) = 
  let
    s' = V.replicate (V.length r - V.length s) 1 V.++ s
    t' = V.replicate (V.length r - V.length s) 0 V.++ t
    r' = V.replicate (V.length s - V.length r) 1 V.++ r
    d' = V.replicate (V.length s - V.length r) 0 V.++ d
    newshape = V.zipWithM (\x y ->
      if x == y || x == 1 || y == 1
      then Just (max x y) else Nothing) s' r'
    t'' = V.zipWith3 (\sx rx tx ->
      if sx == 1 && rx /= 1 then 0 else tx) s' r' t'
    d'' = V.zipWith3 (\rx sx dx ->
      if rx == 1 && sx /= 1 then 0 else dx) r' s' d'
  in
    (\x->(x,t'',d'')) <$> newshape

{-}
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
        where m = fst $ mapIndices ns
-}
-- Pads out dimensions for broadcasting if one array is dimensionally smaller than another.
-- e.g. [1,2,3] and [3].
broadcastDimensions :: (DType a, DType b) =>
  Vector Int -> Vector a -> Vector Int -> Vector b ->
    (Vector Int, Vector a, Vector Int, Vector b)
broadcastDimensions s v r u
  | sl == rl = (s,v,
                r,u)
  |  sl > rl = (s,v,
                sdiff V.++ r,
                V.concat $ replicate (V.product sdiff) u)
  |  sl < rl = (rdiff V.++ s,
                V.concat $ replicate (V.product rdiff) v,
                r,u)
  where
    sl = V.length s
    rl = V.length r
    diff = Prelude.abs (sl - rl)
    sdiff = V.take diff s
    rdiff = V.take diff r

-- Pads out a newshape with repetitions of the existing values
-- Takes the newshape, its map, the old shape and the vector.
{-
padRepeats :: DType a =>
  [Integer] -> M.Map Int [Integer] -> [Integer] -> Vector a -> Vector a
padRepeats newshape oneDmap s v =
  let (_, multiMap) = mapIndices s
  in V.generate (fromIntegral $ product newshape) (\i ->
    let
        multiI = oneDmap M.! i -- equivalent multi-index
        multiWrap = zipWith mod multiI s -- wrap the index over dimensions of size 1
        flatWrap = multiMap M.! multiWrap -- collapse the index over the vector
    in v V.! flatWrap)
-}

-- | Concatenate a list of tensors into a single tensor. All input tensors must have the
-- same shape, except for the dimension size of the axis to concatenate on.
-- Returns Nothing if the arrays are not all of the same type or matching shapes.
concatAlong :: Int -> [NdArray] -> Maybe NdArray
concatAlong _ [] = Nothing
concatAlong _ [nd] = Just nd
concatAlong axis (NdArray sh st v : nds) =
  case unpackArrays (NdArray sh st v : nds) (vecType v) of
    Nothing -> Nothing
    Just (shs, sts, vs) -> 
      case concatAlongVec shs sts vs axis of
        Nothing -> Nothing
        Just (csh, cst, cv) -> Just $ NdArray csh cst cv

-- Helper for concatenation of vectors and their associated shapes.
concatAlongVec :: forall a . DType a => [Vector Int] -> [Vector Int] -> [Vector a] -> Int -> Maybe (Vector Int, Vector Int, Vector a)
concatAlongVec shs sts vs axis =
  if not (checkShapeLengths shs) || not (checkAxis axis shs) then Nothing
  else
    let
      -- Calculates the newshape by adding up all the dimensions along the axis
      axDim = map (V.! axis) shs
      newshape = head shs V.// [(axis, Prelude.sum axDim)]
      newstride = defStride newshape
      -- Each array to be concatenated is given a number to index it with
      -- Values are indexed by array number, then by position in the array
      --arrayPlot = V.fromList $ concat $ zipWith (\arr dim -> [(arr, x) | x <- [0..dim-1]]) [0..] axDim
      arrayNums = V.concat $ zipWith (V.replicate) [0..] axDim
      arrayAxInds = V.concat $ map (V.enumFromN 0) axDim
      --(newMultiInds, _) = mapIndices newshape
      --subArrayMaps = map (snd . mapIndices) shs
    in
      Just (newshape, newstride,
        V.generate (V.product newshape) (\i ->
          let
            -- Generating the new vector by converting the new flat index to a multi-index
            -- then mapping it to a sub-array and index and reading the value.
            multiI = expandInd newstride i
            --arrayMultiI = multiI
            --multiI = newMultiInds M.! i
            arrNum = arrayNums V.! (multiI V.! axis)
            arrAxInd = arrayAxInds V.! (multiI V.! axis)
            arr = vs !! arrNum
            arrStr = sts !! arrNum
          in
            arr V.! (collapseInd arrStr (multiI V.// [(axis, arrAxInd)])) 
          --in
          --  vecInd arrayMap array arrayMultiI <-@ typeRep @a
        )
      )

-- Swaps in a value at the given index
--replaceNth :: Int -> a -> [a] -> [a]
--replaceNth n x l = take n l ++ [x] ++ drop (n+1) l

-- Checks for the same number of dimensions
checkShapeLengths :: [Vector Int] -> Bool
checkShapeLengths [] = False -- same #dimensions but also invalid
checkShapeLengths shapes = all (\sh -> V.length sh == baseLen) shapes
  where baseLen = V.length $ head shapes

-- Checks that each dimension is the same save perhaps the axis one
{-
checkAxis :: Int -> [Vector Int] -> Bool
checkAxis _ [] = False
checkAxis axis shapes =
  let
    dropAxis = map (\sh -> take axis sh ++ drop (axis+1) sh) shapes
    base = head dropAxis
  in 0 <= axis && axis <= length base &&
      foldr intersect base dropAxis == base
-}
checkAxis :: Int -> [Vector Int] -> Bool
checkAxis _ [] = False
checkAxis axis shapes =
  let (preAx, postAx) = (V.take axis (head shapes), V.drop (axis+1) (head shapes))
  in all (\s -> V.take axis s == preAx && V.drop (axis+1) s == postAx) shapes 

-- | Takes an array, set of sub-indices and axis and repeatedly takes slices
-- of the array restricted to that index along the specified axis.
-- The slices are then concatenated into the final array.
gather :: NdArray -> [Int] -> Int -> NdArray
gather nd is axis = fromJust $ concatAlong axis $ map (\i -> slice (sliceLead ++ [(i,i)]) nd) is
  where sliceLead = replicate axis (0,-1)


-- * Matrix Operations
--------------------------------------------------------------------------------

-- * Rows, Columns and Diagonals

{- | Switches the rows at the two given indices over.
NB: designed for 2x2 matrices so will only make swaps in the 'front' matrix of a tensor.
-}
swapRows :: Int -> Int -> NdArray -> NdArray
swapRows r1 r2 (NdArray sh st v)
  | r1 == r2 = NdArray sh st v
  | V.length sh < 2 = error "Too few rows to make swaps."
  | r1 >= numRows || r2 >= numRows = error "Row index exceeds number of rows."
  | otherwise =
      let
        rowInds1 = V.iterateN lenRows (+ V.last st) (st V.! colI * r1)
        rowInds2 = V.iterateN lenRows (+ V.last st) (st V.! colI * r2)
        row1 = V.map (v V.!) rowInds1
        row2 = V.map (v V.!) rowInds2
      in
        NdArray sh st $ V.force $ V.update_ v (rowInds2 V.++ rowInds1) (row1 V.++ row2)
  where
    colI = V.length sh - 2
    numRows = sh V.! colI
    lenRows = V.last sh

{- | Gets the flat array of the leading diagonal of the 'front' matrix of the tensor. -}
diagonal :: NdArray -> NdArray
diagonal (NdArray sh st v) = 
  let
    rows = V.last sh;  cols = sh V.! (V.length sh - 2)
    rStr = V.last st;  cStr = st V.! (V.length st - 2)
    v' = V.generate (min rows cols) (\i -> v V.! (i * (cStr + rStr)))
    sh' = V.singleton $ V.length v'
  in NdArray sh' (defStride sh') v'
{-
diagonal (NdArray sh st v) = NdArray sh' st' v'
  where 
    v'  = V.force $ diagonalVec sh v
    sh' = V.singleton (V.length v')
    st' = defStride sh'
-}

{-
-- Helper to take the leading diagonal in the vector form.
diagonalVec :: forall a . DType a => Vector Int -> Vector a -> Vector a
diagonalVec s = V.ifilter (\i _ -> i `mod` (rowLen+1) == 0 && i < rowLen*columns)
  where
    rowLen = s V.! (V.length s - 1)
    columns = s V.! (V.length s - 2)
-}
{-
diagonalVec :: forall a . DType a => Vector Int -> Vector Int -> Vector a -> Vector a
diagonalVec sh st v =
  let
    rows = V.last sh
    cols = sh V.! (V.length sh - 2)
    rStr = V.last st
    cStr = st V.! (V.length st - 2)
  in
    V.generate (min rows cols) ((V.!) v . (cStr + rStr) * )
-}

-- * Transposition

-- | Reverses the order of axes and switches the elements accordingly.
{-transpose :: NdArray -> NdArray
transpose (NdArray sh v) = transposePerm dec (NdArray sh v)
  where
    l = length sh
    dec = [l-1, l-2 .. 0]
-}

transpose :: NdArray -> NdArray
transpose (NdArray sh st v) = NdArray (V.reverse sh) (V.reverse st) v

-- | Transposes the axes of an array according to the given permutation (e.g. [2,0,1])

transposePerm :: [Int] -> NdArray -> NdArray
transposePerm perm (NdArray sh st v) =
  let
    sh' = V.fromList $ permuteList perm $ V.toList sh
    st' = V.fromList $ permuteList perm $ V.toList st
  in NdArray sh' st' v

-- Applies a permutation to a list
permuteList :: [Int] -> [a] -> [a]
permuteList perm l = if sort perm /= [0 .. length l -1]
  then error "Invalid permutation given."
  else map (l!!) perm

-- Finds the inverse of a permutation
invertPermutation :: [Int] -> [Int]
invertPermutation perm = map (\i -> fromJust $ elemIndex i perm) [0..length perm -1]

-- * Multiplication

-- | Dot product over matrices of the same shape.
dot :: DType a => NdArray -> NdArray -> a
dot nd1 nd2 = foldrA DType.add DType.addId (nd1*nd2)

-- | Standard matrix multiplication following NumPy conventions.
-- 1D arrays have the extra dimension pre/appended
-- 2D arrays are multiplied as expected
-- ND-arrays are broadcast to match each other where possible and treated as stacks of nxm/pxq arrays.
matMul :: NdArray -> NdArray -> NdArray
matMul (NdArray s t v) (NdArray r d u) =
  case v =@= u of
    Just HRefl ->
      case (reverse $ V.toList s, reverse $ V.toList r) of
        -- Standard matrix multiplication
        ([m, n], [q, p]) | m == p -> genStride [n,q] (matMulVec s t v r d u)
        -- 1D arrays have the extra dimension pre/appended then result collapses back to 1D
        ([m], [q, p])   | m == p -> genStride [q] (matMulVec (V.fromList [1,m]) (V.fromList [0,1]) v r d u)
        ([m, n], [p])   | m == p -> genStride [n] (matMulVec s t v (V.fromList [p,1]) (V.fromList [1,0]) u)
        -- ND-arrays are broadcast to match each other where possible and treated as
        -- stacks of nxm/pxq arrays.
        (m : n : _, q : p : _) | m == p -> case (stride (NdArray s t v), stride (NdArray r d u)) of 
          (NdArray ss st sv, NdArray sr sd su) -> 
            let (s', v', _r', u') = broadcastDimensions ss sv sr su
            in 
              case v' =@= u' of 
                Just HRefl ->
                  let  
                    -- here is where you need to care about the strides
                    stackA = vectorChunksOf (m * n) v'
                    stackB = vectorChunksOf (q * p) u'
                    dimA = V.fromList [n,m]
                    dimB = V.fromList [p,q]
                    stackAB = zipWith6 matMulVec (repeat dimA) (repeat $ defStride dimA) stackA 
                                                (repeat dimB) (repeat $ defStride dimB) stackB
                  in
                    genStride (take (V.length s' -2) (V.toList s') ++ [n,q]) $ V.concat stackAB
        _ -> throw (ShapeMismatch (NdArray s t v) (NdArray r d u) "matMul")
    _ -> throw (DTypeMismatch (NdArray s t v) (NdArray r d u) "matMul")

-- Splits a vector into a list of vectors of the given size.
vectorChunksOf :: V.Storable a => Int -> Vector a -> [Vector a]
vectorChunksOf _ v | V.null v = []
vectorChunksOf n v = first : vectorChunksOf n rest
  where (first, rest) = V.splitAt n v

-- Returning the vector result of the standard nxm matMul
matMulVec :: forall a . DType a =>
  Vector Int -> Vector Int -> Vector a -> Vector Int -> Vector Int -> Vector a -> Vector a
matMulVec s t v r d u =
  let
    oneDkey = fst $ mapIndices [s V.!0, r V.!1]
    sz = M.size oneDkey
    --map1 = vecInd (snd $ mapIndices s) v
    map1 is = v V.! collapseInd t (V.fromList is)
    map2 is = u V.! collapseInd d (V.fromList is)
    --map2 = vecInd (snd $ mapIndices r) u
    ks = [0 .. (s V.! 1 -1)]
  in
    V.generate sz (matMulElem map1 map2 ks . (M.!) oneDkey)

-- Calculates the element at position [i,j] in the resultant nxp matrix of a matMul
matMulElem :: forall a . DType a =>
  ([Int] -> a) -> ([Int] -> a) -> [Int] -> [Int] -> a
matMulElem m1 m2 ks (i:j:_) =
  foldr (\k acc ->
      DType.add acc $ DType.multiply (m1 [i,k]) (m2 [k,j])
    ) DType.addId ks
matMulElem _ _ _ _ = DType.multId :: a


{- | General matrix multiplication. Calculates alpha*AB + beta*C with the option
to transpose A and B first.
Takes A, B, C, A transpose?, B transpose?, alpha, beta
Returns nothing if the matrix types/sizes do not match.
Will attempt to broadcast the shape of C and convert the types of alpha & beta.

For more information see:
https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms#Level_3
NB: if the matrices are integers the scalars will also become integers so you should convert the matrices first
-}
gemm :: (DType a, DType b) =>
  NdArray -> NdArray -> NdArray -> Bool -> Bool -> a -> b -> Maybe NdArray
gemm (NdArray sA dA vA) (NdArray sB dB vB) (NdArray sC dC vC) transA transB alpha beta =
  let
    -- Apply transposition to A and B if specified
    (sAT, dAT) = if transA then (V.reverse sA, V.reverse dA) else (sA, dA)
    (sBT, dBT) = if transB then (V.reverse sB, V.reverse dB) else (sB, dB)
  in
    -- Check all the types match
    case gemmTyping vA vB vC alpha beta of
      Nothing -> Nothing
      Just (vA', vB', vC', alpha', beta') ->
        -- Check A and B have shapes (M,K) and (K, N)
        if (V.length sAT /= 2) || (V.length sBT /= 2) || (V.length sC /= 2) || sAT V.!1 /= sBT V.!0 then Nothing
        else
          let
            alphaAB = scale alpha' (matMul (NdArray sAT dAT vA') (NdArray sBT dBT vB'))
            sAB = shape alphaAB
          in
            -- Check if C dimension matches or is broadcastable
            if (sC V.!0 /= 1 && sC V.!0 /= sAB V.!0) || (sC V.!1 /= 1 && sC V.!1 /= sAB V.!1) then Nothing
            else
              let betaC = scale beta' (NdArray sC dC vC')
              -- $ if (sC!!0 /= sAB!!0) || (sC!!1 /= sAB!!1)
              --    then snd $ fromJust $ broadcast (alphaAB, NdArray sC vC')
              --    else NdArray sC vC'
              in
                -- Finally, combine the two
                Just (alphaAB + betaC)

{-
-- Transpose the shape-vector pair if the boolean is true, otherwise return the original.
applyTransposition :: forall a . DType a => ([Integer], Vector a) -> Bool -> ([Integer], Vector a)
applyTransposition (s, v) b =
  let
    ndT = Numskull.transpose (NdArray s v)
    sT = shape ndT
    vT = getVector ndT :: Vector a
  in
    if b then (sT, vT) else (s, v)
-}

-- Checking all mats are same type & converting scalars if neccersary
gemmTyping :: forall a b c d e . (DType a, DType b, DType c, DType d, DType e) =>
  Vector a -> Vector b -> Vector c -> d -> e ->
    Maybe (Vector a, Vector a, Vector a, a, a)
gemmTyping vA vB vC alpha beta =
  case vA =@= vB of
    Just HRefl ->
      case vA =@= vC of
        Just HRefl ->
          -- All matrices match types
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
upperTriangle (NdArray s t v) | V.null s = NdArray s t v
upperTriangle (NdArray s t v) =
  let
    c = V.head s 
    traversals = [(i,j,k) | i <- [0..c-1], j <- [i+1..c-1], k <- [0..c-1]]
  in NdArray s (defStride s) $ triangulateVec t v traversals (identityElem v)

-- Upper triangle form on the hidden vector.
triangulateVec :: DType a => Vector Int -> Vector a -> [(Int,Int,Int)] -> a -> Vector a
triangulateVec _ v [] _ = v
triangulateVec t v ((i,j,k) : trv) r =
  let
    vSet x y e = v V.// [(collapseInd t (V.fromList [x,y]), e)]
    ratio = if k == 0 then DType.divide (vGet v t [j,i]) (vGet v t [i,i]) else r
    scaled = DType.multiply ratio (vGet v t [i,k])
    newVjk = DType.subtract (vGet v t [j,k]) scaled
  in
    triangulateVec t (vSet j k newVjk) trv ratio

{- | Finds the determinant(s) of a tensor. Over matrices of more than two dimensions
each 2D matrix's determinant is individually calculated and concatenated together (as in numpy:
https://numpy.org/doc/stable/reference/generated/numpy.linalg.det.html ).
If the matrix is non-square it is assumed to be padded out and will have determinant of 0
-}
determinant :: forall a . DType a => NdArray -> [a]
determinant (NdArray s t v) = case V.length s of
  0 -> []
  1 -> [DType.addId :: a]
  2 -> [determinant2D (NdArray s t v)]
  _ | V.null v -> []
  l ->
    let
      colrow = V.drop (l-2) s
      crt = V.drop (l-2) t
      (twoDim, rest) = V.splitAt (V.product colrow) v
    in (determinant2D (NdArray colrow crt twoDim) : determinant (NdArray s t rest))

{- | Calculates the determinant of a 2D matrix using LU decomposition as described in the
below paper. O(n^3).
https://informatika.stei.itb.ac.id/~rinaldi.munir/Matdis/2016-2017/Makalah2016/Makalah-Matdis-2016-051.pdf
-}
determinant2D :: forall a . DType a => NdArray -> a
determinant2D nd =
  case V.toList $ shape nd of
    -- 2x2 matrices are calculated quickly with the standard ad-bc
    [2,2] -> determinant2x2 nd
    -- nxn matrices are row-swapped to find an arrangement with no zeros/identity elements
    -- in the leading diagonal (pivots) then put into upper triangle form
    -- determinant is the product of the new pivots
    [c,r] | c == r && not (zeroRow nd) -> case swapRowsWith0Pivot nd of
            Just (NdArray s t v) ->
              let pivots = getVector $ diagonal $ upperTriangle (NdArray s t v) :: Vector a
              in V.foldr DType.multiply (DType.multId :: a) pivots                
    -- If the matrix is non-square or has a zero-row/column, it is singular.
            Nothing -> DType.addId
    [_,_] -> DType.addId
    _ -> error "Given matrix is not 2D."

-- 2x2 quick determinant calculation of ad-bc
determinant2x2 :: forall a . DType a => NdArray -> a
determinant2x2 (NdArray _ t v) =
  let
    ad = DType.multiply (vGet v t [0,0]) (vGet v t [1,1])
    bc = DType.multiply (vGet v t [0,1]) (vGet v t [1,0])
    det = ad `DType.subtract` bc
  in det <-@ (typeRep @a)

-- | Checks the whole array for the prescence of a zero-row.
zeroRow :: NdArray -> Bool
zeroRow (NdArray s t v) = --zeroRowVec (last s) v
  case V.length s of 
    0 -> False
    1 -> s V.!0 == V.length (V.ifilter (\i x -> i `mod` (V.last t) == 0 && x == identityElem v) v)
    _ -> 
      let
        rowLen = V.last s
        numRows = s V.! (V.length s - 2)
        rowStride = V.last t
        colStride = t V.! (V.length t - 2)
      in
        isNothing $ traverse (\r ->
          let sect = V.slice (r*colStride) (rowLen*rowStride) v 
          in if rowLen == V.length (V.ifilter (\i x -> i `mod` rowStride == 0 && x == identityElem v) v)
                then Nothing
                else Just False
        ) [0..numRows-1]
      
-- Checks the array in vector form for a zero-row.
{-
zeroRowVec :: forall a . DType a => Int -> Vector a -> Bool
zeroRowVec r v =
  let
    ident = DType.addId :: a
    (row, rest) = V.splitAt r v
  in
    not (V.null v)        &&
    (V.all (==ident) row  ||
    zeroRowVec r rest)
-}

{- Repeatedly swaps rows until the matrix is found to be singular or
there are no pivots which are zero/identity elem. If singular, returns Nothing.
Note: hangs if given a matrix with a zero-row.
-}
swapRowsWith0Pivot :: NdArray -> Maybe NdArray
swapRowsWith0Pivot (NdArray sh st v) =
  let
    diag = getVector $ diagonal (NdArray sh st v)
    ident = identityElem diag
  in
    case V.elemIndex ident diag of
      -- x is the column-index of the 0 pivot
      Just c -> case V.findIndex (/= ident) (frontColumn c sh st v) of
        -- Swap 0-pivot and non-0 rows & try again
        Just x -> swapRowsWith0Pivot $
          swapRows x c (NdArray sh st v)
        -- The matrix is singular
        Nothing -> Nothing
      -- There is no 0-pivot
      Nothing -> Just (NdArray sh st v)

frontColumn :: forall a . DType a => 
  Int -> Vector Int -> Vector Int -> Vector a  -> Vector a
frontColumn c sh st v =
  let col = c * V.last st
  in V.generate (sh V.! (V.length sh -2)) (\i -> v V.! ((V.length st -2)*i+col))

{-}
{- Extracts the indexed column from the front matrix of a tensor given its shape and vector. -}
frontColumn :: forall a . DType a => Int -> [Integer] -> Vector a  -> Vector a
frontColumn col s v = V.ifilter
    (\i _ -> i `mod` rowLen == col && i < rowLen*columns) $
    v <-@ (typeRep @(Vector a))
  where
    rowLen = fromIntegral @Integer @Int $ s!!(length s -1)
    columns = fromIntegral @Integer @Int $ s!!(length s -2)
-}