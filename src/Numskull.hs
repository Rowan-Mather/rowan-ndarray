{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Numskull (
  -- Metadata
  DType
  , size
  , shape
  , getVector
  , ndType

  -- Creation
  , NdArray
  , NdArray.NdArray
  , fromList
  , fromListFlat
  , TreeMatrix
  , fromMatrix
  , fromVector
  , singleton
  , arange
  , zeros

  -- General mapping, folding & zipping
  , foldrA 
  , mapA 
  , mapTransform
  , pointwiseZip
  , pointwiseBool

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
  , elemPow 

  -- Bounds
  , clip

  -- Type Conversions
  , convertDTypeTo
  , matchDType 

  -- Size conversions
  , constrainSize 
  , padSize 
  , setSize 
  , resize

  -- Shape conversions/manipulations
  , reshape
  , padShape 
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

  -- typing
  , (=@=)

) where

import NdArray
import qualified DType
import DType (DType)
import MatrixForm
import Indexing
import Typing

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import Type.Reflection
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.List (sort, elemIndex, intersect)

import Debug.Trace

-- $setup
-- >>> import Numskull as N
-- >>> import qualified Vector

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
      Nothing    -> error $ typeMismatch (show v) (show u)
    else error $ shapeMismatch (show s) (show r)
  
  (NdArray s v) <= (NdArray r u) = if s == r then case v =@= u of
      Just HRefl -> v <= u
      Nothing    -> error $ typeMismatch (show v) (show u)
    else error $ shapeMismatch (show s) (show r)

-- To do: change fromInteger to return an integer array rather than int
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
  -- Creates a singleton array
  fromInteger = singleton . fromInteger @Int

-- * Creation & Miscellaneous

-- | Gets the total number of elements in a given array shape.
-- >>> size [2,3]
-- 6
size :: [Integer] -> Int
size sh = (fromIntegral $ product sh) :: Int

shape :: NdArray -> [Integer]
shape (NdArray s _) = s

getVector :: forall a . DType a => NdArray -> Vector a
getVector (NdArray _ v) = v <-@ typeRep @(Vector a)

-- | Gets the TypeRep String representation the NdArray elements
ndType :: NdArray -> String
ndType (NdArray _ v) = show $ vecType v
{-
ndType (NdArray _ v) = case v =@= (undefined :: DType a => Vector a) of 
  Just HRefl -> show $ vecType v
  _ -> error "Impossible type mismatch."
-}
-- Helper to get the vector typeRep
vecType :: forall a . DType a => Vector a -> TypeRep a
vecType _ = typeRep @a

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

-- Todo: get the ident of the dtype from an nd array
identityElem :: forall a . DType a => NdArray -> a
identityElem (NdArray s v) = indentityElem' v <-@ typeRep @a

-- Helper for the vectors in identityElem
indentityElem' :: forall a . DType a => Vector a -> a
indentityElem' _ = DType.addId :: DType a => a

-- | Creates an NdArray from a given shape and list. The number of elements must match.
-- >>> printArray $ fromList [2,2] [1,2,3,4::Int]
-- 1 2 
-- 3 4 
fromList :: DType a => [Integer] -> [a] -> NdArray
fromList sh l = 
  if length l /= size sh then error "Length of the list should match the total number of elements specified by the shape."
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
arange mini maxi = if mini <= maxi then NdArray [fromIntegral $ fromEnum maxi - fromEnum mini] $ V.fromList [mini..maxi]
  else error "Minimum of range is higher than maximum."

{- | Creates the smallest possible square matrix from the given list, 
padding out any required space with the identity element for the DType -}
squareArr = undefined

{- | Creates an array of the given shape of the identity element for the given type. -}
zeros :: forall a . DType a => TypeRep a -> [Integer] -> NdArray
zeros _ s = NdArray s zerovec
  where
    ident = (DType.addId :: DType a => a)
    zerovec = (V.replicate (size s) ident) :: DType a => Vector a

-- * Pointwise Functions  -- 
-- All the numpy-like functions not defined within the Eq, Ord or Num instances

----- One Argument

{- | Near identical to a standard foldr instance, expect NdArrays do not have an explicit type. 
Folds in row-major order.
-}
foldrA :: forall a b . DType a => (a -> b -> b) -> b -> NdArray -> b
foldrA f z (NdArray _ v) = 
  case v =@= (undefined :: Vector a) of
    Just HRefl -> V.foldr f z v
    _ -> error "Starting value type does not match array type."

-- | Near identical to a standard map implementation in row-major order.
mapA :: forall a . forall b . (DType a, DType b) => (a -> b) -> NdArray -> NdArray
mapA f (NdArray s v) = case v =@= (undefined :: Vector a) of 
  Just HRefl -> NdArray s (V.map f v)
  _ -> error "Function input does not match array type."

-- | Maps functions which return the same type.
mapTransform :: (forall a . DType a => a -> a) -> NdArray -> NdArray
mapTransform f (NdArray s v) = NdArray s (V.map f v)

scale :: forall a . DType a => a -> NdArray -> NdArray
scale x = mapA (DType.multiply x)

absA :: NdArray -> NdArray
absA = mapTransform (DType.abs)

signumA :: NdArray -> NdArray
signumA = mapTransform (DType.signum)

ceilA :: NdArray -> NdArray
ceilA = mapTransform (DType.ceil)

floorA :: NdArray -> NdArray
floorA = mapTransform (DType.floor)

sinA :: NdArray -> NdArray
sinA = mapTransform (DType.sin)

cosA :: NdArray -> NdArray
cosA = mapTransform (DType.cos)

tanA :: NdArray -> NdArray
tanA = mapTransform (DType.tan)

invertA :: NdArray -> NdArray
invertA = mapTransform (DType.invert)

shiftleftA :: NdArray -> NdArray
shiftleftA = mapTransform (DType.shiftleft)

shiftrightA :: NdArray -> NdArray
shiftrightA = mapTransform (DType.shiftright)

origin :: forall a . DType a => NdArray -> a
origin (NdArray s v) = (v V.! 0) <-@ typeRep @a

maxElem :: forall a . DType a => NdArray -> a
maxElem nd = foldrA max (origin nd) nd

minElem :: forall a . DType a => NdArray -> a
minElem nd = foldrA min (origin nd) nd

bound :: Ord a => a -> a -> a -> a
bound mini maxi x 
  | x <= mini = mini
  | x >= maxi = maxi
  | otherwise = x

-- NB must still specify type for Nothing i.e. clip (Nothing :: Maybe Int) Nothing myNd
clip :: forall a . DType a => Maybe a -> Maybe a -> NdArray -> NdArray
clip mini maxi (NdArray s v) = case v =@= (undefined :: Vector a) of
  Just HRefl -> 
    case (mini, maxi) of
      (Just mn, Just mx) -> mapA (\x -> bound mn mx x) (NdArray s v)
      (Just mn, Nothing) -> mapA (\x -> bound mn x x) (NdArray s v)
      (Nothing, Just mx) -> mapA (\x -> bound x mx x) (NdArray s v)
      (Nothing, Nothing) -> (NdArray s v)
  _ -> error "Min and max types do not match array type."

----- Two Arguments

-- | The generic function for operating on two DType arrays with the same shape in an element-wise/pointwise way.
-- >>> x = fromList [2,2] [1,2,3,4 :: Int]
-- >>> y = fromList [2,2] [5,2,2,2 :: Int]
-- >>> printArray $ pointwiseZip (DType.multiply) x y
-- 5 4 
-- 6 8
pointwiseZip :: (forall t . DType t => t -> t -> t) -> NdArray -> NdArray -> NdArray
pointwiseZip zipfunc (NdArray s v) (NdArray r u) = if s == r then 
  case v =@= u of
    Just HRefl -> NdArray s (V.zipWith zipfunc v u) -- Types match
    Nothing    -> error $ typeMismatch (show$ty v) (show$ty u)
  else error $ shapeMismatch (show s) (show r)

pointwiseBool :: (forall t . DType t => t -> t -> Bool) -> NdArray -> NdArray -> NdArray
pointwiseBool zipfunc (NdArray s v) (NdArray r u) = if s == r then 
  case v =@= u of
    Just HRefl -> NdArray s (V.zipWith zipfunc v u) -- Types match
    Nothing    -> error $ typeMismatch (show$ty v) (show$ty u)
  else error $ shapeMismatch (show s) (show r)

-- Todo: Needs to operate on doubles
--elemDivide :: NdArray -> NdArray -> NdArray
--elemDivide = pointwiseZip divide

-- | Pointwise division
elemDivide :: NdArray -> NdArray -> NdArray
elemDivide = pointwiseZip DType.divide

-- Todo: Needs to operate on doubles
--elemPower :: NdArray -> NdArray -> NdArray
--elemPower = pointwiseZip power

-- | Pointwise exponentiation
elemPow :: NdArray -> NdArray -> NdArray
elemPow = pointwiseZip DType.pow

-- * Type & Shape Conversion
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
matchDType (NdArray _ v) nd = convertDTypeTo (vecType v) nd 

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
smallerShape s r =
  if length s > length s then False
  else and $ zipWith (<=) s r

-- | Adds zero-rows to an array. Will error if you map to a smaller shape.
-- >>> x = fromList [2,2] [1,2,3,4 :: Int]
-- >>> printArray $ padShape [4,3] x
-- 1 2 0 0 
-- 3 4 0 0 
-- 0 0 0 0
padShape :: [Integer] -> NdArray -> NdArray
padShape r (NdArray s v) = 
  let
    nullVec = V.replicate (size r) (indentityElem' v)
    newIndices = V.imap (\i _ -> fromIntegral $ map1DIndex s r (toInteger i) :: Int) v
  in
    if smallerShape s r 
    then NdArray r (V.unsafeUpdate_ nullVec newIndices v)
    else error "Cannot map to a smaller shape."

--broadcast :: forall a . DType a => (NdArray, NdArray) -> Maybe (Vector a, Vector a)
broadcast :: (NdArray, NdArray) -> Maybe (NdArray, NdArray)
broadcast ((NdArray s v), (NdArray r u)) = 
  let 
    (s',v',r',u') = broadcastDimensions s v r u
    newshape = sequenceA $ zipWith (\x y -> if x == y || x == 1 || y == 1 
      then Just (max x y) else Nothing) s' r'
  in
    case newshape of
      Nothing -> Nothing
      Just ns -> Just (
        NdArray ns $ padRepeats ns m s' v', --NdArray ns $ 
        NdArray ns $ padRepeats ns m r' u') -- NdArray ns $ 
        where m = (fst $ mapIndicies ns)

-- makes the number of dimensions correct but not the size of the later 1s 
broadcastDimensions :: (DType a, DType b) => 
  [Integer] -> Vector a -> [Integer] -> Vector b -> 
    ([Integer], Vector a, [Integer], Vector b)
broadcastDimensions s v r u 
  | sl == rl = (s,v,
                r,u)
  |  sl > rl = (s,v,
                sdiff ++ r,
                V.concat $ replicate (fromIntegral $ product $ sdiff) u)
  |  sl < rl = (rdiff ++ s,
                V.concat $ replicate (fromIntegral $ product $ rdiff) v,
                r,u)
  where
    sl = length s
    rl = length r
    diff = Prelude.abs (sl - rl)
    sdiff = take diff s
    rdiff = take diff r

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

{-
identifyCommon :: forall a . Eq a => [[a]] -> Maybe [(Int, a)]
identifyCommon [] = Nothing
identifyCommon (x : xs) =
  let 
    n = length x
    indexed = traverse (\y -> if length y == n then Just (zip [(0::Int)..] y) else Nothing) (x:xs) :: Maybe [[(Int, a)]]
    common = fmap (foldr intersect (head $ fromJust indexed)) indexed
  in
    case common of
      Just c | length c < n-1 -> Nothing
      otherwise -> common
-}

-- Concatenate a list of tensors into a single tensor. All input tensors must have the
-- same shape, except for the dimension size of the axis to concatenate on.
concatAlong :: Int -> [NdArray] -> Maybe NdArray
concatAlong _ [] = Nothing
concatAlong _ [nd] = Just nd
concatAlong axis ((NdArray s v):nds) =
  case extractVectors ((NdArray s v):nds) (vecType v) of 
    Nothing -> Nothing
    Just vs -> 
      case concatAlongVec vs (map shape ((NdArray s v):nds)) axis of 
          Nothing -> Nothing
          Just (ns, c) -> Just $ NdArray ns c

{- Converts the dimension of each sub-array at the axis to a mapping from 
an index along this axis in the new array to the sub array it corresponds to.-}
--plotArrays :: [Integer] -> 
--plotArrays dims = 
--  concat $ zipWith (\arr dim -> [(arr, x) | x <- [0..dim]]) [0..] dims
  
--concatenateAlong nds axis = case identifyCommon shapes of 
{-
concatAlongVec :: forall a . DType a => [Vector a] -> [[Integer]] -> Int -> Maybe ([Integer], Vector a)
concatAlongVec vs shs axis = case identifyCommon shs of 
  Nothing -> Nothing
  Just c ->
      if 0 <= axis && axis < n
      then if (length c == n) || (length c == n-1 && not (elem axis (map fst c))) 
        then
          let
            concatIndicies = map (!! axis) shs -- axis can actually be concatenated along
            concatSize = sum concatIndicies 
            steps = [0] ++ scanl1 (+) concatIndicies
            ranges = zip steps (drop 1 steps)
            fullranges = map (\(x,y) -> [x..y-1]) ranges
            numbered = M.fromList $ concat $ zipWith (\x y -> map (\z-> (z, y)) x) fullranges [0..]
            newshape = replaceNth axis concatSize base 
            (oneDmap,_) = mapIndicies newshape
            multiMaps = map (\x -> snd $ mapIndicies x) shs
          in 
          --in Just newshape
            Just (newshape, V.generate (fromIntegral $ product newshape) (\i -> 
              let 
                multiI = oneDmap M.! i
                array = numbered M.! (multiI !! axis)
                arraySize = concatIndicies !! array
                arrayMultiI = replaceNth axis (multiI !! axis - (steps !! array)) multiI
                arrayFlatI = (multiMaps !! array) M.! arrayMultiI
              in
                --arrayFlatI
                (vs !! array) V.! arrayFlatI <-@ typeRep @(a)
              ) )

        else Nothing
      else Nothing
  where
    base = head shs
    n = length $ base
-}

concatAlongVec :: forall a . DType a => [Vector a] -> [[Integer]] -> Int -> Maybe ([Integer], Vector a)
concatAlongVec vs shs axis = 
  if not (checkShapeLengths shs) || not (checkAxis axis shs) then Nothing
  else 
    let 
      axDim = axisDimensions axis shs
      newshape = replaceNth axis (sum axDim) (head shs)
      -- first is sub-array number, second is sub-array index
      arrayPlot = concat $ zipWith (\arr dim -> [(arr, x) | x <- [0..dim-1]]) [0..] axDim 
      (newMultiInds, _) = mapIndicies newshape
      subArrayMaps = map (\x -> snd $ mapIndicies x) shs
    in 
      Just (newshape,
        V.generate (length newMultiInds) (\i ->
          let 
            multiI = newMultiInds M.! i
            (arrayNo, arrayAxInd) = arrayPlot !! (fromIntegral $ multiI !! axis)
            array = vs !! arrayNo
            arrayMap = subArrayMaps !! arrayNo
            arrayMultiI = replaceNth axis arrayAxInd multiI
          in 
            --arrayNo
            vecInd arrayMap array arrayMultiI <-@ typeRep @(a)
        ) 
      )

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n x l = take n l ++ [x] ++ drop (n+1) l

-- same number of dimensions
checkShapeLengths :: [[Integer]] -> Bool
checkShapeLengths [] = False
checkShapeLengths shapes = (filter (\sh -> length sh /= baseLen) shapes) == []
  where baseLen = length $ head shapes

-- dimensions are the same save perhaps the axis one
checkAxis :: Int -> [[Integer]] -> Bool
checkAxis _ [] = False
checkAxis axis shapes = 
  let 
    dropAxis = map (\sh -> take axis sh ++ drop (axis+1) sh) shapes
    base = head dropAxis
  in 0 <= axis && axis <= length base && (foldr intersect base dropAxis) == base

-- gets the size of the dimension of the axis over all the shapes
axisDimensions :: Int -> [[Integer]] -> [Integer]
axisDimensions axis shapes = map (!! axis) shapes

--ctest = concatAlongVec [V.fromList [1..6::Int], V.fromList [11..16::Int], V.fromList [101..108::Int]] [[2,3], [2,3], [2,4]] 1
--dtest = concatAlong [fromList [2,3] [1..6::Int], fromList [2,3] [11..16::Int], fromList [2,4] [101..108::Int]] 1

gather :: NdArray -> [Integer] -> Integer -> NdArray
gather nd is axis = fromJust $ concatAlong ax (map (\i -> slice (sliceLead ++ [(i,i)]) nd) is) 
  where
    ax = fromIntegral axis
    sliceLead = replicate ax (0,-1)
    --(m,_) = mapIndicies $ shape nd

{-
onnxex = fromMatrix $ A [
    A [B (1.0::Float), B 1.2, B 1.9],
    A [B 2.3, B 3.4, B 3.9],
    A [B 4.5, B 5.7, B 5.9]]

etest = gather onnxex [0,2] 1
-}

-- * Matrix Operations

-- ROWS, COLUMNS & DIAGONALS

{- | Switches the rows at the two given indicies over. 
NB: designed for 2x2 matricies so will only make swaps in the 'front' matrix of a tensor.
-}
swapRows :: Integer -> Integer -> NdArray -> NdArray
swapRows r1 r2 (NdArray s v)
  | r1 == r2 = (NdArray s v)
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
diagonalVec s v = 
  V.ifilter (\i _ -> i `mod` (rowLen+1) == 0 && i < rowLen*columns) v 
  where
    rowLen = fromIntegral @Integer @Int $ s!!(length s -1)
    columns = fromIntegral @Integer @Int $ s!!(length s -2)

-- TRANSPOSITION

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
        flatV = toV M.! (permuteList perm' multU)
      in v V.! flatV)

-- Applies a permutation to a list
permuteList :: [Int] -> [a] -> [a]
permuteList perm l = if sort perm /= [0 .. length l -1] 
  then error "Invalid permutation given."
  else map (l!!) perm

-- Finds the inverse of a permutation
invertPermutation :: [Int] -> [Int]
invertPermutation perm = map (\i -> fromJust $ elemIndex i perm) [0..length perm -1]

-- MULTIPLICATION

-- | Dot product over matricies of the same shape.
dot :: DType a => NdArray -> NdArray -> a
dot nd1 nd2 = foldrA (DType.add) (DType.addId) (nd1*nd2)

-- For now, just nxm and mxp = nxp
matMul :: NdArray -> NdArray -> NdArray
matMul (NdArray s v) (NdArray r u) =
  if (length s /= 2) || (length r /= 2) || s!!1 /= r!!0 then 
    error "Matricies must be 2D and the number of columns in the first must match the number of rows in the second."
  else case v =@= u of
    Just HRefl -> NdArray sh (matMulVec s v r u)
    _ -> error "Cannot multiply matricies of two distinct types."
  where
    sh = [s!!0, r!!1]

-- returning the vector result of the matMul
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

-- element at position [i,j] in the resultant nxp matrix (from matMultiplying a prev: mxn and pxm = pxn) 
--matMulElem :: DType a => 
--  NdArray -> M.Map [Integer] Int -> NdArray -> M.Map [Integer] Int -> [Integer] -> a
matMulElem :: forall a . DType a =>
  ([Integer] -> a) -> ([Integer] -> a) -> [Integer] -> [Integer] -> a
matMulElem map1 map2 ks (i:j:_) =
  foldr (\k acc -> DType.add acc $ DType.multiply (map1 [i,k]) (map2 [k,j])) DType.addId ks
matMulElem _ _ _ _ = DType.multId :: a

-- DETERMINANTS & INVERSES

-- | Converts a nxn matrix to upper triangle form. O(n^3).
upperTriangle :: NdArray -> NdArray
upperTriangle (NdArray [] v) = (NdArray [] v)
upperTriangle (NdArray (c:rs) v) = 
  let
    (_, fromMulti) = mapIndicies (c:rs)
    traversals = [(i,j,k) | i <- [0..c-1], j <- [i+1..c-1], k <- [0..c-1]]
  in 
    NdArray (c:rs) $ triangulateVec fromMulti v traversals (indentityElem' v)

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
  [_] -> [DType.addId :: a]
  [_,_] -> [determinant2D (NdArray s v)]
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
    [2,2] -> determinant2x2 nd
    -- nxn matricies are row-swapped to find an arrangement with no zeros/identity elements
    -- in the leading diagonal (pivots) then put into upper triangle form
    [c,r] | c == r && (not $ zeroRow nd) -> case swapRowsWith0Pivot nd of
            Just (NdArray s v) ->
              let
                upperTri = upperTriangle (NdArray s v)
                upperTriV = getVector upperTri :: Vector a 
                pivots = diagonalVec s upperTriV
              in
                -- determinant is the product of the pivots in upper triangle form
                V.foldr (DType.multiply) (DType.multId :: a) pivots
    -- If the matrix is non-square or has a zero-row/column, it is singular.
            Nothing -> DType.addId
    [_,_] -> DType.addId
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
    (not $ V.null v)        && 
    ((V.all (==ident) row)  || 
    (zeroRowVec r rest))

{- Repeatedly swaps rows until the matrix is found to be singular or
there are no pivots which are zero/identity elem. If singular, returns Nothing.
Note: hangs if given a matrix with a zero-row.
-}
swapRowsWith0Pivot :: NdArray -> Maybe NdArray
swapRowsWith0Pivot (NdArray s v) =
  let
    diag = diagonalVec s v
    ident = indentityElem' diag
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

-- NB if the matricies are integers the scalars will also become integers so you should convert the matricies first
gemm :: (DType a, DType b) => 
  NdArray -> NdArray -> NdArray -> Bool -> Bool -> a -> b -> 
    Maybe (NdArray, NdArray, NdArray, NdArray, NdArray, NdArray)
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
            --alphaAB = scale alpha' (dot (NdArray sAT vA') (NdArray sBT vB'))
            sAB = shape alphaAB
          in
            -- Check if C dimension matches or is broadcastable
            if (sC!!0 /= 1 && sC!!0 /= sAB!!0) || (sC!!1 /= 1 && sC!!1 /= sAB!!1) then Nothing
            else 
              let betaC = scale beta' $ if (sC!!0 /= sAB!!0) || (sC!!1 /= sAB!!1) 
                  then snd $ fromJust $ broadcast (alphaAB, NdArray sC vC')
                  else (NdArray sC vC')
              in 
                -- Finally, combine the two
                Just $ (alphaAB + betaC, alphaAB, betaC, (NdArray sAT vA'), (NdArray sBT vB'), matMul (NdArray sAT vA') (NdArray sBT vB'))

{-
Ok so we need to convert the scalars to whatever the matrix types are
and check matrix types all match
and check the a and b shapes are good
and possibly size c up
and scale the alphas & betas
-}

applyTransposition :: forall a . DType a => ([Integer], Vector a) -> Bool -> ([Integer], Vector a)
applyTransposition (s, v) b = 
  let 
    ndT = Numskull.transpose (NdArray s v)
    sT = shape ndT
    vT = (getVector ndT) :: Vector a 
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


-- * Common Errors 
shapeMismatch :: String -> String -> String
shapeMismatch s1 s2 = "Cannot match first array of shape '" <> s1 <> "' with array of shape '" <> s2 <> "'."

typeMismatch :: String -> String -> String
typeMismatch t1 t2 = "Cannot match first array of type '" <> t1 <> "' with array of type '" <> t2 <> "'."

ndt1 :: NdArray
ndt1 = fromList [3,2] [1,2,3,4,5,6::Int]
ndt2 :: NdArray
ndt2 = fromList [2,3] [0,2,4,6,8,10::Int]

nd3 = fromList [2,2] [1,2,3,4 :: Int]

nd4 = fromList [3,3] [2,5,1, 9,2,7, 4,16,3 ::Float] 
-- det = -71
{-
 1  0  0     2  5  1
9/2 1  0  x  0  s 5/2 
 2  t  1     0  0 71/41

t = -12/41
s = -41/2
-} 

