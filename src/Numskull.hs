{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Numskull where

import NdArray
import qualified DType
import DType (DType)
import MatrixForm

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import Type.Reflection
import qualified Data.Map as M
import Data.Maybe (fromJust)

-- $setup
-- >>> import Numskull as N
-- >>> import qualified Vector


-- * Typing Shorthand 
-- | typeOf synonym.
ty :: Typeable a => a -> TypeRep a
ty = typeOf

-- | eqTypeRep synonym, returning Just HRefl in the case of type equality.
-- >>> case True =@= False of
-- >>>  Just HRefl -> putStrLn "Two Booleans will match"
-- >>>  Nothing    -> putStrLn "Mismatching types"
-- Two Booleans will match
(=@=) :: (Typeable a, Typeable b) => a -> b -> Maybe (a :~~: b)
(=@=) v u = eqTypeRep (ty v) (ty u)

-- Todo: show in a nicer shapely form :)
instance Show NdArray where
  show (NdArray s v) = show s <> " " <> show v

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

-- To do: matrix multiplication :O
-- To do: change fromInteger to return an integer array rather than int
instance Num NdArray where
  -- | Adds elements pointwise
  (+) = pointwiseZip DType.add
  -- | Subtracts elements pointwise
  (-) = pointwiseZip DType.subtract
  -- | Matrix multiplication TODO
  (*) = undefined
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

-- | Gets the TypeRep for the NdArray elements
ndType :: forall a . DType a => NdArray -> TypeRep a
ndType (NdArray _ v) = case v =@= (undefined :: Vector a) of 
  Just HRefl -> vecType v :: TypeRep a
  _ -> error "Impossible type mismatch."

-- Helper to get the vector typeRep
vecType :: forall a . DType a => Vector a -> TypeRep a
vecType _ = typeRep @a

-- Todo: get the ident of the dtype from an nd array
indentityElem = undefined

-- Helper for the vectors in identityElem
indentityElem' :: forall a . DType a => Vector a -> a
indentityElem' _ = DType.identity :: DType a => a

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

-- | Creates a 1x1 matrix
-- >>> printArray $ singleton (3::Int)
-- 3
singleton :: DType a => a -> NdArray
singleton x = NdArray [1] (V.fromList [x])

arange = undefined

{- | Creates the smallest possible square matrix from the given list, 
padding out any required space with the identity element for the DType -}
squareArr = undefined

{- | Creates an array of the given shape of the identity element for the given type. -}
zeros :: forall a . DType a => TypeRep a -> [Integer] -> NdArray
zeros _ s = NdArray s zerovec
  where
    ident = (DType.identity :: DType a => a)
    zerovec = (V.replicate (size s) ident) :: DType a => Vector a


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
  Nothing -> DType.identity :: DType a => a

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
    positives = zipWith (\x y -> if y < 0 then x+y else y) s i
    flatInd = fromIntegral $ collapseInd s positives :: Int
  in
    -- The type comparison should always hold
    if validIndex (NdArray s v) i then
      case ty v `eqTypeRep` typeRep @(Vector a) of
        Just HRefl -> Just (v V.! flatInd) :: Maybe a -- Indexing the vector
        Nothing -> Nothing
    else Nothing

-- Todo: slicing

-- * Pointwise Functions  -- 
-- All the numpy-like functions not defined within the Eq, Ord or Num instances

----- One Argument

mapA :: forall a . forall b . (DType a, DType b) => (a -> b) -> NdArray -> NdArray
mapA f (NdArray s v) = case v =@= (undefined :: Vector a) of 
  Just HRefl -> NdArray s (V.map f v)
  _ -> error "Function input does not match array type."

--mapA :: (forall a . forall b . (DType a, DType b) => a -> b) -> NdArray -> NdArray
--mapA f (NdArray s v) = NdArray s (V.map f v)

mapTransform :: (forall a . DType a => a -> a) -> NdArray -> NdArray
mapTransform f (NdArray s v) = NdArray s (V.map f v)

scale :: forall a . DType a => a -> NdArray -> NdArray
scale x = mapA (DType.multiply x)

abs :: NdArray -> NdArray
abs = mapTransform (DType.abs)

signum :: NdArray -> NdArray
signum = mapTransform (DType.signum)

ceil :: NdArray -> NdArray
ceil = mapTransform (DType.ceil)

floor :: NdArray -> NdArray
floor = mapTransform (DType.floor)

sin :: NdArray -> NdArray
sin = mapTransform (DType.sin)

cos :: NdArray -> NdArray
cos = mapTransform (DType.cos)

tan :: NdArray -> NdArray
tan = mapTransform (DType.tan)

invert :: NdArray -> NdArray
invert = mapTransform (DType.invert)

shiftleft :: NdArray -> NdArray
shiftleft = mapTransform (DType.shiftleft)

shiftright :: NdArray -> NdArray
shiftright = mapTransform (DType.shiftright)

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

-- | Pointwise multiplication
elemMultiply :: NdArray -> NdArray -> NdArray
elemMultiply = pointwiseZip DType.multiply

-- Todo: Needs to operate on doubles
--elemDivide :: NdArray -> NdArray -> NdArray
--elemDivide = pointwiseZip divide

-- | Pointwise integer division
elemDiv :: NdArray -> NdArray -> NdArray
elemDiv = pointwiseZip DType.div

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
convertDTFromTo t1 t2 (NdArray s v) = case v =@= (undefined :: Vector a) of
  Just HRefl -> NdArray s (V.map convert v)
  _ -> error "Impossible type mismatch."
  where
    convert :: (DType a, DType b) => a -> b
    convert x = DType.rationalToDtype (DType.dtypeToRational x)

-- | Converts the second NdArray to be the same DType as the first.
matchDType :: NdArray -> NdArray -> NdArray
matchDType (NdArray _ v) nd = convertDTypeTo (vecType v) nd 

{-
-- | Converts the second NdArray to be the same DType as the first.
matchDType :: NdArray -> NdArray -> NdArray
matchDType (NdArray _ v) (NdArray r u) = NdArray r (V.map convert u)
  where 
    convert x = case (V.singleton x) =@= u of 
      Just HRefl -> DType.rationalToDtype (DType.dtypeToRational x) :: a
      _ -> error "Impossible type mismatch."
-}
{-
-- Todo extract the type then call this fnction to effectively pattern match on the type
matchDType' :: forall a . forall b . NdArray -> TypeRep a -> NdArray -> TypeRep b -> NdArray
matchDType' (NdArray _ v) t1 (NdArray r u) t2 = NdArray r (V.map convert u)
  where
    convert x = case eqTypeRep (ty v) t1 of
      Just HRefl -> DType.rationalToDtype (DType.dtypeToRational x) :: b
      _ -> error "Impossible type mismatch."
-}
    --case x =@= (undefined :: a) of
    --  Just HRefl -> 
    --  _ -> error "Impossible type mismatch."

-- To do: add many more possible types you can convert to
-- Use the TypeApplications syntax: 
-- case typeOf x `eqTypeRep` typeRep @Integer of 
-- TODO USING dtypetorational
{-
matchDType :: NdArray -> NdArray -> Maybe NdArray
matchDType (NdArray _ v) (NdArray r u) = case v =@= V.fromList [1::Int] of
  Just HRefl  -> Just $ NdArray r (V.map dtypeToInt u)
  _           -> Nothing
-}

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
padSize s v = v V.++ V.replicate ((fromIntegral s ::Int) - len) DType.identity
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

-- * Matrix Operations

-- For now, just nxm and mxp = nxp
matMul :: NdArray -> NdArray -> NdArray
matMul (NdArray s v) (NdArray r u) =
  if (length s /= 2) || (length r /= 2) || s!!1 /= r!!0 then 
    error "Invalid matrix dimensions."
  else case v =@= u of
    Just HRefl -> NdArray sh (matMulVec s v r u)
    _ -> error "Mismatching types"
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
matMulElem :: DType a =>
  ([Integer] -> a) -> ([Integer] -> a) -> [Integer] -> [Integer] -> a
matMulElem map1 map2 ks (i:j:_) =
  foldr (\k acc -> DType.add acc $ DType.multiply (map1 [i,k]) (map2 [k,j])) DType.identity ks
    --sum [DType.multiply (nd1>![i,k]) (nd2>![k,j]) | [k <- 1..m]]


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
