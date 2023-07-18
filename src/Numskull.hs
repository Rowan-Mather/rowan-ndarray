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

-- $setup
-- >>> import Numskull as N


-- * Typing Shorthand 
-- | typeOf synonym.
ty :: Typeable a => a -> TypeRep a
ty = typeOf

-- | eqTypeRep synonym, returning Just HRefl in the case of type equality.
(=@=) :: (Typeable a, Typeable b) => a -> b -> Maybe (a :~~: b)
(=@=) v u = eqTypeRep (ty v) (ty u)

-- * NdArray
-- Todo: Should shapes be [Integer] or [Int] or maybe even another vector?
-- | The core of this module. NdArrays can be of any type (a) and size/shape (list of dimensions) but these are
-- hidden by the type. Both attributes can be inferred using the library constructors (TODO!).
data NdArray where
  NdArray :: DType a => [Integer] -> Vector a -> NdArray

fromList :: DType a => [Integer] -> [a] -> NdArray
fromList shape content = NdArray shape (V.fromList content)

-- Todo: show in a nicer shapely form :)
instance Show NdArray where
  show (NdArray s v) = show s <> show v

instance Eq NdArray where
  (NdArray s v) == (NdArray r u) = (r == s) && 
    case v =@= u of
      Just HRefl -> v == u
      Nothing    -> False
  (NdArray s v) /= (NdArray r u) = (r /= s) || 
    case v =@= u of
      Just HRefl -> v /= u
      Nothing    -> True

-- Investigate how Vector implements further
-- Todo: check max and min work properly on all dtypes, probably use a map instead
instance Ord NdArray where
  (NdArray s v) `compare` (NdArray r u) = if s == r then case v =@= u of
      Just HRefl -> compare v u
      Nothing    -> error $ typeMismatch (show v) (show u)
    else error $ shapeMismatch (show s) (show r)
  
  (NdArray s v) <= (NdArray r u) = if s == r then case v =@= u of
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
  negate (NdArray s v) = NdArray s (V.map DType.invert v)
  abs (NdArray v s) = NdArray s (V.map DType.abs v)
  signum (NdArray v s) = NdArray s (V.map DType.signum v)
  fromInteger x = NdArray [1] (singleton $ fromInteger @Int x)

size :: [Integer] -> Int
size shape = (fromIntegral $ P.product shape) :: Int

-- | Creation | --
-- Todo: get the ident of the dtype from an nd array
indentityElem = undefined

indentityElem' :: forall a . DType a => Vector a -> a
indentityElem' _ = DType.identity :: DType a => a

-- Todo: Create ident array of certain shape
{-
zeros :: forall a . DType a => TypeRep a -> [Integer] -> NdArray
zeros t s = NdArray zerovec s
  where
    ident = (DType.identity :: DType a => a)
    zerovec = (V.replicate (size s) ident) :: DType a => Vector a
-}

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
-- Fun things with forall http://hoogle.jeeves.myrtle/file/nix/store/3pyqd7gjlxwj7wfrcsr0w1rjvc0qyl8r-clash-extra-0.1.0.0-doc/share/doc/clash-extra-0.1.0.0/html/Data-SNat.html#v:reifySNat
-- https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Monad-ST.html#v:runST
pointwiseZip :: (forall t . DType t => t -> t -> t) -> NdArray -> NdArray -> NdArray
pointwiseZip zipfunc (NdArray s v) (NdArray r u) = if s == r then 
  case v =@= u of
    Just HRefl -> NdArray s (V.zipWith zipfunc v u) -- Types match
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
matchDType (NdArray _ v) (NdArray r u) = case v =@= V.fromList [1::Int] of
  Just HRefl  -> Just $ NdArray r (V.map dtypeToInt u)
  _           -> Nothing

-- Check that the matrix isn't larger than the shape but if so truncate it
constrainSize :: DType a => [Integer] -> Vector a -> (Bool, Vector a)
constrainSize s v =
  if (size s) < len then (False, V.take (size s) v)
  else (True, v)
  where
    len = V.length v

-- Fill out any spaces in a vector smaller than the shape with 0s (or whatever the dtype 'identity' is)
padSize :: DType a => [Integer] -> Vector a -> Vector a
padSize s v = v V.++ V.replicate ((size s) - len) identity
  where len = V.length v

-- Contrain or pad the vector to match the size
setSize :: DType a => [Integer] -> Vector a -> Vector a
setSize s v = let (unchanged, u) = constrainSize s v in
  if unchanged then padSize s u else u

-- Constrain or pad the NdArray to match the new given size
resize :: NdArray -> [Integer] -> NdArray
resize (NdArray _ v) r = NdArray (setSize r v) r

reshape :: NdArray -> [Integer] -> Maybe NdArray
reshape (NdArray s v) r = if P.product s == P.product r
  then Just $ NdArray r v
  else Nothing

--NB: reshape will pad/truncate individual dimensions whereas resize keeps as many values as possible but they might switch position
-- a matrix being reshaped must already match the size correctly
map1DIndex :: [Integer] -> [Integer] -> Integer -> Integer
map1DIndex s r i = collapseInd r (expandInd s i)

mapShapeLoss :: [Integer] -> [Integer] -> Bool
mapShapeLoss s r = 
  if P.length r < P.length s then True
  else P.or $ P.zipWith (>) s r

-- If you try to map to a smaller shape, values are dropped & weird stuff happens, otherwise padded

-- | Some helpful documentation
--
-- >>> padShape (Numskull.fromList [2] [3,4 :: Int]) [5]
-- [3,4,0,0,0][5]
--
padShape :: NdArray -> [Integer] -> NdArray
padShape (NdArray s v) r =
  let
    newSize = fromInteger @Int (P.product r)
    nullVec = V.replicate newSize (indentityElem' v)
    fi i = fromIntegral @Int @Integer i
    newIndices = imap (\i _ -> fromInteger @Int $ map1DIndex s r (fi i)) v
  in
    NdArray (unsafeUpdate_ nullVec newIndices v) r


-- | Common Errors | -- 
shapeMismatch :: String -> String -> String
shapeMismatch s1 s2 = "Cannot match first array of shape '" <> s1 <> "' with array of shape '" <> s2 <> "'."

typeMismatch :: String -> String -> String
typeMismatch t1 t2 = "Cannot match first array of type '" <> t1 <> "' with array of type '" <> t2 <> "'."