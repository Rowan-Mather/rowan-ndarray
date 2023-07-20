{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numskull where

import qualified DType
import DType (DType)
import MatrixForm

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import Type.Reflection

-- $setup
-- >>> import Numskull as N


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

-- * NdArray
-- Todo: Should shapes be [Integer] or [Int] or maybe even another vector?
-- | The core of this module. NdArrays can be of any type (a) and size/shape (list of dimensions) but these are
-- hidden by the type. Both attributes can be inferred using the library constructors (TODO!).
data NdArray where
  NdArray :: DType a => [Integer] -> Vector a -> NdArray

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
size shape = (fromIntegral $ product shape) :: Int

-- Todo: get the ident of the dtype from an nd array
indentityElem = undefined

-- Helper for the vectors in identityElem
indentityElem' :: forall a . DType a => Vector a -> a
indentityElem' _ = DType.identity :: DType a => a

-- | Creates an NdArray from a given shape and list. The number of elements must match.
fromList :: DType a => [Integer] -> [a] -> NdArray
fromList shape l = 
  if length l /= size shape then error "Length of the list should match the total number of elements specified by the shape."
  else NdArray shape (V.fromList l)

-- | Creates a 1xn NdArray from a list.
fromListFlat :: DType a => [a] -> NdArray
fromListFlat l = NdArray [fromIntegral$length l :: Integer] (V.fromList l)

{-| Creates an NdArray from an explicitly given matrix such as the example 2x3. -}
-- >>> m = A [A [B 1,  B 2],
-- >>>        A [B 3,  B 4],
-- >>>        A [B 5,  B 6]]
--
fromMatrix :: DType a => TreeMatrix a -> NdArray
fromMatrix m = NdArray (matrixShape m) (V.fromList l)
  where l = flattenToList $ matrixToTree m

-- | Creates a 1x1 matrix
singleton :: DType a => a -> NdArray
singleton x = NdArray [1] (V.fromList [x])

{- | Creates the smallest possible square matrix from the given list, 
padding out any required space with the identity element for the DType -}
squareArr = undefined

{- | Creates an array of the given shape of the identity element for the given type. -}
zeros = undefined
{-
zeros :: forall a . DType a => TypeRep a -> [Integer] -> NdArray
zeros t s = NdArray zerovec s
  where
    ident = (DType.identity :: DType a => a)
    zerovec = (V.replicate (size s) ident) :: DType a => Vector a
-}

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

-- | Converts a shape and multi-index to a 1D index.
collapseInd :: [Integer] -> [Integer] -> Integer
collapseInd shape indicies = collapseRun shape indicies 1

-- Helper for collapseInd
collapseRun :: [Integer] -> [Integer] -> Integer -> Integer
collapseRun _ [] _ = 0
collapseRun [] _ _ = 0
collapseRun (s:ss) (x:xs) runSize = x*runSize + collapseRun ss xs (s*runSize)

-- | Converts a shape and 1D index to a multi-index.
expandInd :: [Integer] -> Integer -> [Integer]
expandInd shape i = expandRun shape i 1

-- Helper for expandInd
expandRun :: [Integer] -> Integer -> Integer -> [Integer]
expandRun [] _ _ = []
expandRun (s:ss) i runSize = x : expandRun ss i (s*runSize)
  where x = (i `div` runSize) `mod` s

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
elemMultiply = pointwiseZip DType.multiply

-- Todo: Needs to operate on doubles
--elemDivide :: NdArray -> NdArray -> NdArray
--elemDivide = pointwiseZip divide

elemDiv :: NdArray -> NdArray -> NdArray
elemDiv = pointwiseZip DType.div

-- Todo: Needs to operate on doubles
--elemPower :: NdArray -> NdArray -> NdArray
--elemPower = pointwiseZip power

elemPow :: NdArray -> NdArray -> NdArray
elemPow = pointwiseZip DType.pow

-- | Type & Shape Conversion | --
-- Converting between the standard dtypes and changing the shapes of matricies

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

-- Check that the matrix isn't larger than the shape but if so truncate it
constrainSize :: DType a => [Integer] -> Vector a -> (Bool, Vector a)
constrainSize s v =
  if (size s) < len then (False, V.take (size s) v)
  else (True, v)
  where
    len = V.length v

-- Fill out any spaces in a vector smaller than the shape with 0s (or whatever the dtype 'identity' is)
padSize :: DType a => [Integer] -> Vector a -> Vector a
padSize s v = v V.++ V.replicate ((size s) - len) DType.identity
  where len = V.length v

-- Contrain or pad the vector to match the size
setSize :: DType a => [Integer] -> Vector a -> Vector a
setSize s v = let (unchanged, u) = constrainSize s v in
  if unchanged then padSize s u else u

-- Constrain or pad the NdArray to match the new given size
resize :: NdArray -> [Integer] -> NdArray
resize (NdArray _ v) r = NdArray r (setSize r v)

reshape :: NdArray -> [Integer] -> Maybe NdArray
reshape (NdArray s v) r = if product s == product r
  then Just $ NdArray r v
  else Nothing

--NB: reshape will pad/truncate individual dimensions whereas resize keeps as many values as possible but they might switch position
-- a matrix being reshaped must already match the size correctly
map1DIndex :: [Integer] -> [Integer] -> Integer -> Integer
map1DIndex s r i = collapseInd r (expandInd s i)

mapShapeLoss :: [Integer] -> [Integer] -> Bool
mapShapeLoss s r = 
  if length r < length s then True
  else or $ zipWith (>) s r

-- If you try to map to a smaller shape, values are dropped & weird stuff happens, otherwise padded

-- | Some helpful documentation
--
-- >>> padShape (Numskull.fromList [2] [3,4 :: Int]) [5]
-- [3,4,0,0,0][5]
--
padShape :: NdArray -> [Integer] -> NdArray
padShape (NdArray s v) r =
  let
    newSize = fromInteger @Int (product r)
    nullVec = V.replicate newSize (indentityElem' v)
    fi i = fromIntegral @Int @Integer i
    newIndices = V.imap (\i _ -> fromInteger @Int $ map1DIndex s r (fi i)) v
  in
    NdArray r (V.unsafeUpdate_ nullVec newIndices v)

-- | Common Errors | -- 
shapeMismatch :: String -> String -> String
shapeMismatch s1 s2 = "Cannot match first array of shape '" <> s1 <> "' with array of shape '" <> s2 <> "'."

typeMismatch :: String -> String -> String
typeMismatch t1 t2 = "Cannot match first array of type '" <> t1 <> "' with array of type '" <> t2 <> "'."