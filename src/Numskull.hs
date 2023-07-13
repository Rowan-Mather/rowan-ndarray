-- trust me bro ;) 
-- :set -fdefer-type-errors

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Numskull where

import Prelude as P
import Data.Dynamic
import Type.Reflection
---import Data.Vector as V
import Data.Vector.Storable as V
import DType

-- Typing shorthand
ty :: Typeable a => a -> TypeRep a
ty x = typeOf x

-- NdArray --
data NdArray where
  NdArray :: (Typeable a, DType a, Storable a) => Vector a -> NdArray

-- Todo: show in a nicer shapely form :)
instance Show NdArray where
  show (NdArray x) = show x 

-- Todo: check shapes are also equal
instance Eq NdArray where
  (NdArray x) == (NdArray y) = x == y
  (NdArray x) /= (NdArray y) = x /= y

-- Todo: should we have some notion of LEQ for a smaller size vector?
-- Investigate how Vector implements further
instance Ord NdArray where
  compare (NdArray x) (NdArray y)   = compare x y
  (<) (NdArray x) (NdArray y)       = x < y 
  (<=) (NdArray x) (NdArray y)      = x <= y
  (>) (NdArray x) (NdArray y)       = x > y
  (>=) (NdArray x) (NdArray y)      = x >= y
  max (NdArray x)                   = max x
  min (NdArray x)                   = min x

-- To do: matrix multiplication :O
instance Num NdArray where
  (+) = pointwiseZip add
  (-) = pointwiseZip subtract
  (*) = undefined -- Matrix multiplication
  negate = invert
  abs = abs
  signum = signum
  fromInteger x = NdArray (fromList [x])

-- Pointwise Functions
  -- All the numpy-like functions not defined within the Eq, Ord or Num instances
  
  -- Single Argument
  
  -- To do ;)

  -- Two Arguments
  pointwiseZip :: (forall t . DType t => t -> t -> t) -> NdArray -> NdArray -> NdArray
  pointwiseZip zipfunc (NdArray x) (NdArray y) = 
    case eqTypeRep (ty x) (ty y) of
      Just HRefl -> NdArray (V.zipWith zipfunc x y) -- Types match
      Nothing    -> error ("Cannot match second matrix of type '" P.++ show (ty y) P.++ "' to type '" P.++ show (ty x) P.++ "'.")
  
  elemMultiply :: NdArray -> NdArray -> NdArray
  elemMultiply = pointwiseZip multiply
  
  elemDivide :: NdArray -> NdArray -> NdArray
  elemDivide = pointwiseZip divide
  
  elemDiv :: NdArray -> NdArray -> NdArray
  elemDiv = pointwiseZip div
  
  elemPower :: NdArray -> NdArray -> NdArray
  elemPower = pointwiseZip power
  
  elemPow :: NdArray -> NdArray -> NdArray
  elemPow = pointwiseZip pow
  
-- Type & Shape Conversion
  -- Converting between the standard dtypes and changing the shapes of matricies
  
  -- To do: add many more possible types you can convert to
  -- Use the TypeApplications syntax: 
  -- case typeOf x `eqTypeRep` typeRep @Integer of 
  matchDType :: NdArray -> NdArray -> Maybe NdArray
  matchDType (NdArray x) (NdArray y) = case eqTypeRep (ty x) (ty (fromList [1::Int])) of
    Just HRefl  -> Just $ NdArray (V.map dtypeToInt y)
    _           -> Nothing









---- Testing

-- Helper trying to simplify the type checking....
eqDType :: (Typeable a, Typeable b) => a -> b -> Bool
eqDType x y = case eqTypeRep (ty x) (ty y) of 
  Just HRefl  -> True 
   _           -> False 

{- Pointwise zip with type conversion (TODO)
pointwiseZip (NdArray x) (NdArray y) = case (eqTypeRep xtype ytype, matchDType (NdArray x) (NdArray y)) of
        (Just HRefl, _)     -> NdArray (V.zipWith add x y) -- Types match
        -- Code to auto-cast types
        --(_, Just casted)    -> (NdArray x) + casted -- Second type can be converted to first
        _                   -> error ("Cannot match second matrix of type '" P.++ show ytype P.++ "' to type '" P.++ show xtype P.++ "'.")
        where
            xtype = ty x; ytype = ty y
-}

{-
unwrapND :: NdArray -> (String, Vector Dynamic)
unwrapND (NdArray x) = case typeOf x of
    vecTypeInt      -> ("Int", V.map toDyn x)
    vecTypeBool     -> ("Bool", V.map toDyn x)
-}

nd1 :: NdArray; nd2 :: NdArray
nd1 = NdArray (fromList [1,2,3::Int]); nd2 = NdArray (fromList [10,11,12::Int])