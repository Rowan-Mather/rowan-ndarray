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

ty :: Typeable a => a -> TypeRep a
ty x = typeOf x -- Shorthand

-- NdArray --
data NdArray where
  NdArray :: (Typeable a, DType a, Storable a) => Vector a -> NdArray

instance Show NdArray where
  show (NdArray x) = show x 

instance Num NdArray where
    (NdArray x) + (NdArray y) = case (eqTypeRep xtype ytype, matchDType (NdArray x) (NdArray y)) of
        (Just HRefl, _)     -> NdArray (V.zipWith add x y) -- Types match
        _                   -> error ("Cannot match second matrix of type '" P.++ show ytype P.++ "' to type '" P.++ show xtype P.++ "'.")
        where
            xtype = ty x; ytype = ty y

    --x - y = pointwiseZip DType.subtract x y 
    --(NdArray x) * (NDArray y)
    -- To do: https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Num.html


-- Helper 
eqDType :: (Typeable a, Typeable b) => a -> b -> Bool
eqDType x y = case eqTypeRep (ty x) (ty y) of 
    Just HRefl  -> True 
    _           -> False 

-- case typeOf x `eqTypeRep` typeRep @Integer of NOTE THIS IS INT SYNTAX
matchDType :: NdArray -> NdArray -> Maybe NdArray
matchDType (NdArray x) (NdArray y) = case eqTypeRep (ty x) (ty (fromList [1::Int])) of
    Just HRefl  -> Just $ NdArray (V.map dtypeToInt y)
    _           -> Nothing

-- Pointwise Operations not defined in Num

{- Pointwise zip with type conversion (TODO)
pointwiseZip (NdArray x) (NdArray y) = case (eqTypeRep xtype ytype, matchDType (NdArray x) (NdArray y)) of
        (Just HRefl, _)     -> NdArray (V.zipWith add x y) -- Types match
        -- Code to auto-cast types
        --(_, Just casted)    -> (NdArray x) + casted -- Second type can be converted to first
        _                   -> error ("Cannot match second matrix of type '" P.++ show ytype P.++ "' to type '" P.++ show xtype P.++ "'.")
        where
            xtype = ty x; ytype = ty y
-}

pointwiseZip :: (forall t . DType t => t -> t -> t) -> NdArray -> NdArray -> NdArray
pointwiseZip zipfunc (NdArray x) (NdArray y) = case eqTypeRep (ty x) (ty y) of
        Just HRefl -> NdArray (V.zipWith zipfunc x y) -- Types match
        Nothing    -> error ("Cannot match second matrix of type '" P.++ show ytype P.++ "' to type '" P.++ show xtype P.++ "'.")
        where
            xtype = ty x; ytype = ty y

elemMultiply x y = pointwiseZip multiply x y










---- Testing

{-
unwrapND :: NdArray -> (String, Vector Dynamic)
unwrapND (NdArray x) = case typeOf x of
    vecTypeInt      -> ("Int", V.map toDyn x)
    vecTypeBool     -> ("Bool", V.map toDyn x)
-}

nd1 :: NdArray; nd2 :: NdArray
nd1 = NdArray (fromList [1,2,3::Int]); nd2 = NdArray (fromList [10,11,12::Int])