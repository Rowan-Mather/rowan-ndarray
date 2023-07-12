-- trust me bro ;) 
-- :set -fdefer-type-errors

{-# LANGUAGE GADTs #-}

module Numskull where

import Prelude as P
import Data.Maybe (isJust, fromJust)
import Data.Dynamic
import Type.Reflection
import Data.Vector as V

ty x = typeOf x

-- NdArray --
data NdArray where
  NdArray :: (Typeable a, DType a) => Vector a -> NdArray

instance Show NdArray where
    show (NdArray x) = show x 

-- To do: https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Num.html
instance Num NdArray where
    (NdArray x) + (NdArray y) = case (eqTypeRep xtype ytype, matchDType (NdArray x) (NdArray y)) of
        (Just HRefl, _)         -> NdArray (V.zipWith add x y) -- Types match
        (_, Just casted)        -> (NdArray x) + casted -- Second type can be converted to first
        otherwise               -> error ("Cannot convert second matrix of type '" P.++ show ytype P.++ "' to type '" P.++ show xtype P.++ "'.")
        where
            xtype = ty x
            ytype = ty y

    --(NdArray x) - (NDArray y) = 
    
    --(NdArray x) * (NDArray y)


-- Helper 
eqDType x y = case eqTypeRep (ty x) (ty y) of 
    Just HRefl -> True 
    otherwise -> False 

matchDType :: NdArray -> NdArray -> Maybe NdArray
matchDType (NdArray x) (NdArray y) = case eqTypeRep (ty x) (ty (fromList [1::Int])) of
    Just HRefl -> Just $ NdArray (V.map dtypeToInt y)
    otherwise -> Nothing


---- Testing

unwrapND :: NdArray -> (String, Vector Dynamic)
unwrapND (NdArray x) = case typeOf x of
    vecTypeInt      -> ("Int", V.map toDyn x)
    vecTypeBool     -> ("Bool", V.map toDyn x)

nd1 = NdArray (fromList [1,2,3::Int])
nd2 = NdArray (fromList [10,11,12::Int])