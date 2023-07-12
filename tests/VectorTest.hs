-- trust me bro ;) 
-- :set -fdefer-type-errors

{-# LANGUAGE GADTs #-}

module VectorTest where

import Prelude as P
import Data.Vector as V
import Data.Dynamic
import Type.Reflection
import Data.Maybe (isJust, fromJust)

ty x = typeOf x

-- DType --
class (Show a, Typeable a) => DType a where
  add :: a -> a -> a
  subtract :: a -> a -> a
  multiply :: a -> a -> a
  eq :: a -> a -> Bool
  dtypeToInt :: a -> Int

instance DType Int where
    add x y = x + y
    multiply x y = x * y
    eq x y = x == y

-- NdArray --
data NdArray where
  NdArray :: (Typeable a, DType a) => Vector a -> NdArray

instance Show NdArray where
    show (NdArray x) = show x 

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




-- Spaghetti
{-
instance Num NdArray where
    (NdArray x) + (NdArray y) = 
        case typeOf x `eqTypeRep` typeOf y of 
            Just HRefl -> NdArray (V.zipWith add x y) -- Types match
            Nothing -> case casted of Just newNd -> (V.zipWith add x newNd)
        where casted = matchDType (NdArray x) (NdArray y)
-}



---- Testing

unwrapND :: NdArray -> (String, Vector Dynamic)
unwrapND (NdArray x) = case typeOf x of
    vecTypeInt      -> ("Int", V.map toDyn x)
    vecTypeBool     -> ("Bool", V.map toDyn x)

nd1 = NdArray (fromList [1,2,3::Int])
nd2 = NdArray (fromList [10,11,12::Int])

