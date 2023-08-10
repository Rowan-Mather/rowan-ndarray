{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NdArrayException where

import Control.Exception
import Type.Reflection
import Data.Vector.Storable (Vector)

import DType
import NdArray

-- | The main type of exception thrown from Numskull functions when the user
-- tries to perform illegal operations given the size and shape of the array. 
data NdArrayException 
    = DTypeMismatch NdArray NdArray String
    | ShapeMismatch NdArray NdArray String
    | CreationSize Int (Vector Int)
    | TypeMismatch String
    | ExceededShape Int (Vector Int)
    | NotBroadcastable NdArray NdArray String

instance Exception NdArrayException

instance Show NdArrayException where
    show (DTypeMismatch (NdArray _ _ v) (NdArray _ _ u) extra) = 
        if extra == "" then 
            "Cannot match NdArrays of type '" <> showType v <> 
                "' and type '" <> showType u <> "'."
        else 
            "Cannot perform " <> extra <> " on mismatching NdArrays of type '" <> showType v <> 
                "' and type '" <> showType u <> "'."
    
    show (ShapeMismatch (NdArray s t _) (NdArray r d _) extra) =
        if extra == "" then 
            "Cannot match NdArrays of shape " <> show s <> " and stride " <> show t <>
                ", and shape " <> show r <> "and stride " <> show d <> "."
        else 
            "Cannot perform " <> extra <> " on mismatching NdArrays of shape " <> 
                show s <> " and stride " <> show t <>
                " and shape " <> show r <> "and stride " <> show d <> "."

    show (CreationSize sz sh) =
        "Cannot create array of size " <> show sz <> " and shape " <> show sh <> "."

    show (TypeMismatch str) = str 

    show (ExceededShape dim sh) = 
        "Cannot index into dimension " <> show dim <> "in NdArray of shape " <> show sh <> "."

    show (NotBroadcastable (NdArray s _ _) (NdArray r _ _) str) = 
        "Cannot broadcast NdArrays of shape " <> show s <> "and shape" <> show r <> str <> "."

-- Returns the string type of vector elements.
showType :: forall a . DType a => Vector a -> String
showType _ = show (typeRep @a)