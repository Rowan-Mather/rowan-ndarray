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
    | CreationSize Integer [Integer]
    | TypeMismatch String
    | ExceededShape Integer [Integer]

instance Exception NdArrayException

instance Show NdArrayException where
    show (DTypeMismatch (NdArray _ v) (NdArray _ u) extra) = 
        if extra == "" then 
            "Cannot match NdArrays of type '" <> showType v <> 
                "' and type '" <> showType u <> "'."
        else 
            "Cannot perform " <> extra <> " on mismatching NdArrays of type '" <> showType v <> 
                "' and type '" <> showType u <> "'."
    
    show (ShapeMismatch (NdArray s _) (NdArray r _) extra) =
        if extra == "" then 
            "Cannot match NdArrays of shape " <> show s <> 
                " and shape " <> show r <> "."
        else 
            "Cannot perform " <> extra <> " on mismatching NdArrays of shape " <> show s <> 
                " and shape " <> show r <> "."

    show (CreationSize sz sh) =
        "Cannot create array of size " <> show sz <> " and shape " <> show sh <> "."

    show (TypeMismatch str) = str 

    show (ExceededShape dim sh) = 
        "Cannot index into dimension " <> show dim <> "in NdArray of shape " <> show sh <> "."

-- Returns the string type of vector elements.
showType :: forall a . DType a => Vector a -> String
showType _ = show (typeRep @a)
