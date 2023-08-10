{-# LANGUAGE GADTs #-}

module NdArray where

import DType
import Data.Vector.Storable

-- * NdArray
-- | The core of this module. NdArrays can be of any DType a and size/shape (list of dimensions)
-- These are hidden by the type. 
data NdArray where
  -- shape stride elements
  NdArray :: DType a => Vector Int -> Vector Int -> Vector a -> NdArray

-- | By default arrays are printed flat with the shape as metadata. 
-- For a tidier representation, use printArray.
instance Show NdArray where
  show (NdArray sh st v) = "{elements: " <> show v <> ", shape: " <> show sh <> ", stride: " <> show st <> "}"