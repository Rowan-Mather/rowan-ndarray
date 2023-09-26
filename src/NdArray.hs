{-# LANGUAGE GADTs #-}

module NdArray where

import DType
import Data.Vector.Storable
import Control.DeepSeq (NFData (rnf), deepseq)

-- * NdArray
-- | The core of this module. NdArrays can be of any DType a and size/shape (list of dimensions)
-- These are hidden by the type. 
data NdArray where
  NdArray :: DType a => [Integer] -> Vector a -> NdArray

-- | By default arrays are printed flat with the shape as metadata. 
-- For a tidier representation, use printArray.
instance Show NdArray where
  show (NdArray s v) = "{elements: " <> show v <> ", shape: " <> show s <> "}"

instance NFData NdArray where
  rnf (NdArray s v) = s `deepseq` v `deepseq` ()
