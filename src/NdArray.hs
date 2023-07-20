{-# LANGUAGE GADTs #-}

module NdArray where

import DType
import Data.Vector.Storable

-- * NdArray
-- Todo: Should shapes be [Integer] or [Int] or maybe even another vector?
-- | The core of this module. NdArrays can be of any type (a) and size/shape (list of dimensions) but these are
-- hidden by the type. Both attributes can be inferred using the library constructors (TODO!).
data NdArray where
  NdArray :: DType a => [Integer] -> Vector a -> NdArray