module DenseTest where

import Dense

class Typeable a => Dtype a where
  add :: a -> a -> a
  mult :: a -> a -> a
  eq :: a -> a -> Bool
  ...

data NdArray where
  NdArray :: Dtype a => Array DynIx a -> NdArray