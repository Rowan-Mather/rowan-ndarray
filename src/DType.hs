module DType where 

-- Basis for all pointwise operations
class (Show a, Typeable a) => DType a where
  -- Numeric
  add :: a -> a -> a
  subtract :: a -> a -> a
  multiply :: a -> a -> a
  divide :: a -> a -> Double
  div :: a -> a -> a
  power :: a -> Double -> Double
  pow :: a -> a -> a
  log :: a -> a -> a
  mod :: a -> a -> Integer
  abs :: a -> a
  ceil :: a -> a
  floor :: a -> a
  -- Trig
  sin :: a -> a
  cos :: a -> a
  tan :: a -> a
  -- Logical 
  invert :: a -> a
  shiftleft :: a -> a
  shiftright :: a -> a
  -- Comparative
  eq :: a -> a -> Bool
  leq :: a -> a -> Bool
  geq :: a -> a -> Bool
  less :: a -> a -> Bool
  greater :: a -> a -> Bool
  -- Standard Conversions
  dtypeToInt :: a -> Int
  dtypeToFloat :: a -> Float
  dtypeToDouble :: a -> Double
  dtypeToBool :: a -> Bool


instance DType Int where
    add x y = x + y
    multiply x y = x * y
    divide x y = 
    eq x y = x == y

--instance DType Integer where?

--instance DType Float where?

--instance DType Double

--instance DType Bool where

--instance DType Char where?

--instance ByteString where?

--instance DType BFloat16 where?