module DType where 

import Prelude as P
import Type.Reflection
import GHC.Float (float2Double)

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
  --dtypeToFloat :: a -> Float
  --dtypeToDouble :: a -> Double
  --dtypeToBool :: a -> Bool

{- 
--INSTANCE TEMPLATE-- 
instance DType TYPE where 
    -- Numeric
  add x y = 
  subtract x y = 
  multiply x y = 
  divide x y = double
  div x y = 
  power x d = double
  pow x y = 
  log x y = 
  mod x y = integer
  abs x = 
  ceil x = 
  floor x = 
  -- Trig
  sin x = 
  cos x =
  tan x = 
  -- Logical 
  invert x = 
  shiftleft x = 
  shiftright x = 
  -- Comparative
  eq x y = 
  leq x y = 
  geq x y = 
  less x y = 
  greater x y = 
  -- Standard Conversions
  dtypeToInt x = 
  dtypeToFloat x = 
  dtypeToDouble x = 
  dtypeToBool x = 
-}

instance DType Int where 
  -- Numeric
  add x y = x + y 
  subtract x y = x - y
  multiply x y = x * y
  divide x y = fromIntegral x / fromIntegral y
  div = P.div
  power x d = fromIntegral x ** d
  pow x y = x ^ y
  log x y = P.floor $ logBase (fromIntegral x) (fromIntegral y) :: Int
  mod x y = fromIntegral (x `P.mod` y) :: Integer
  abs = P.abs
  ceil x = x
  floor x = x
  -- Trig
  sin x = (round $ P.sin $ fromIntegral x) :: Int
  cos x = (round $ P.cos $ fromIntegral x) :: Int
  tan x = (round $ P.tan $ fromIntegral x) :: Int
  -- Logical 
  invert x = -x
  shiftleft x = x * 2
  shiftright x = x `P.div` 2
  -- Comparative
  eq x y = x == y
  leq x y = x <= y
  geq x y = x >= y
  less x y = x < y
  greater x y = x > y
  -- (Conversions)
  dtypeToInt x = x


--instance DType Int64 where?

instance DType Float where 
  -- Numeric
  add x y = x + y 
  subtract x y = x - y
  multiply x y = x * y
  divide x y = float2Double (x / y)
  div x y = fromIntegral (P.floor x `P.div` P.floor y) :: Float
  power x d = float2Double x ** d
  pow x y = x ** y
  log x y = logBase x y
  mod x y = fromIntegral (P.floor x `P.mod` P.floor y) :: Integer
  abs = P.abs
  ceil x = (fromIntegral $ P.ceiling x) :: Float 
  floor x = (fromIntegral $ P.floor x) :: Float
  -- Trig
  sin = P.sin
  cos = P.cos
  tan = P.tan
  -- Logical 
  invert x = -x
  shiftleft x = x * 2
  shiftright x = x / 2
  -- Comparative
  eq x y = x == y
  leq x y = x <= y
  geq x y = x >= y
  less x y = x < y
  greater x y = x > y
  -- (Conversions)
  dtypeToInt x = (P.floor x) :: Int

--instance DType Double

instance DType Bool where 
    -- Numeric
  add x y = x || y
  subtract x y = toEnum (fromEnum x - fromEnum y)
  multiply x y = x && y
  divide _x _y = undefined
  div x y = (x || y) && not (x && y)
  power _x _d = undefined
  pow x y = toEnum (fromEnum x ^ fromEnum y)
  log _x _y = undefined
  mod x y = fromIntegral (fromEnum x `P.mod` fromEnum y) :: Integer
  abs _ = True
  ceil x = x
  floor x = x
  -- Trig
  sin x = toEnum (round $ P.sin $ fromIntegral $ fromEnum x) :: Bool
  cos x = toEnum (round $ P.cos $ fromIntegral $ fromEnum x) :: Bool
  tan x = toEnum (round $ P.tan $ fromIntegral $ fromEnum x) :: Bool
  -- Logical 
  invert x = not x
  shiftleft _ = False
  shiftright _ = False
  -- Comparative
  eq x y = x == y
  leq x y = x <= y
  geq x y = x >= y
  less x y = x < y
  greater x y = x > y
  -- (Conversions)
  dtypeToInt x = (fromEnum x) :: Int

--instance DType Char where?

--instance ByteString where?

--instance DType BFloat16 where?