{-# LANGUAGE TypeApplications #-}

module DType where 

import Prelude as P
import Data.Vector.Storable
import Type.Reflection
import GHC.Float (float2Double)
import Data.Int
import Data.Char

-- Basis for all pointwise operations
class (Typeable a, Storable a, Show a, Eq a, Ord a) => DType a where
  addId :: a
  multId :: a
  -- Numeric
  add :: a -> a -> a
  subtract :: a -> a -> a
  multiply :: a -> a -> a
  divide :: a -> a -> a
  div :: a -> a -> Int
  power :: a -> Double -> Double
  pow :: a -> a -> a
  log :: a -> a -> a
  mod :: a -> a -> Int
  abs :: a -> a
  signum :: a -> a
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
  -- Casting
  dtypeToRational :: a -> Rational
  rationalToDtype :: Rational -> a

instance DType Int where 
  addId = 0
  multId = 1
  -- Numeric
  add x y = x + y 
  subtract x y = x - y
  multiply x y = x * y
  divide x y = P.div x y
  div x y = (fromIntegral $ P.div x y) :: Int
  power x d = fromIntegral x ** d
  pow x y = x ^ y
  log x y = (P.floor $ logBase xd yd) :: Int
    where xd = fromIntegral @Int @Double x
          yd = fromIntegral @Int @Double y
  mod = P.mod
  abs = P.abs
  signum = P.signum
  ceil x = x
  floor x = x
  -- Trig
  sin = roundIntFunc P.sin
  cos = roundIntFunc P.cos
  tan = roundIntFunc P.tan
  -- Logical 
  invert x = -x
  shiftleft x = x * 2
  shiftright x = x `P.div` 2
  -- (Conversions)
  dtypeToRational = toRational
  rationalToDtype = P.floor . fromRational @Double

roundIntFunc :: (Float -> Float) -> Int -> Int
roundIntFunc f x = (round $ f $ fromIntegral @Int @Float x) :: Int

-- Entirely copied really
instance DType Int32 where 
  addId = 0
  multId = 1
  -- Numeric
  add x y = x + y 
  subtract x y = x - y
  multiply x y = x * y
  divide x y = P.div x y
  div x y = fromIntegral @Int32 @Int $ P.div x y
  power x d = fromIntegral x ** d
  pow x y = x ^ y
  log x y = (P.floor $ logBase xd yd) :: Int32
    where xd = fromIntegral @Int32 @Double x
          yd = fromIntegral @Int32 @Double y
  mod x y = fromIntegral @Int32 @Int $ P.mod x y
  abs = P.abs
  signum = P.signum
  ceil x = x
  floor x = x
  -- Trig
  sin x = (round $ P.sin $ fromIntegral @Int32 @Float x) :: Int32
  cos x = (round $ P.sin $ fromIntegral @Int32 @Float x) :: Int32
  tan x = (round $ P.sin $ fromIntegral @Int32 @Float x) :: Int32
  -- Logical 
  invert x = -x
  shiftleft x = x * 2
  shiftright x = x `P.div` 2
  -- (Conversions)
  dtypeToRational = toRational
  rationalToDtype = P.floor . fromRational @Double

instance DType Int64 where
  addId = 0
  multId = 1
  -- Numeric
  add x y = x + y 
  subtract x y = x - y
  multiply x y = x * y
  divide x y = P.div x y
  div x y = fromIntegral @Int64 @Int $ P.div x y
  power x d = fromIntegral x ** d
  pow x y = x ^ y
  log x y = (P.floor $ logBase xd yd) :: Int64
    where xd = fromIntegral @Int64 @Double x
          yd = fromIntegral @Int64 @Double y
  mod x y = fromIntegral @Int64 @Int $ P.mod x y
  abs = P.abs
  signum = P.signum
  ceil x = x
  floor x = x
  -- Trig
  sin x = (round $ P.sin $ fromIntegral @Int64 @Float x) :: Int64
  cos x = (round $ P.sin $ fromIntegral @Int64 @Float x) :: Int64
  tan x = (round $ P.sin $ fromIntegral @Int64 @Float x) :: Int64
  -- Logical 
  invert x = -x
  shiftleft x = x * 2
  shiftright x = x `P.div` 2
  -- (Conversions)
  dtypeToRational = toRational
  rationalToDtype = P.floor . fromRational @Double

instance DType Float where 
  addId = 0.0
  multId = 1.0
  -- Numeric
  add x y = x + y 
  subtract x y = x - y
  multiply x y = x * y
  divide x y = x/y
  div x y = P.floor x `P.div` P.floor y
  power x d = float2Double x ** d
  pow x y = x ** y
  log x y = logBase x y
  mod x y = P.floor x `P.mod` P.floor y
  abs = P.abs
  signum = P.signum
  ceil = fromIntegral @Integer @Float . P.ceiling
  floor = fromIntegral @Integer @Float . P.floor
  -- Trig
  sin = P.sin
  cos = P.cos
  tan = P.tan
  -- Logical 
  invert x = -x
  shiftleft x = x * 2
  shiftright x = x / 2
  -- Conversion
  dtypeToRational = toRational
  rationalToDtype = fromRational @Float

instance DType Double where 
  addId = 0.0
  multId = 1.0
  -- Numeric
  add x y = x + y 
  subtract x y = x - y
  multiply x y = x * y
  divide x y = x/y
  div x y = P.floor x `P.div` P.floor y
  power x d = x ** d
  pow x y = x ** y
  log x y = logBase x y
  mod x y = P.floor x `P.mod` P.floor y
  abs = P.abs
  signum = P.signum
  ceil = fromIntegral @Integer @Double . P.ceiling
  floor = fromIntegral @Integer @Double . P.floor
  -- Trig
  sin = P.sin
  cos = P.cos
  tan = P.tan
  -- Logical 
  invert x = -x
  shiftleft x = x * 2
  shiftright x = x / 2
  -- Conversion
  dtypeToRational = toRational
  rationalToDtype = fromRational @Double

instance DType Bool where 
  addId = False
  multId = True
    -- Numeric
  add x y = x || y
  subtract x y = (x || y) && not (x && y)
  multiply x y = x && y
  divide x y = not (x && y)
  div _ _ = 0
  power _x _d = undefined
  pow x y = toEnum (fromEnum x ^ fromEnum y)
  log _x _y = undefined
  mod x y = fromEnum x `P.mod` fromEnum y
  abs _ = True
  signum = id
  ceil = id
  floor = id
  -- Trig (False = 0, True = 1 or /=0)
  sin False = False
  sin True = True
  cos False = True
  cos True = True
  tan False = False
  tan True = True
  -- Logical 
  invert x = not x
  shiftleft _ = False
  shiftright _ = False
  -- Conversions
  dtypeToRational False = 0
  dtypeToRational True = 1
  rationalToDtype 0 = False
  rationalToDtype _ = True

instance DType Char where 
  addId = '\NUL'
  multId = 'a'
  -- Numeric
  add x y = chr $ ord x + ord y
  subtract x y = chr $ min 0 $ ord x + ord y
  multiply _ _ = undefined
  divide _ _ = undefined
  div _ _ = undefined
  power _ _ = undefined
  pow _ _ = undefined
  log _ _ = undefined
  mod _ _ = undefined
  abs = undefined
  signum c = if isAlpha c then if isUpper c then 'A' else 'a'
             else if isDigit c then '0' else c
  ceil = toUpper
  floor = toLower
  -- Trig
  sin = undefined
  cos = undefined
  tan = undefined
  -- Logical 
  invert c = if isUpper c then toLower c else toUpper c
  shiftleft x = chr $ ord x + 1
  shiftright x =  chr $ ord x - 1
  -- Conversion
  dtypeToRational = toRational . ord
  rationalToDtype = chr . P.floor. fromRational @Double

--instance ByteString where?

--instance DType BFloat16 where?