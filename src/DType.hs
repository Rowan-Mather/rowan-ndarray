{-# LANGUAGE TypeApplications #-}

module DType where 

import Prelude as P
import Data.Vector.Storable
import Type.Reflection
import GHC.Float (float2Double)
import Data.Int
import Data.Char

-- | All types storable within an NdArray must implement DType. 
-- This defines some basic properties, mathematical operations and standards for conversion.
class (Typeable a, Storable a, Show a, Eq a, Ord a) => DType a where
  -- | Additive identity 
  addId :: a
  -- | Multiplicative identity
  multId :: a
  -- | Standard numeric operations
  -- NB: 
  -- divide preserves DType
  -- div is specifically for integer division and returns an Int
  -- pow preserves DType
  -- power is for precision and uses Doubles
  -- mod returns an Int
  add :: a -> a -> a
  subtract :: a -> a -> a
  multiply :: a -> a -> a
  divide :: a -> a -> a
  div :: a -> a -> Int
  power :: a -> Double -> Double
  pow :: a -> a -> a
  -- Log base x of y
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
  -- | Most logical operations are simply defined in the numeric section on Booleans.
  -- Invert is naturally defined as -x numerically and NOT x logically.
  invert :: a -> a
  shiftleft :: a -> a
  shiftright :: a -> a
  -- | Dtypes are converted between via the intermediate type of rational
  dtypeToRational :: a -> Rational
  rationalToDtype :: Rational -> a

instance DType Int where 
  addId = 0
  multId = 1
  -- Numeric
  add x y = x + y 
  subtract x y = x - y
  multiply x y = x * y
  divide = P.div
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
  -- Conversion
  dtypeToRational = toRational
  rationalToDtype = P.floor . fromRational @Double

roundIntFunc :: (Float -> Float) -> Int -> Int
roundIntFunc f x = (round $ f $ fromIntegral @Int @Float x) :: Int

instance DType Int32 where 
  addId = 0
  multId = 1
  -- Numeric
  add x y = x + y 
  subtract x y = x - y
  multiply x y = x * y
  divide = P.div 
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
  -- Conversion
  dtypeToRational = toRational
  rationalToDtype = P.floor . fromRational @Double

instance DType Int64 where
  addId = 0
  multId = 1
  -- Numeric
  add x y = x + y 
  subtract x y = x - y
  multiply x y = x * y
  divide = P.div
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
  -- Conversion
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
  log = logBase
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
  log = logBase
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
  -- | Logical OR
  add x y = x || y
  -- | Logical NOR
  subtract x y = not (x || y)
  -- | Logical AND
  multiply x y = x && y
  -- | Logical NAND
  divide x y = not (x && y)
  div x y = fromEnum $ DType.divide x y
  -- | Numeric power
  power x d = fromIntegral (fromEnum x) ** d
  -- | Logical reverse implication
  pow x y = not y || x
  -- | Logical implication
  log x y = not x || y
  -- | Logical XOR, but Int result
  mod x y = fromEnum $ (x || y) && not (x && y)
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
  -- Logical NOT
  invert = not
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
  signum c
    | isUpper c = 'A' 
    | isLower c = 'a'
    | isDigit c = '0'
    | otherwise = c
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