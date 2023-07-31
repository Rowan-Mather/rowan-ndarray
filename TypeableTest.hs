--language extension list
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/table.html

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}



module TypeableTest where

--import Data.Dense
import Data.Dynamic
import Data.Typeable
import Data.Vector
import Data.Array.Repa as R
--import Onnx.Representation.GraphDef

--instance Typeable (Array U s )

{-
data NDArray typ where
    NDArray :: (Typeable typ, Show typ, Shape sh) =>
        Array U sh typ -> NDArray typ
newtype NDArray a = NDArray (R.Array U s t)
-}
--data NDArray2 = NDArray2 (Z :. Int :. ...) Dynamic

-- to fix w/type variable? https://downloads.haskell.org/~ghc/6.4/docs/html/users_guide/type-extensions.html
--data NDArray = NDAConstr { 
--    fromND :: (Shape sh) => Array U sh Dynamic
--}

--deriving instance Show (NDArray a)
--instance Show (NDArray a) where
--    show (NDArray x) = show x

--t = NDArray (fromListUnboxed (Z :. (3::Int) :. (3::Int)) [1..9::Int])





-- Dynamic type, a dype, have an instance of each supported type
-- which defines how all the standard ops work
-- Can you have optional functions defined here? e.g. bitshift 
class Typeable a => Dype a where
  add :: a -> a -> a
  mult :: a -> a -> a
  eq :: a -> a -> Bool
  -- ...

-- You can provide a shape for the arrays but you can also not
-- Need a function which works this out to convert a Dshape to a Sshape
-- given the array
data Dshape = Dshape | Sshape [Int] deriving Show

-- Dshape is a hacky instance of Repa's shape so the NdArray definition 
-- won't complain. 
instance Eq Dshape where
    Dshape == Dshape        = False
    Sshape x == Sshape y    = x == y
    Dshape == Sshape _      = False
    Sshape _ == Dshape      = False
instance R.Shape Dshape where
    rank _ = undefined

--data NdArray where
--  NdArray :: Dype a => Array U Dshape a -> NdArray

--unwrapND :: Dype a => NdArray -> Array U Dshape a
--unwrapND (NdArray x) = x
--    where intarr = 
--        if typeRep a == "Int" then 
--            arr :: Just (Array U Dshape Int) 
--        else Nothing

--instance Show NdArray where
--    show x | = show $ cast (Array U Dshape Int)

data TypedNdArray type shape = TypedNdArray String [Int]

data NdArray where
  NdArray :: Dype a => Array U (Z :. Int) a -> NdArray

instance Dype Int where
    add x y = x + y
    mult x y = x * y
    eq x y = x == y

unwrapND :: NdArray -> Maybe (Array U (Z :. Int) Int)
unwrapND (NdArray x) = Just x

arr = fromListUnboxed (Z :. (2::Int)) [1,2::Int]
nd = NdArray arr
arr2 = unwrapND nd


--deriving instance Show (NdArray)

-- instance of Dypes for Float Int etc

{-
addArrays :: NdArray -> NdArray -> NdArray
addArrays (NdArray x) (NdArray y) = case typeOf x `eqTypeRep` typeOf b of
  Just HRefl -> NdArray (zipWith add x y)
  Nothing -> throw ValueError  -- or should this automatically convert?
-}