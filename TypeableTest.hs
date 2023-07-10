--language extension list
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/table.html

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module TypeableTest where

--import Data.Dense
import Data.Dynamic
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
data NDArray = NDAConstr { 
    fromND :: (Shape sh) => Array U sh Dynamic
}

--deriving instance Show (NDArray a)
--instance Show (NDArray a) where
--    show (NDArray x) = show x

--t = NDArray (fromListUnboxed (Z :. (3::Int) :. (3::Int)) [1..9::Int])

--dynTypeRep (toDyn 4) == dynTypeRep (toDyn 2)
--DArray a + NDArray b =