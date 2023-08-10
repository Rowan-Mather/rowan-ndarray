{-# LANGUAGE TypeApplications #-}

module SliceTest where

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)

{-
preciseDiv x y = fromIntegral @Int @Float x / fromIntegral @Int @Float y

stride :: Vector Int -> Vector Int -> Vector a -> Vector Int
stride sh st v = 
  let
    -- shape
    dim' = V.scanr' (*) 1 sh
    newshape = V.map (\i -> 
            ceiling $ preciseDiv 
                ( 1 + (dim' V.! (i+1)) * (sh V.!i -1) ) 
                ( st V.! i )
            :: Int)
        (V.enumFromN 0 (V.length sh))
  in
    newshape

t = stride (V.fromList [5,5,5]) (V.fromList [50, 10, 2]) (V.fromList [0..124])
-}

expandRun :: [Int] -> Int -> [Int]
expandRun [] _ = []
expandRun (s:sts) x =
  if s == 0 then (0 : expandRun sts x)
  else x `div` s : expandRun sts (x `mod` s)