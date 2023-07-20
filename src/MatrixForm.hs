{-# LANGUAGE TypeApplications #-}

module MatrixForm where

import Data.Tree

data TreeMatrix a = B a | A [TreeMatrix a]

matrixToTree :: TreeMatrix a -> Tree [a]
matrixToTree (B x)  = Node [x] []
matrixToTree (A xs) = Node [] (map matrixToTree xs)

-- Example 2x3x2
{-
l   = A [A [A [B 1,  B 2],
            A [B 3,  B 4],
            A [B 5,  B 6]],

         A [A [B 7,  B 8],
            A [B 9,  B 10],
            A [B 11, B 12]]]
-}

-- Prelude.map Prelude.length $ levels $ treeify l''
-- dimension = next val/current val
-- Prelude.zipWith div (Prelude.drop 1 x) x

flattenToList :: Tree [a] -> [a]
flattenToList = concat . flatten

treeShape :: Tree [a] -> [Integer]
treeShape t = zipWith (\x y -> fromIntegral $ div x y ::Integer) (drop 1 levelLen) levelLen
  where levelLen = map length $ levels t

matrixShape :: TreeMatrix a -> [Integer]
matrixShape = treeShape . matrixToTree