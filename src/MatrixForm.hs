{-# LANGUAGE TypeApplications #-}

module MatrixForm where

import NdArray
import Data.Tree
import qualified Data.Vector.Storable as V

data TreeMatrix a = B a | A [TreeMatrix a]

-- READING MATRICIES
matrixToTree :: TreeMatrix a -> Tree [a]
matrixToTree (B x)  = Node [x] []
matrixToTree (A xs) = Node [] (map matrixToTree xs)

-- Example 2x3x2
{-
l :: TreeMatrix Int
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

-- WRITING MATRICIES
prettyShowArray :: NdArray -> String
prettyShowArray (NdArray s v) = conc <> "\n"
  where
    vl = map show (V.toList v)
    largest = maximum $ map length vl
    newlines = scanr1 (*) s
    spaced = zipWith (\i x -> (i, padStringTo largest x)) [0..] vl
    lined = addNewlines newlines spaced
    conc = concatMap snd lined

printArray :: NdArray -> IO ()
printArray nd = putStr $ prettyShowArray nd

padStringTo :: Int -> String -> String
padStringTo i s = replicate (i - length s) ' ' ++ s ++ " "

addNewlines :: [Integer] -> [(Integer, String)] -> [(Integer, String)]
addNewlines [] xs = xs
addNewlines (l:ls) xs = map (\(i,x) -> if i /= 0 && i `mod` l == 0 then (i, "\n"++x) else (i,x)) (addNewlines ls xs)