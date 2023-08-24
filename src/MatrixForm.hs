module MatrixForm where

import NdArray
import Data.Tree
import qualified Data.Vector.Storable as V

-- * READING MATRICIES

{- | This type is specifically for pretty explicit definitions of NdArrays.
The A constructor is for Array - a set of values and B is the value.
-- Example 2x3x2
l :: TreeMatrix Int
l   = A [A [A [B 1,  B 2],
            A [B 3,  B 4],
            A [B 5,  B 6]],

         A [A [B 7,  B 8],
            A [B 9,  B 10],
            A [B 11, B 12]]]
-}
data TreeMatrix a = B a | A [TreeMatrix a]

-- Converts a TreeMatrix to a Tree of lists
matrixToTree :: TreeMatrix a -> Tree [a]
matrixToTree (B x)  = Node [x] []
matrixToTree (A xs) = Node [] (map matrixToTree xs)

-- Converts a Tree of lists to a single ordered list.
flattenToList :: Tree [a] -> [a]
flattenToList = concat . flatten

-- Calculates the shape of the NdArray corresponding to the Tree.
treeShape :: Tree [a] -> [Integer]
treeShape t = zipWith (\x y -> fromIntegral $ div x y ::Integer) (drop 1 levelLen) levelLen
  where levelLen = map length $ levels t

-- Calculates the shape of the NdArray corresponding to the TreeMatrix.
matrixShape :: TreeMatrix a -> [Integer]
matrixShape = treeShape . matrixToTree

-- * WRITING MATRICIES

-- | Prints out the pretty NdArray representation.
printArray :: NdArray -> IO ()
printArray nd = putStr $ prettyShowArray nd

-- | Converts an NdArray to its pretty representation.
-- Values along a row are separated whitespace. Along a column, newlines.
-- For higher dimensions, an additional newline is added to separate the nxm matrices. 
prettyShowArray :: NdArray -> String
prettyShowArray (NdArray s v) = conc <> "\n"
  where
    vl = map show (V.toList v)
    largest = maximum $ map length vl
    newlines = scanr1 (*) s
    spaced = zipWith (\i x -> (i, padStringTo largest x)) [0..] vl
    lined = addNewlines newlines spaced
    conc = concatMap snd lined

-- Separates values along a row by whitespace.
padStringTo :: Int -> String -> String
padStringTo i s = replicate (i - length s) ' ' ++ s ++ " "

-- Separates columns and higher dimensions by newlines.
addNewlines :: [Integer] -> [(Integer, String)] -> [(Integer, String)]
addNewlines ls xs = foldr (\l -> 
  map (\(i, x) -> if i /= 0 && i `mod` l == 0 
    then (i, "\n" ++ x) 
    else (i, x))) xs ls