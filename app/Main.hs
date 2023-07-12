module Main where

import qualified MyLib (someFunc)
import Dense

x = V2 1 2 ^+^ V2 3 4

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
