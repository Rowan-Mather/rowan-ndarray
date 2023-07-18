module Main where

-- doctest
import Test.DocTest

main :: IO ()
main = doctest $ "-isrc" : map ("src/" <>)
  [ "DType.hs"
  , "Numskull.hs"
  , "Serialisation.hs"
  ]