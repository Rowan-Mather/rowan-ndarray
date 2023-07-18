module Main where

-- hspec
import Test.Hspec

-- ndarray (local)
import qualified Test.Numskull
import qualified Test.Serialisation

main :: IO ()
main = hspec $ do
    describe "Test.Numskull" Test.Numskull.spec
    describe "Test.Serialisation" Test.Serialisation.spec