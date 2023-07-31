module Test.Numskull where

-- hspec
import Test.Hspec

-- QuickCheck
import Test.QuickCheck (NonNegative(..), property)

-- ndarray (local)
import Numskull as N

spec :: Spec
spec = do
    describe "NdArray equality" $
        it "works" $
           N.fromList [3] [1,2,3::Int] == N.fromList [3] [1,2,3::Int]

{-
    describe "padShape" $ do
        focus . it "works on a less simple example" $
            property $ \content (NonNegative extra) ->
            let n = toInteger $ length content
            in
            padShape (N.fromList [n] content) [n + extra] `shouldBe` N.fromList [n + extra] (content <> replicate (fromInteger extra) (0 :: Int))
-}
--cabal test --test-show-details=streaming
-- ghci -isrc -itest test/Test/Numskull.hs
--     ghci> hspec spec

-- http://hoogle.jeeves.myrtle/file/nix/store/93d2fwbq53zclxallszy9ih0nbgcb5zw-hspec-expectations-0.8.2-doc/share/doc/hspec-expectations-0.8.2/html/Test-Hspec-Expectations.html#v:shouldBe

-- https://hackage.haskell.org/package/hspec-2.11.3/docs/Test-Hspec.html#v:example

-- https://hspec.github.io/