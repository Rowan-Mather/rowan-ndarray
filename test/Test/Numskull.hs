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

  describe "NdArray matmul" $
    it "2x3 times 3x4" $
      (N.fromList [2,3] [1..2*3 :: Int] `matMul` N.fromList [3,4] [1..3*4 :: Int]) `shouldBe`
        N.fromList [2,4] [38, 44, 50, 56, 83, 98, 113, 128 :: Int]

{-
    describe "padShape" $ do
        focus . it "works on a less simple example" $
            property $ \content (NonNegative extra) ->
            let n = toInteger $ length content
            in
            padShape (N.fromList [n] content) [n + extra] `shouldBe` N.fromList [n + extra] (content <> replicate (fromInteger extra) (0 :: Int))
-}
