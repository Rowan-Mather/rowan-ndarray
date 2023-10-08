module Main where

-- base
import Control.Monad (forM)

-- criterion
import qualified Criterion.Main as Criterion

-- hmatrix
import qualified Numeric.LinearAlgebra as HM

-- massiv
import qualified Data.Massiv.Array as MA

-- numskull
import qualified Numskull as NS

-- random-mwc
import qualified System.Random.MWC as Mwc
import qualified System.Random.MWC.Distributions as Mwc

-- vector
import qualified Data.Vector.Storable as S

listGen :: Int -> IO [Double]
listGen n = do
  gen <- Mwc.create
  forM [1..n] $ \_ -> Mwc.normal 0 1 gen

main :: IO ()
main = do
  let n :: Int
      n = 50

  aList <- listGen (n*n)
  bList <- listGen (n*n)

  let aSVector = S.fromList aList
  let bSVector = S.fromList bList

  -- numskull
  let Just aNS' = NS.fromVector [toInteger n, toInteger n] aSVector
  let Just bNS' = NS.fromVector [toInteger n, toInteger n] bSVector

  -- hmatrix
  let aHM' = (n HM.>< n) aList
  let bHM' = (n HM.>< n) bList

  -- massiv
  let aMA' = MA.resize' (MA.Sz (n MA.:. n)) (MA.fromList MA.Seq aList) :: MA.Matrix MA.S Double
  let bMA' = MA.resize' (MA.Sz (n MA.:. n)) (MA.fromList MA.Seq aList) :: MA.Matrix MA.S Double

  Criterion.defaultMain
    [ Criterion.env (pure (aNS', bNS')) $ \ ~(aNS, bNS) ->
        Criterion.bgroup "numskull"
          [ Criterion.bench "matrix-matrix multiplication" $ Criterion.nf (aNS `NS.matMul`) bNS ]
    , Criterion.env (pure (aHM', bHM')) $ \ ~(aHM, bHM) ->
        Criterion.bgroup "hmatrix"
          [ Criterion.bench "matrix-matrix multiplication" $ Criterion.nf (aHM <>) bHM ]
    , Criterion.env (pure (aMA', bMA')) $ \ ~(aMA, bMA) ->
        Criterion.bgroup "massiv"
          [ Criterion.bench "matrix-matrix multiplication" $ Criterion.nf (aMA MA.!><!) bMA ]
    ]
