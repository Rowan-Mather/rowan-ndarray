{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_vector (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "vector"
version :: Version
version = Version [0,12,3,1] []

synopsis :: String
synopsis = "Efficient Arrays"
copyright :: String
copyright = "(c) Roman Leshchinskiy 2008-2012"
homepage :: String
homepage = "https://github.com/haskell/vector"
