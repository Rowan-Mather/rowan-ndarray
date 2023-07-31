{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_text (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "text"
version :: Version
version = Version [2,0,1] []

synopsis :: String
synopsis = "An efficient packed Unicode text type."
copyright :: String
copyright = "2009-2011 Bryan O'Sullivan, 2008-2009 Tom Harper, 2021 Andrew Lelechenko"
homepage :: String
homepage = "https://github.com/haskell/text"
