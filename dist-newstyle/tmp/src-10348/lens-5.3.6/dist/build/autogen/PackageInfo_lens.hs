{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_lens (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "lens"
version :: Version
version = Version [5,3,6] []

synopsis :: String
synopsis = "Lenses, Folds and Traversals"
copyright :: String
copyright = "Copyright (C) 2012-2016 Edward A. Kmett"
homepage :: String
homepage = "http://github.com/ekmett/lens/"
