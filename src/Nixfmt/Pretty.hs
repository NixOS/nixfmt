{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Pretty where

import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc hiding (Doc, prettyList)
import qualified Data.Text.Prettyprint.Doc
import           Nixfmt.Types

type Doc = Data.Text.Prettyprint.Doc.Doc ()

instance Pretty NixAST where
    pretty (Leaf l) = pretty $ show l
    pretty node     = pretty $ show node
