{-# LANGUAGE OverloadedStrings #-}

module Foglang.IdentSpec (spec) where

import qualified Data.Text as T
import Foglang.Ident (ident)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (eof, parse)

spec :: Spec
spec = do
  let goSpecExamples =
        [ "a",
          "_x9",
          "ThisVariableIsExported",
          "αβ"
        ] ::
          [T.Text]

  let parseIdent s = parse (ident <* eof) "IdentSpec.hs" s

  describe "ident" $
    it "parses go spec examples" $
      mapM_ (\s -> parseIdent s `shouldBe` Right s) goSpecExamples
