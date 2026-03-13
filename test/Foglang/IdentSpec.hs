{-# LANGUAGE OverloadedStrings #-}

module Foglang.IdentSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Text as T
import Foglang.Ident (ident)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, parse)

spec :: Spec
spec = do
  let valid =
        [ "a",
          "_x9",
          "ThisVariableIsExported",
          "αβ"
        ] ::
          [T.Text]

  let invalid =
        [ "123abc",
          "",
          " x",
          "42"
        ]

  let parseIdent s = parse (ident <* eof) "IdentSpec.hs" s

  describe "ident" $ do
    it "parses valid identifiers" $
      mapM_ (\s -> parseIdent s `shouldBe` Right s) valid

    it "rejects invalid identifiers" $
      mapM_ (\s -> parseIdent s `shouldSatisfy` isLeft) invalid
