module Foglang.Test.Parser.IdentSpec (spec) where

import Data.Either (isLeft)
import Foglang.AST (Ident (..))
import Foglang.Parser.Ident (ident)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, parse)

spec :: Spec
spec = do
  let valid =
        [ "a",
          "_x9",
          "ThisVariableIsExported",
          "αβ"
        ]

  let invalid =
        [ "123abc",
          "",
          " x",
          "42"
        ]

  let parseIdent s = parse (ident <* eof) "IdentSpec.hs" s

  describe "ident parses" $ do
    it "identifiers" $
      mapM_ (\s -> parseIdent s `shouldBe` Right (Ident s)) valid

  describe "ident rejects" $ do
    it "invalid identifiers" $
      mapM_ (\s -> parseIdent s `shouldSatisfy` isLeft) invalid
