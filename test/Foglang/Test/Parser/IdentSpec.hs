module Foglang.Test.Parser.IdentSpec (spec) where

import Control.Monad.State.Strict (evalState)
import Data.Either (isLeft)
import Foglang.AST (Ident (..))
import Foglang.Parser.Ident (ident)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, runParserT)

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

  let parseIdent s = evalState (runParserT (ident <* eof) "IdentSpec.hs" s) 0

  describe "ident parses" $ do
    it "identifiers" $
      mapM_ (\s -> parseIdent s `shouldBe` Right (Ident s)) valid

  describe "ident rejects" $ do
    it "invalid identifiers" $
      mapM_ (\s -> parseIdent s `shouldSatisfy` isLeft) invalid
