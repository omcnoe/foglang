{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Foglang.ExprSpec (spec) where

import Data.Either (isLeft)
import Foglang.Expr qualified as Expr
import Foglang.IntLit qualified as IntLit
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, parse)

spec :: Spec
spec = do
  let validLet =
        [ ("let x = 1", Expr.Let ("x", Expr.IntLit (IntLit.Decimal "1"))),
          ("let x=2", Expr.Let ("x", Expr.IntLit (IntLit.Decimal "2")))
        ]

  let invalidLet =
        [ "let x =",
          "letx = 1",
          "let type = 1"
        ]

  let parseExpr s = parse (Expr.expr <* eof) "ExprSpec.hs" s

  describe "expr" $ do
    it "parses valid let expressions" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validLet

    it "rejects invalid let expressions" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidLet
