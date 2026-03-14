module Foglang.ExprSpec (spec) where

import Data.Either (isLeft)
import Foglang.AST (Expr (..), FloatLit (..), IntLit (..))
import Foglang.Parser.Expr qualified as Parser.Expr
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, parse)

spec :: Spec
spec = do
  let validLet =
        [ ("let x = 1", Let "x" [] (IntLit (Decimal "1"))),
          ("let x=2", Let "x" [] (IntLit (Decimal "2"))),
          ("let x p1 p2 = 7", Let "x" ["p1", "p2"] (IntLit (Decimal "7")))
        ]

  let invalidLet =
        [ "let x =",
          "letx = 1",
          "let type = 1"
        ]

  let validBinaryOp =
        [ ("1 + 2", BinaryOp (IntLit (Decimal "1")) "+" (IntLit (Decimal "2"))),
          ("3.14 * 2.0", BinaryOp (FloatLit (DecimalFloat "3.14")) "*" (FloatLit (DecimalFloat "2.0"))),
          ("x - y", BinaryOp (Ident "x") "-" (Ident "y")),
          ( "1 + 2 * 3",
            BinaryOp
              (IntLit (Decimal "1"))
              "+"
              (BinaryOp (IntLit (Decimal "2")) "*" (IntLit (Decimal "3")))
          )
        ]

  let invalidBinaryOp =
        [ "1 +",
          "+ 1",
          "1 + + 2"
        ]

  let validIf =
        [ ( "if x then 1 else 2",
            If (Ident "x") (IntLit (Decimal "1")) (IntLit (Decimal "2"))
          ),
          ( "if x then y else z",
            If (Ident "x") (Ident "y") (Ident "z")
          ),
          ( "if x then 1 else 2 + 3",
            If
              (Ident "x")
              (IntLit (Decimal "1"))
              (BinaryOp (IntLit (Decimal "2")) "+" (IntLit (Decimal "3")))
          )
        ]

  let invalidIf =
        [ "if then 1 else 2",
          "if x then else 2",
          "if x then 1 else",
          "ifx then 1 else 2",
          "if x 1 else 2"
        ]

  let validParen =
        [ ("(1)", IntLit (Decimal "1")),
          ("(x)", Ident "x"),
          ( "(1 + 2) * 3",
            BinaryOp
              (BinaryOp (IntLit (Decimal "1")) "+" (IntLit (Decimal "2")))
              "*"
              (IntLit (Decimal "3"))
          )
        ]

  let invalidParen =
        [ "(1",
          "1)",
          "()"
        ]

  let validApplication =
        [ ("f x", Application (Ident "f") [Ident "x"]),
          ("f x y", Application (Ident "f") [Ident "x", Ident "y"]),
          ("f 1 2", Application (Ident "f") [IntLit (Decimal "1"), IntLit (Decimal "2")]),
          ( "f (x + 1)",
            Application (Ident "f") [BinaryOp (Ident "x") "+" (IntLit (Decimal "1"))]
          ),
          ( "f x + y",
            BinaryOp (Application (Ident "f") [Ident "x"]) "+" (Ident "y")
          )
        ]

  let invalidApplication =
        [ "f if x then 1 else 2",
          "f let x = 1"
        ]

  let parseExpr s = parse (Parser.Expr.expr <* eof) "ExprSpec.hs" s

  describe "expr" $ do
    it "parses valid let expressions" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validLet

    it "rejects invalid let expressions" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidLet

    it "parses valid infix expressions" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validBinaryOp

    it "rejects invalid infix expressions" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidBinaryOp

    it "parses valid if expressions" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validIf

    it "rejects invalid if expressions" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidIf

    it "parses valid paren expressions" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validParen

    it "rejects invalid paren expressions" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidParen

    it "parses valid application expressions" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validApplication

    it "rejects invalid application expressions" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidApplication
