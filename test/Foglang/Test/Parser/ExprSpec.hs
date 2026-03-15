module Foglang.Test.Parser.ExprSpec (spec) where

import Data.Either (isLeft)
import Foglang.AST (Expr (..), FloatLit (..), IntLit (..), Param (..), TypeExpr (..))
import Foglang.Parser.Expr (exprBlock)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, parse)

spec :: Spec
spec = do
  let validLet =
        [ ("let x : int = 1", Let "x" [] (NamedType "int") (IntLit (Decimal "1")) (Sequence [])),
          ("let x:int=2", Let "x" [] (NamedType "int") (IntLit (Decimal "2")) (Sequence [])),
          ( "let f (x : int) => int = x",
            Let "f" [TypedParam "x" (NamedType "int")] (NamedType "int") (Var "x") (Sequence [])
          ),
          ( "let f (x : int) -> (y : int) => int = x",
            Let "f" [TypedParam "x" (NamedType "int"), TypedParam "y" (NamedType "int")] (NamedType "int") (Var "x") (Sequence [])
          ),
          ( "let f () => unit = x",
            Let "f" [UnitParam] (NamedType "unit") (Var "x") (Sequence [])
          )
        ]

  let invalidLet =
        [ "let x =",
          "letx = 1",
          "let type = 1",
          "let x = 1",
          "let f x = 1"
        ]

  let validBinaryOp =
        [ ("1 + 2", BinaryOp (IntLit (Decimal "1")) "+" (IntLit (Decimal "2"))),
          ("3.14 * 2.0", BinaryOp (FloatLit (DecimalFloat "3.14")) "*" (FloatLit (DecimalFloat "2.0"))),
          ("x - y", BinaryOp (Var "x") "-" (Var "y")),
          ( "1 + 2 * 3",
            BinaryOp
              (IntLit (Decimal "1"))
              "+"
              (BinaryOp (IntLit (Decimal "2")) "*" (IntLit (Decimal "3")))
          ),
          ("a / b", BinaryOp (Var "a") "/" (Var "b")),
          ("a % b", BinaryOp (Var "a") "%" (Var "b")),
          ("a << b", BinaryOp (Var "a") "<<" (Var "b")),
          ("a >> b", BinaryOp (Var "a") ">>" (Var "b")),
          ("a & b", BinaryOp (Var "a") "&" (Var "b")),
          ("a &^ b", BinaryOp (Var "a") "&^" (Var "b")),
          ("a | b", BinaryOp (Var "a") "|" (Var "b")),
          ("a ^ b", BinaryOp (Var "a") "^" (Var "b")),
          ("a == b", BinaryOp (Var "a") "==" (Var "b")),
          ("a != b", BinaryOp (Var "a") "!=" (Var "b")),
          ("a < b", BinaryOp (Var "a") "<" (Var "b")),
          ("a > b", BinaryOp (Var "a") ">" (Var "b")),
          ("a <= b", BinaryOp (Var "a") "<=" (Var "b")),
          ("a >= b", BinaryOp (Var "a") ">=" (Var "b")),
          ("a && b", BinaryOp (Var "a") "&&" (Var "b")),
          ("a || b", BinaryOp (Var "a") "||" (Var "b")),
          -- & (prec 5) tighter than && (prec 2)
          ("a & b && c", BinaryOp (BinaryOp (Var "a") "&" (Var "b")) "&&" (Var "c")),
          -- \| (prec 4) tighter than && (prec 2)
          ("a | b && c", BinaryOp (BinaryOp (Var "a") "|" (Var "b")) "&&" (Var "c")),
          -- == (prec 3) tighter than && (prec 2)
          ("a == b && c", BinaryOp (BinaryOp (Var "a") "==" (Var "b")) "&&" (Var "c")),
          -- && (prec 2) tighter than || (prec 1)
          ("a && b || c", BinaryOp (BinaryOp (Var "a") "&&" (Var "b")) "||" (Var "c"))
        ]

  let invalidBinaryOp =
        [ "a +",
          "+ b",
          "a + + b",
          "a & & b",
          "a | | b"
        ]

  let validIf =
        [ ( "if x then 1 else 2",
            If (Var "x") (IntLit (Decimal "1")) (IntLit (Decimal "2"))
          ),
          ( "if x then y else z",
            If (Var "x") (Var "y") (Var "z")
          ),
          ( "if x then 1 else 2 + 3",
            If
              (Var "x")
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
          ("(x)", Var "x"),
          ( "(1 + 2) * 3",
            BinaryOp
              (BinaryOp (IntLit (Decimal "1")) "+" (IntLit (Decimal "2")))
              "*"
              (IntLit (Decimal "3"))
          )
        ]

  let invalidParen =
        [ "(1",
          "1)"
        ]

  let validApplication =
        [ ("f x", Application (Var "f") [Var "x"]),
          ("f x y", Application (Var "f") [Var "x", Var "y"]),
          ("f 1 2", Application (Var "f") [IntLit (Decimal "1"), IntLit (Decimal "2")]),
          ( "f (x + 1)",
            Application (Var "f") [BinaryOp (Var "x") "+" (IntLit (Decimal "1"))]
          ),
          ( "f x + y",
            BinaryOp (Application (Var "f") [Var "x"]) "+" (Var "y")
          )
        ]

  let invalidApplication =
        [ "f if x then 1 else 2",
          "f let x = 1"
        ]

  let parseExpr s = parse (exprBlock <* eof) "ExprSpec.hs" s

  describe "exprBlock parses" $ do
    it "let" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validLet
    it "binary op" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validBinaryOp
    it "if" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validIf
    it "paren" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validParen
    it "application" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validApplication

  describe "exprBlock rejects" $ do
    it "invalid let" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidLet
    it "invalid binary op" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidBinaryOp
    it "invalid if" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidIf
    it "invalid paren" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidParen
    it "invalid application" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidApplication
