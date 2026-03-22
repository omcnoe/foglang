module Foglang.Test.Parser.ExprSpec (spec) where

import Data.Either (isLeft)
import Foglang.AST (Binding (..), Expr (..), FloatLit (..), IntLit (..), Param (..), TypeExpr (..))
import Foglang.Parser (scn)
import Foglang.Parser.Expr (sequence')
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, parse)
import Text.Megaparsec.Pos (mkPos)

spec :: Spec
spec = do
  let validELet =
        [ ("let x : int = 1", ELet "x" (Binding [] (NamedType "int") (EIntLit (Decimal "1"))) (ESequence [])),
          ("let x:int=2", ELet "x" (Binding [] (NamedType "int") (EIntLit (Decimal "2"))) (ESequence [])),
          ( "let f (x : int) => int = x",
            ELet "f" (Binding [TypedParam "x" (NamedType "int")] (NamedType "int") (EVar "x")) (ESequence [])
          ),
          ( "let f (x : int) -> (y : int) => int = x",
            ELet "f" (Binding [TypedParam "x" (NamedType "int"), TypedParam "y" (NamedType "int")] (NamedType "int") (EVar "x")) (ESequence [])
          ),
          ( "let f () => unit = x",
            ELet "f" (Binding [UnitParam] (NamedType "unit") (EVar "x")) (ESequence [])
          )
        ]

  let invalidELet =
        [ "let x =",
          "letx = 1",
          "let type = 1",
          "let x = 1",
          "let f x = 1"
        ]

  let validEInfixOp =
        [ ("1 + 2", EInfixOp (EIntLit (Decimal "1")) "+" (EIntLit (Decimal "2"))),
          ("3.14 * 2.0", EInfixOp (EFloatLit (DecimalFloat "3.14")) "*" (EFloatLit (DecimalFloat "2.0"))),
          ("x - y", EInfixOp (EVar "x") "-" (EVar "y")),
          ( "1 + 2 * 3",
            EInfixOp
              (EIntLit (Decimal "1"))
              "+"
              (EInfixOp (EIntLit (Decimal "2")) "*" (EIntLit (Decimal "3")))
          ),
          ("a / b", EInfixOp (EVar "a") "/" (EVar "b")),
          ("a % b", EInfixOp (EVar "a") "%" (EVar "b")),
          ("a <<< b", EInfixOp (EVar "a") "<<<" (EVar "b")),
          ("a >>> b", EInfixOp (EVar "a") ">>>" (EVar "b")),
          ("a &&& b", EInfixOp (EVar "a") "&&&" (EVar "b")),
          ("a ||| b", EInfixOp (EVar "a") "|||" (EVar "b")),
          ("a ^^^ b", EInfixOp (EVar "a") "^^^" (EVar "b")),
          ("a == b", EInfixOp (EVar "a") "==" (EVar "b")),
          ("a != b", EInfixOp (EVar "a") "!=" (EVar "b")),
          ("a < b", EInfixOp (EVar "a") "<" (EVar "b")),
          ("a > b", EInfixOp (EVar "a") ">" (EVar "b")),
          ("a <= b", EInfixOp (EVar "a") "<=" (EVar "b")),
          ("a >= b", EInfixOp (EVar "a") ">=" (EVar "b")),
          ("a && b", EInfixOp (EVar "a") "&&" (EVar "b")),
          ("a || b", EInfixOp (EVar "a") "||" (EVar "b")),
          -- &&& (prec 5) tighter than && (prec 2)
          ("a &&& b && c", EInfixOp (EInfixOp (EVar "a") "&&&" (EVar "b")) "&&" (EVar "c")),
          -- ||| (prec 4) tighter than && (prec 2)
          ("a ||| b && c", EInfixOp (EInfixOp (EVar "a") "|||" (EVar "b")) "&&" (EVar "c")),
          -- == (prec 3) tighter than && (prec 2)
          ("a == b && c", EInfixOp (EInfixOp (EVar "a") "==" (EVar "b")) "&&" (EVar "c")),
          -- && (prec 2) tighter than || (prec 1)
          ("a && b || c", EInfixOp (EInfixOp (EVar "a") "&&" (EVar "b")) "||" (EVar "c"))
        ]

  let invalidEInfixOp =
        [ "a +",
          "+ b",
          "a + + b",
          "a &&& &&& b",
          "a ||| ||| b"
        ]

  let validEIf =
        [ ( "if x then 1 else 2",
            EIf (EVar "x") (EIntLit (Decimal "1")) (EIntLit (Decimal "2"))
          ),
          ( "if x then y else z",
            EIf (EVar "x") (EVar "y") (EVar "z")
          ),
          ( "if x then 1 else 2 + 3",
            EIf
              (EVar "x")
              (EIntLit (Decimal "1"))
              (EInfixOp (EIntLit (Decimal "2")) "+" (EIntLit (Decimal "3")))
          )
        ]

  let invalidEIf =
        [ "if then 1 else 2",
          "if x then else 2",
          "if x then 1 else",
          "ifx then 1 else 2",
          "if x 1 else 2"
        ]

  let validParen =
        [ ("(1)", EIntLit (Decimal "1")),
          ("(x)", EVar "x"),
          ( "(1 + 2) * 3",
            EInfixOp
              (EInfixOp (EIntLit (Decimal "1")) "+" (EIntLit (Decimal "2")))
              "*"
              (EIntLit (Decimal "3"))
          )
        ]

  let invalidParen =
        [ "(1",
          "1)"
        ]

  let validEApplication =
        [ ("f x", EApplication (EVar "f") [EVar "x"]),
          ("f x y", EApplication (EVar "f") [EVar "x", EVar "y"]),
          ("f 1 2", EApplication (EVar "f") [EIntLit (Decimal "1"), EIntLit (Decimal "2")]),
          ( "f (x + 1)",
            EApplication (EVar "f") [EInfixOp (EVar "x") "+" (EIntLit (Decimal "1"))]
          ),
          ( "f x + y",
            EInfixOp (EApplication (EVar "f") [EVar "x"]) "+" (EVar "y")
          ),
          -- if/func/match are now valid argument atoms (line fold disambiguates)
          ( "f if x then 1 else 2",
            EApplication (EVar "f") [EIf (EVar "x") (EIntLit (Decimal "1")) (EIntLit (Decimal "2"))]
          )
        ]

  let invalidEApplication =
        [ "f let x = 1"
        ]

  let parseExpr s = parse (sequence' Nothing (mkPos 1) <* scn <* eof) "ExprSpec.hs" s

  describe "sequence parses" $ do
    it "let" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validELet
    it "infix op" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validEInfixOp
    it "if" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validEIf
    it "paren" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validParen
    it "application" $
      mapM_ (\(s, expected) -> parseExpr s `shouldBe` Right expected) validEApplication

  describe "sequence rejects" $ do
    it "invalid let" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidELet
    it "invalid infix op" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidEInfixOp
    it "invalid if" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidEIf
    it "invalid paren" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidParen
    it "invalid application" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidEApplication
