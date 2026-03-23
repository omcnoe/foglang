module Foglang.Test.Parser.ExprSpec (spec) where

import Control.Monad.State.Strict (evalState)
import Data.Either (isLeft)
import Foglang.AST (Binding (..), Expr (..), FloatLit (..), Ident (..), IntLit (..), MatchArm (..), Param (..), TypeExpr (..))
import Foglang.Parser (scn)
import Foglang.Parser.Expr (sequence')
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, runParserT)
import Text.Megaparsec.Pos (SourcePos, initialPos, mkPos)

-- Dummy source position for test expected values.
p :: SourcePos
p = initialPos ""

-- Placeholder type used in expected values (matches what stripPos normalizes to).
u :: TypeExpr
u = TNamed (Ident "unresolved")

-- Strip all source positions and types from an Expr tree to enable structural
-- comparison without caring about exact positions or placeholder types.
stripPos :: Expr -> Expr
stripPos (EVar _ _ i) = EVar p u i
stripPos (EIntLit _ _ lit) = EIntLit p u lit
stripPos (EFloatLit _ _ lit) = EFloatLit p u lit
stripPos (EStrLit _ _ lit) = EStrLit p u lit
stripPos (EUnitLit _) = EUnitLit p
stripPos (ELet _ _ name (Binding ps ty rhs) mInExpr) =
  ELet p u name (Binding (map stripParam ps) (stripType ty) (stripPos rhs)) (fmap stripPos mInExpr)
stripPos (ELambda _ _ (Binding ps ty body)) =
  ELambda p u (Binding (map stripParam ps) (stripType ty) (stripPos body))
stripPos (EIf _ _ cond then' else') =
  EIf p u (stripPos cond) (stripPos then') (stripPos else')
stripPos (EInfixOp _ _ e1 op e2) =
  EInfixOp p u (stripPos e1) op (stripPos e2)
stripPos (EApplication _ _ f args) =
  EApplication p u (stripPos f) (map stripPos args)
stripPos (EIndex _ _ e idx) =
  EIndex p u (stripPos e) (stripPos idx)
stripPos (ESliceLit _ _ exprs) =
  ESliceLit p u (map stripPos exprs)
stripPos (EMapLit _ _) = EMapLit p u
stripPos (ESequence _ _ exprs) =
  ESequence p u (map stripPos exprs)
stripPos (EVariadicSpread _ _ e) =
  EVariadicSpread p u (stripPos e)
stripPos (EMatch _ _ scrut arms) =
  EMatch p u (stripPos scrut) (map stripArmPos arms)

-- Normalize TypeExpr: replace TVars with the placeholder u.
stripType :: TypeExpr -> TypeExpr
stripType (TVar _) = u
stripType (TConstrained _ _) = u
stripType (TSlice t) = TSlice (stripType t)
stripType (TMap k v) = TMap (stripType k) (stripType v)
stripType (TFunc ps mv r) = TFunc (map stripType ps) (fmap stripType mv) (stripType r)
stripType t = t

-- Normalize params: replace TVars in param types with placeholder u.
stripParam :: Param -> Param
stripParam PUnit = PUnit
stripParam (PTyped n t) = PTyped n (stripType t)
stripParam (PVariadic n t) = PVariadic n (stripType t)

stripArmPos :: MatchArm -> MatchArm
stripArmPos (MatchArm _ pat body) = MatchArm p pat (stripPos body)

spec :: Spec
spec = do
  let validELet =
        [ ("let x : int = 1", ELet p u "x" (Binding [] (TNamed "int") (EIntLit p u (IntDecimal "1"))) Nothing),
          ("let x:int=2", ELet p u "x" (Binding [] (TNamed "int") (EIntLit p u (IntDecimal "2"))) Nothing),
          ( "let f (x : int) => int = x",
            ELet p u "f" (Binding [PTyped "x" (TNamed "int")] (TNamed "int") (EVar p u "x")) Nothing
          ),
          ( "let f (x : int) -> (y : int) => int = x",
            ELet p u "f" (Binding [PTyped "x" (TNamed "int"), PTyped "y" (TNamed "int")] (TNamed "int") (EVar p u "x")) Nothing
          ),
          ( "let f () => unit = x",
            ELet p u "f" (Binding [PUnit] (TNamed "unit") (EVar p u "x")) Nothing
          ),
          -- Untyped value binding (inferred type)
          ("let x = 1", ELet p u "x" (Binding [] u (EIntLit p u (IntDecimal "1"))) Nothing),
          -- Bare identifier params (inferred types)
          ( "let f x y = x",
            ELet p u "f" (Binding [PTyped "x" u, PTyped "y" u] u (EVar p u "x")) Nothing
          ),
          -- Parenthesized untyped params
          ( "let f (x) (y) = x",
            ELet p u "f" (Binding [PTyped "x" u, PTyped "y" u] u (EVar p u "x")) Nothing
          ),
          -- Mixed bare and annotated params
          ( "let f x (y : int) = x",
            ELet p u "f" (Binding [PTyped "x" u, PTyped "y" (TNamed "int")] u (EVar p u "x")) Nothing
          ),
          -- Bare params with explicit return type
          ( "let f x y => int = x",
            ELet p u "f" (Binding [PTyped "x" u, PTyped "y" u] (TNamed "int") (EVar p u "x")) Nothing
          )
        ]

  let invalidELet =
        [ "let x =",
          "letx = 1",
          "let type = 1"
        ]

  let validEInfixOp =
        [ ("1 + 2", EInfixOp p u (EIntLit p u (IntDecimal "1")) "+" (EIntLit p u (IntDecimal "2"))),
          ("3.14 * 2.0", EInfixOp p u (EFloatLit p u (FloatDecimal "3.14")) "*" (EFloatLit p u (FloatDecimal "2.0"))),
          ("x - y", EInfixOp p u (EVar p u "x") "-" (EVar p u "y")),
          ( "1 + 2 * 3",
            EInfixOp p u
              (EIntLit p u (IntDecimal "1"))
              "+"
              (EInfixOp p u (EIntLit p u (IntDecimal "2")) "*" (EIntLit p u (IntDecimal "3")))
          ),
          ("a / b", EInfixOp p u (EVar p u "a") "/" (EVar p u "b")),
          ("a % b", EInfixOp p u (EVar p u "a") "%" (EVar p u "b")),
          ("a <<< b", EInfixOp p u (EVar p u "a") "<<<" (EVar p u "b")),
          ("a >>> b", EInfixOp p u (EVar p u "a") ">>>" (EVar p u "b")),
          ("a &&& b", EInfixOp p u (EVar p u "a") "&&&" (EVar p u "b")),
          ("a ||| b", EInfixOp p u (EVar p u "a") "|||" (EVar p u "b")),
          ("a ^^^ b", EInfixOp p u (EVar p u "a") "^^^" (EVar p u "b")),
          ("a == b", EInfixOp p u (EVar p u "a") "==" (EVar p u "b")),
          ("a != b", EInfixOp p u (EVar p u "a") "!=" (EVar p u "b")),
          ("a < b", EInfixOp p u (EVar p u "a") "<" (EVar p u "b")),
          ("a > b", EInfixOp p u (EVar p u "a") ">" (EVar p u "b")),
          ("a <= b", EInfixOp p u (EVar p u "a") "<=" (EVar p u "b")),
          ("a >= b", EInfixOp p u (EVar p u "a") ">=" (EVar p u "b")),
          ("a && b", EInfixOp p u (EVar p u "a") "&&" (EVar p u "b")),
          ("a || b", EInfixOp p u (EVar p u "a") "||" (EVar p u "b")),
          -- &&& (prec 5) tighter than && (prec 2)
          ("a &&& b && c", EInfixOp p u (EInfixOp p u (EVar p u "a") "&&&" (EVar p u "b")) "&&" (EVar p u "c")),
          -- ||| (prec 4) tighter than && (prec 2)
          ("a ||| b && c", EInfixOp p u (EInfixOp p u (EVar p u "a") "|||" (EVar p u "b")) "&&" (EVar p u "c")),
          -- == (prec 3) tighter than && (prec 2)
          ("a == b && c", EInfixOp p u (EInfixOp p u (EVar p u "a") "==" (EVar p u "b")) "&&" (EVar p u "c")),
          -- && (prec 2) tighter than || (prec 1)
          ("a && b || c", EInfixOp p u (EInfixOp p u (EVar p u "a") "&&" (EVar p u "b")) "||" (EVar p u "c"))
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
            EIf p u (EVar p u "x") (EIntLit p u (IntDecimal "1")) (EIntLit p u (IntDecimal "2"))
          ),
          ( "if x then y else z",
            EIf p u (EVar p u "x") (EVar p u "y") (EVar p u "z")
          ),
          ( "if x then 1 else 2 + 3",
            EIf p u
              (EVar p u "x")
              (EIntLit p u (IntDecimal "1"))
              (EInfixOp p u (EIntLit p u (IntDecimal "2")) "+" (EIntLit p u (IntDecimal "3")))
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
        [ ("(1)", EIntLit p u (IntDecimal "1")),
          ("(x)", EVar p u "x"),
          ( "(1 + 2) * 3",
            EInfixOp p u
              (EInfixOp p u (EIntLit p u (IntDecimal "1")) "+" (EIntLit p u (IntDecimal "2")))
              "*"
              (EIntLit p u (IntDecimal "3"))
          )
        ]

  let invalidParen =
        [ "(1",
          "1)"
        ]

  let validEApplication =
        [ ("f x", EApplication p u (EVar p u "f") [EVar p u "x"]),
          ("f x y", EApplication p u (EVar p u "f") [EVar p u "x", EVar p u "y"]),
          ("f 1 2", EApplication p u (EVar p u "f") [EIntLit p u (IntDecimal "1"), EIntLit p u (IntDecimal "2")]),
          ( "f (x + 1)",
            EApplication p u (EVar p u "f") [EInfixOp p u (EVar p u "x") "+" (EIntLit p u (IntDecimal "1"))]
          ),
          ( "f x + y",
            EInfixOp p u (EApplication p u (EVar p u "f") [EVar p u "x"]) "+" (EVar p u "y")
          ),
          -- if/func/match are now valid argument atoms (line fold disambiguates)
          ( "f if x then 1 else 2",
            EApplication p u (EVar p u "f") [EIf p u (EVar p u "x") (EIntLit p u (IntDecimal "1")) (EIntLit p u (IntDecimal "2"))]
          )
        ]

  -- Lambda expressions with untyped/bare params
  let validELambda =
        [ ( "func (x) = x",
            ELambda p u (Binding [PTyped "x" u] u (EVar p u "x"))
          ),
          ( "func x = x",
            ELambda p u (Binding [PTyped "x" u] u (EVar p u "x"))
          ),
          ( "func x y = x",
            ELambda p u (Binding [PTyped "x" u, PTyped "y" u] u (EVar p u "x"))
          )
        ]

  let parseExpr s = evalState (runParserT (sequence' Nothing (mkPos 1) <* scn <* eof) "ExprSpec.hs" s) 0

  describe "sequence parses" $ do
    it "let" $
      mapM_ (\(s, expected) -> fmap stripPos (parseExpr s) `shouldBe` Right expected) validELet
    it "infix op" $
      mapM_ (\(s, expected) -> fmap stripPos (parseExpr s) `shouldBe` Right expected) validEInfixOp
    it "if" $
      mapM_ (\(s, expected) -> fmap stripPos (parseExpr s) `shouldBe` Right expected) validEIf
    it "paren" $
      mapM_ (\(s, expected) -> fmap stripPos (parseExpr s) `shouldBe` Right expected) validParen
    it "application" $
      mapM_ (\(s, expected) -> fmap stripPos (parseExpr s) `shouldBe` Right expected) validEApplication
    it "lambda with untyped params" $
      mapM_ (\(s, expected) -> fmap stripPos (parseExpr s) `shouldBe` Right expected) validELambda

  describe "sequence rejects" $ do
    it "invalid let" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidELet
    it "invalid infix op" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidEInfixOp
    it "invalid if" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidEIf
    it "invalid paren" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidParen
