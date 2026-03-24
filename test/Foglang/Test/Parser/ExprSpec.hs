module Foglang.Test.Parser.ExprSpec (spec) where

import Control.Monad.State.Strict (evalState)
import Data.Either (isLeft)
import Foglang.AST (Binding (..), Expr (..), ExprAnn (..), FloatLit (..), Ident (..), IntLit (..), MatchArm (..), Param (..), TypeExpr (..))
import Foglang.Parser (SC(..), scn)
import Foglang.Parser.Expr (sequence')
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, runParserT)
import Text.Megaparsec.Pos (initialPos, mkPos)

-- Dummy annotation for test expected values (matches what stripPos normalizes to).
a :: ExprAnn
a = ExprAnn { pos = initialPos "", ty = TNamed (Ident "unresolved"), isStmt = False }

-- Strip all source positions and types from an Expr tree to enable structural
-- comparison without caring about exact positions or placeholder types.
stripPos :: Expr -> Expr
stripPos (EVar _ i) = EVar a i
stripPos (EIntLit _ lit) = EIntLit a lit
stripPos (EFloatLit _ lit) = EFloatLit a lit
stripPos (EStrLit _ lit) = EStrLit a lit
stripPos (EUnitLit _) = EUnitLit a
stripPos (ELet _ name (Binding ps t rhs) mInExpr) =
  ELet a name (Binding (map stripParam ps) (stripType t) (stripPos rhs)) (fmap stripPos mInExpr)
stripPos (ELambda _ (Binding ps t body)) =
  ELambda a (Binding (map stripParam ps) (stripType t) (stripPos body))
stripPos (EIf _ cond then' else') =
  EIf a (stripPos cond) (stripPos then') (stripPos else')
stripPos (EInfixOp _ e1 op e2) =
  EInfixOp a (stripPos e1) op (stripPos e2)
stripPos (EApplication _ f args) =
  EApplication a (stripPos f) (map stripPos args)
stripPos (EIndex _ e idx) =
  EIndex a (stripPos e) (stripPos idx)
stripPos (ESliceLit _ exprs) =
  ESliceLit a (map stripPos exprs)
stripPos (EMapLit _) = EMapLit a
stripPos (ESequence _ exprs) =
  ESequence a (map stripPos exprs)
stripPos (EVariadicSpread _ e) =
  EVariadicSpread a (stripPos e)
stripPos (EMatch _ scrut arms) =
  EMatch a (stripPos scrut) (map stripArmPos arms)

-- Normalize TypeExpr: replace TVars with the placeholder.
stripType :: TypeExpr -> TypeExpr
stripType (TVar _) = ty a
stripType (TConstrained _ _) = ty a
stripType (TSlice t) = TSlice (stripType t)
stripType (TMap k v) = TMap (stripType k) (stripType v)
stripType (TFunc ps mv r) = TFunc (map stripType ps) (fmap stripType mv) (stripType r)
stripType t = t

-- Normalize params: replace TVars in param types with placeholder.
stripParam :: Param -> Param
stripParam PUnit = PUnit
stripParam (PTyped n t) = PTyped n (stripType t)
stripParam (PVariadic n t) = PVariadic n (stripType t)

stripArmPos :: MatchArm -> MatchArm
stripArmPos (MatchArm _ pat body) = MatchArm (pos a) pat (stripPos body)

spec :: Spec
spec = do
  let validELet =
        [ ("let x : int = 1", ELet a "x" (Binding [] (TNamed "int") (EIntLit a (IntDecimal "1"))) Nothing),
          ("let x:int=2", ELet a "x" (Binding [] (TNamed "int") (EIntLit a (IntDecimal "2"))) Nothing),
          ( "let f (x : int) => int = x",
            ELet a "f" (Binding [PTyped "x" (TNamed "int")] (TNamed "int") (EVar a "x")) Nothing
          ),
          ( "let f (x : int) -> (y : int) => int = x",
            ELet a "f" (Binding [PTyped "x" (TNamed "int"), PTyped "y" (TNamed "int")] (TNamed "int") (EVar a "x")) Nothing
          ),
          ( "let f () => unit = x",
            ELet a "f" (Binding [PUnit] (TNamed "unit") (EVar a "x")) Nothing
          ),
          -- Untyped value binding (inferred type)
          ("let x = 1", ELet a "x" (Binding [] (ty a) (EIntLit a (IntDecimal "1"))) Nothing),
          -- Bare identifier params (inferred types)
          ( "let f x y = x",
            ELet a "f" (Binding [PTyped "x" (ty a), PTyped "y" (ty a)] (ty a) (EVar a "x")) Nothing
          ),
          -- Parenthesized untyped params
          ( "let f (x) (y) = x",
            ELet a "f" (Binding [PTyped "x" (ty a), PTyped "y" (ty a)] (ty a) (EVar a "x")) Nothing
          ),
          -- Mixed bare and annotated params
          ( "let f x (y : int) = x",
            ELet a "f" (Binding [PTyped "x" (ty a), PTyped "y" (TNamed "int")] (ty a) (EVar a "x")) Nothing
          ),
          -- Bare params with explicit return type
          ( "let f x y => int = x",
            ELet a "f" (Binding [PTyped "x" (ty a), PTyped "y" (ty a)] (TNamed "int") (EVar a "x")) Nothing
          )
        ]

  let invalidELet =
        [ "let x =",
          "letx = 1",
          "let type = 1"
        ]

  let validEInfixOp =
        [ ("1 + 2", EInfixOp a (EIntLit a (IntDecimal "1")) "+" (EIntLit a (IntDecimal "2"))),
          ("3.14 * 2.0", EInfixOp a (EFloatLit a (FloatDecimal "3.14")) "*" (EFloatLit a (FloatDecimal "2.0"))),
          ("x - y", EInfixOp a (EVar a "x") "-" (EVar a "y")),
          ( "1 + 2 * 3",
            EInfixOp a
              (EIntLit a (IntDecimal "1"))
              "+"
              (EInfixOp a (EIntLit a (IntDecimal "2")) "*" (EIntLit a (IntDecimal "3")))
          ),
          ("a / b", EInfixOp a (EVar a "a") "/" (EVar a "b")),
          ("a % b", EInfixOp a (EVar a "a") "%" (EVar a "b")),
          ("a <<< b", EInfixOp a (EVar a "a") "<<<" (EVar a "b")),
          ("a >>> b", EInfixOp a (EVar a "a") ">>>" (EVar a "b")),
          ("a &&& b", EInfixOp a (EVar a "a") "&&&" (EVar a "b")),
          ("a ||| b", EInfixOp a (EVar a "a") "|||" (EVar a "b")),
          ("a ^^^ b", EInfixOp a (EVar a "a") "^^^" (EVar a "b")),
          ("a == b", EInfixOp a (EVar a "a") "==" (EVar a "b")),
          ("a != b", EInfixOp a (EVar a "a") "!=" (EVar a "b")),
          ("a < b", EInfixOp a (EVar a "a") "<" (EVar a "b")),
          ("a > b", EInfixOp a (EVar a "a") ">" (EVar a "b")),
          ("a <= b", EInfixOp a (EVar a "a") "<=" (EVar a "b")),
          ("a >= b", EInfixOp a (EVar a "a") ">=" (EVar a "b")),
          ("a && b", EInfixOp a (EVar a "a") "&&" (EVar a "b")),
          ("a || b", EInfixOp a (EVar a "a") "||" (EVar a "b")),
          -- &&& (prec 5) tighter than && (prec 2)
          ("a &&& b && c", EInfixOp a (EInfixOp a (EVar a "a") "&&&" (EVar a "b")) "&&" (EVar a "c")),
          -- ||| (prec 4) tighter than && (prec 2)
          ("a ||| b && c", EInfixOp a (EInfixOp a (EVar a "a") "|||" (EVar a "b")) "&&" (EVar a "c")),
          -- == (prec 3) tighter than && (prec 2)
          ("a == b && c", EInfixOp a (EInfixOp a (EVar a "a") "==" (EVar a "b")) "&&" (EVar a "c")),
          -- && (prec 2) tighter than || (prec 1)
          ("a && b || c", EInfixOp a (EInfixOp a (EVar a "a") "&&" (EVar a "b")) "||" (EVar a "c"))
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
            EIf a (EVar a "x") (EIntLit a (IntDecimal "1")) (EIntLit a (IntDecimal "2"))
          ),
          ( "if x then y else z",
            EIf a (EVar a "x") (EVar a "y") (EVar a "z")
          ),
          ( "if x then 1 else 2 + 3",
            EIf a
              (EVar a "x")
              (EIntLit a (IntDecimal "1"))
              (EInfixOp a (EIntLit a (IntDecimal "2")) "+" (EIntLit a (IntDecimal "3")))
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
        [ ("(1)", EIntLit a (IntDecimal "1")),
          ("(x)", EVar a "x"),
          ( "(1 + 2) * 3",
            EInfixOp a
              (EInfixOp a (EIntLit a (IntDecimal "1")) "+" (EIntLit a (IntDecimal "2")))
              "*"
              (EIntLit a (IntDecimal "3"))
          )
        ]

  let invalidParen =
        [ "(1",
          "1)"
        ]

  let validEApplication =
        [ ("f x", EApplication a (EVar a "f") [EVar a "x"]),
          ("f x y", EApplication a (EVar a "f") [EVar a "x", EVar a "y"]),
          ("f 1 2", EApplication a (EVar a "f") [EIntLit a (IntDecimal "1"), EIntLit a (IntDecimal "2")]),
          ( "f (x + 1)",
            EApplication a (EVar a "f") [EInfixOp a (EVar a "x") "+" (EIntLit a (IntDecimal "1"))]
          ),
          ( "f x + y",
            EInfixOp a (EApplication a (EVar a "f") [EVar a "x"]) "+" (EVar a "y")
          ),
          -- if/func/match are now valid argument atoms (line fold disambiguates)
          ( "f if x then 1 else 2",
            EApplication a (EVar a "f") [EIf a (EVar a "x") (EIntLit a (IntDecimal "1")) (EIntLit a (IntDecimal "2"))]
          )
        ]

  -- Lambda expressions with untyped/bare params
  let validELambda =
        [ ( "func (x) = x",
            ELambda a (Binding [PTyped "x" (ty a)] (ty a) (EVar a "x"))
          ),
          ( "func x = x",
            ELambda a (Binding [PTyped "x" (ty a)] (ty a) (EVar a "x"))
          ),
          ( "func x y = x",
            ELambda a (Binding [PTyped "x" (ty a), PTyped "y" (ty a)] (ty a) (EVar a "x"))
          )
        ]

  let parseExpr s = evalState (runParserT (sequence' Nothing (mkPos 1) <* runSC scn <* eof) "ExprSpec.hs" s) 0

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
