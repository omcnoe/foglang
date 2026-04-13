module Foglang.Test.Parser.ExprSpec (spec) where

import Data.Either (isLeft)
import Foglang.AST (Binding (..), ConcreteShape (..), Expr (..), ExprAnn (..), FloatLit (..), Ident (..), IntLit (..), MatchArm (..), Param (..), TypeExpr (..))
import Foglang.Parser (SC(..), evalParse, scn)
import Foglang.Parser.Expr (childBlockExprSequence)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof)
import Text.Megaparsec.Pos (initialPos)

-- Dummy annotation for test expected values (matches what stripPos normalizes to).
a :: ExprAnn TypeExpr
a = ExprAnn { pos = initialPos "", ty = TShape (CNamed (Ident "unresolved")), isStmt = False }

-- Strip all source positions and types from an Expr tree to enable structural
-- comparison without caring about exact positions or placeholder types.
stripPos :: Expr TypeExpr -> Expr TypeExpr
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
stripPos (ECoerce _ c inner) =
  ECoerce a c (stripPos inner)

-- Normalize TypeExpr: replace TypeVars with the placeholder.
stripType :: TypeExpr -> TypeExpr
stripType (TShape (CNamed t)) = TShape (CNamed t)
stripType (TShape (CSlice t)) = TShape (CSlice (stripType t))
stripType (TShape (CMap k v)) = TShape (CMap (stripType k) (stripType v))
stripType (TShape (CFunc ps mv r)) = TShape (CFunc (map stripType ps) (fmap stripType mv) (stripType r))
stripType (TVar _ _) = ty a

-- Normalize params: replace TypeVars in param types with placeholder.
stripParam :: Param TypeExpr -> Param TypeExpr
stripParam PUnit = PUnit
stripParam (PTyped n t) = PTyped n (stripType t)
stripParam (PVariadic n t) = PVariadic n (stripType t)

stripArmPos :: MatchArm TypeExpr -> MatchArm TypeExpr
stripArmPos (MatchArm _ pat body) = MatchArm (pos a) pat (stripPos body)

spec :: Spec
spec = do
  let validELet =
        [ ("let x : int = 1", ELet a "x" (Binding [] (TShape (CNamed "int")) (EIntLit a (IntDecimal "1"))) Nothing),
          ("let x:int=2", ELet a "x" (Binding [] (TShape (CNamed "int")) (EIntLit a (IntDecimal "2"))) Nothing),
          ( "let f (x : int) => int = x",
            ELet a "f" (Binding [PTyped "x" (TShape (CNamed "int"))] (TShape (CNamed "int")) (EVar a "x")) Nothing
          ),
          ( "let f (x : int) -> (y : int) => int = x",
            ELet a "f" (Binding [PTyped "x" (TShape (CNamed "int")), PTyped "y" (TShape (CNamed "int"))] (TShape (CNamed "int")) (EVar a "x")) Nothing
          ),
          ( "let f () => unit = x",
            ELet a "f" (Binding [PUnit] (TShape (CNamed "unit")) (EVar a "x")) Nothing
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
            ELet a "f" (Binding [PTyped "x" (ty a), PTyped "y" (TShape (CNamed "int"))] (ty a) (EVar a "x")) Nothing
          ),
          -- Bare params with explicit return type
          ( "let f x y => int = x",
            ELet a "f" (Binding [PTyped "x" (ty a), PTyped "y" (ty a)] (TShape (CNamed "int")) (EVar a "x")) Nothing
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

  -- childBlock inside continuations should anchor to the
  -- continuation's column, not the enclosing fold's envFoldCol.
  let invalidIfIndent =
        [ -- body at col 4, if at col 5 -> body not indented past if
          "let x =\n\
          \  y +\n\
          \    if cond\n\
          \    then\n\
          \   body",
          -- body at same col as if (col 5) -> still not indented past if
          "let x =\n\
          \  y +\n\
          \    if cond\n\
          \    then\n\
          \    body",
          -- else branch body under-indented
          "let x =\n\
          \  y +\n\
          \    if cond\n\
          \    then\n\
          \      1\n\
          \    else\n\
          \   body"
        ]

  let validParen =
        [ ("(1)", EIntLit a (IntDecimal "1")),
          ("(x)", EVar a "x"),
          ( "(1 + 2) * 3",
            EInfixOp a
              (EInfixOp a (EIntLit a (IntDecimal "1")) "+" (EIntLit a (IntDecimal "2")))
              "*"
              (EIntLit a (IntDecimal "3"))
          ),
          -- Semicolons separate items inside parens
          ( "(1; 2)",
            ESequence a [EIntLit a (IntDecimal "1"), EIntLit a (IntDecimal "2")]
          ),
          -- Let inside parens: semicolon separates let from in-expression
          ( "(let x = 1; x)",
            ELet a "x" (Binding [] (ty a) (EIntLit a (IntDecimal "1"))) (Just (EVar a "x"))
          ),
          -- Let RHS doesn't absorb semicolon
          ( "(let x = 1 + 2; x)",
            ELet a "x" (Binding [] (ty a) (EInfixOp a (EIntLit a (IntDecimal "1")) "+" (EIntLit a (IntDecimal "2")))) (Just (EVar a "x"))
          ),
          -- Semicolons replace alignment: outdented items valid with explicit ;
          ( "(f 1;\n\
            \  2;\n\
            \  3)",
            ESequence a [EApplication a (EVar a "f") [EIntLit a (IntDecimal "1")], EIntLit a (IntDecimal "2"), EIntLit a (IntDecimal "3")]
          )
        ]

  let invalidParen =
        [ "(1",
          "1)"
        ]

  let invalidMatch =
        [ -- arms at same column as match (must be indented past)
          "match x with\n| 0 => 1\n| _ => 2"
        ]

  let validEIndex =
        [ -- Single
          ("xs[0]", EIndex a (EVar a "xs") (EIntLit a (IntDecimal "0"))),
          -- Nested
          ( "m[0][1]",
            EIndex a
              (EIndex a (EVar a "m") (EIntLit a (IntDecimal "0")))
              (EIntLit a (IntDecimal "1"))
          ),
          -- Tight (no space): index
          ("f[xs]", EIndex a (EVar a "f") (EVar a "xs")),
          -- Space: application with slice-literal argument
          ( "f [xs]",
            EApplication a (EVar a "f") [ESliceLit a [EVar a "xs"]]
          ),
          -- Indexing binds tighter than application: `f xs[0]` = `f (xs[0])`, not `(f xs)[0]`
          ( "f xs[0]",
            EApplication a (EVar a "f") [EIndex a (EVar a "xs") (EIntLit a (IntDecimal "0"))]
          ),
          -- Symmetric: indexed function applied to arg
          ( "f[0] x",
            EApplication a (EIndex a (EVar a "f") (EIntLit a (IntDecimal "0"))) [EVar a "x"]
          ),
          -- Slice literal indexing
          ( "[1; 2; 3][2]",
            EIndex a
              (ESliceLit a [EIntLit a (IntDecimal "1"), EIntLit a (IntDecimal "2"), EIntLit a (IntDecimal "3")])
              (EIntLit a (IntDecimal "2"))
          ),
          -- Index-via-subexpression
          ( "xs[f 1+2/3]",
            EIndex a
              (EVar a "xs")
              ( EInfixOp a
                  (EApplication a (EVar a "f") [EIntLit a (IntDecimal "1")])
                  "+"
                  (EInfixOp a (EIntLit a (IntDecimal "2")) "/" (EIntLit a (IntDecimal "3")))
              )
          ),
          -- Multi-line: index content on continuation lines
          ( "xs[\n\
            \  77\n\
            \]",
            EIndex a (EVar a "xs") (EIntLit a (IntDecimal "77"))
          )
        ]

  let invalidEIndex =
        [ "xs[]",     -- empty brackets
          "xs[0; 1]"  -- semicolons inside brackets (withoutSemicolons should reject)
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

  -- Indented args are fold continuations (application), not sequence items.
  let validFoldApplication =
        [ -- Bare: indented args
          ( "f\n\
            \  1\n\
            \  2\n\
            \  3",
            EApplication a (EVar a "f") [EIntLit a (IntDecimal "1"), EIntLit a (IntDecimal "2"), EIntLit a (IntDecimal "3")]
          ),
          -- Bare: inline + indented args
          ( "f 1\n\
            \  2\n\
            \  3",
            EApplication a (EVar a "f") [EIntLit a (IntDecimal "1"), EIntLit a (IntDecimal "2"), EIntLit a (IntDecimal "3")]
          ),
          -- Parens: inline start, indented args
          ( "(f\n\
            \  1\n\
            \  2\n\
            \  3\n\
            \)",
            EApplication a (EVar a "f") [EIntLit a (IntDecimal "1"), EIntLit a (IntDecimal "2"), EIntLit a (IntDecimal "3")]
          ),
          -- Then body: inline with then, continuation below is application
          ( "if cond\n\
            \then f 1\n\
            \  2\n\
            \else 3",
            EIf a
              (EVar a "cond")
              (EApplication a (EVar a "f") [EIntLit a (IntDecimal "1"), EIntLit a (IntDecimal "2")])
              (EIntLit a (IntDecimal "3"))
          ),
          -- Then body: inline paren sequence (semicolons relax alignment)
          ( "if cond\n\
            \then (f 1;\n\
            \  2)\n\
            \else 3",
            EIf a
              (EVar a "cond")
              (ESequence a [EApplication a (EVar a "f") [EIntLit a (IntDecimal "1")], EIntLit a (IntDecimal "2")])
              (EIntLit a (IntDecimal "3"))
          ),
          -- Else-if: inline with then/else, continuations are application
          ( "if a\n\
            \then f 1\n\
            \  2\n\
            \else if b\n\
            \then g 3\n\
            \  4\n\
            \else 5",
            EIf a
              (EVar a "a")
              (EApplication a (EVar a "f") [EIntLit a (IntDecimal "1"), EIntLit a (IntDecimal "2")])
              (EIf a
                (EVar a "b")
                (EApplication a (EVar a "g") [EIntLit a (IntDecimal "3"), EIntLit a (IntDecimal "4")])
                (EIntLit a (IntDecimal "5")))
          )
        ]

  -- Same-column items are sequence siblings, not application args.
  let validSequenceIndent =
        [ -- Bare: all same column
          ( "f\n\
            \1\n\
            \2\n\
            \3",
            ESequence a [EVar a "f", EIntLit a (IntDecimal "1"), EIntLit a (IntDecimal "2"), EIntLit a (IntDecimal "3")]
          ),
          -- Parens: all same column
          ( "(\n\
            \  f\n\
            \  1\n\
            \  2\n\
            \  3\n\
            \)",
            ESequence a [EVar a "f", EIntLit a (IntDecimal "1"), EIntLit a (IntDecimal "2"), EIntLit a (IntDecimal "3")]
          ),
          -- Parens: semicolons force sequence even with indented items
          ( "(f;\n\
            \  1;\n\
            \  2;\n\
            \  3\n\
            \)",
            ESequence a [EVar a "f", EIntLit a (IntDecimal "1"), EIntLit a (IntDecimal "2"), EIntLit a (IntDecimal "3")]
          )
        ]

  -- Closing delimiter at parent indent level
  let validClosingDelimiter =
        [ -- Paren on own line at fold level
          ( "let x =\n\
            \  (\n\
            \    1\n\
            \  )",
            ELet a "x" (Binding [] (ty a) (EIntLit a (IntDecimal "1"))) Nothing
          ),
          -- Bracket on own line at fold level
          ( "let x =\n\
            \  [\n\
            \    1\n\
            \  ]",
            ELet a "x" (Binding [] (ty a) (ESliceLit a [EIntLit a (IntDecimal "1")])) Nothing
          )
        ]

  let parseExpr s = evalParse (childBlockExprSequence <* runSC scn <* eof) "ExprSpec.hs" s

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
    it "index" $
      mapM_ (\(s, expected) -> fmap stripPos (parseExpr s) `shouldBe` Right expected) validEIndex
    it "lambda with untyped params" $
      mapM_ (\(s, expected) -> fmap stripPos (parseExpr s) `shouldBe` Right expected) validELambda

  describe "indentation" $ do
    it "indented items are application args (fold continuation)" $
      mapM_ (\(s, expected) -> fmap stripPos (parseExpr s) `shouldBe` Right expected) validFoldApplication
    it "same-column items are sequence siblings" $
      mapM_ (\(s, expected) -> fmap stripPos (parseExpr s) `shouldBe` Right expected) validSequenceIndent
    it "closing delimiter at parent indent level" $
      mapM_ (\(s, expected) -> fmap stripPos (parseExpr s) `shouldBe` Right expected) validClosingDelimiter
    it "under-indented if/then/else body" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidIfIndent

  describe "sequence rejects" $ do
    it "invalid let" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidELet
    it "invalid infix op" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidEInfixOp
    it "invalid if" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidEIf
    it "invalid paren" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidParen
    it "invalid match (flush arms)" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidMatch
    it "invalid index" $
      mapM_ (\s -> parseExpr s `shouldSatisfy` isLeft) invalidEIndex
