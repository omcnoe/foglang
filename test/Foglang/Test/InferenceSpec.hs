module Foglang.Test.InferenceSpec (spec) where

import Control.Monad.State.Strict (evalState)
import Data.Text qualified as T
import Foglang.AST (Binding (..), Expr (..), Ident (..), TypeExpr (..), bindingType, exprType)
import Foglang.Inference (InferError (..), inferAndResolve)
import Foglang.Parser (SC(..), scn)
import Foglang.Parser.Expr (sequence')
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, runParserT)
import Text.Megaparsec.Pos (mkPos)

-- Parse a fog expression string into an Expr.
parseExpr :: T.Text -> Either String Expr
parseExpr s = case evalState (runParserT (sequence' Nothing (mkPos 1) <* runSC scn <* eof) "test" s) 0 of
  Left err -> Left (show err)
  Right expr -> Right expr

-- Parse and infer, returning the type of the outermost expression.
inferType :: T.Text -> Either [InferError] TypeExpr
inferType s = case parseExpr s of
  Left err -> error ("parse failed: " ++ err)
  Right expr -> exprType <$> inferAndResolve expr

-- Parse and infer, returning the full result.
inferResult :: T.Text -> Either [InferError] Expr
inferResult s = case parseExpr s of
  Left err -> error ("parse failed: " ++ err)
  Right expr -> inferAndResolve expr

-- Extract the type of the let-bound name's binding (the function/value type).
-- For "let f x = ... \n f 5", the outer expr type is the result of applying f.
-- But we can also look at the Binding inside a let to get the function type.
inferLetBindingType :: T.Text -> Either [InferError] TypeExpr
inferLetBindingType s = case parseExpr s of
  Left err -> error ("parse failed: " ++ err)
  Right expr -> do
    resolved <- inferAndResolve expr
    case resolved of
      ELet _ _ (Binding params retTy _) _ -> Right (bindingType params retTy)
      _ -> error "expected ELet"

-- Helper to check error constructor
isUnknownVariable :: InferError -> Bool
isUnknownVariable (UnknownVariable _ _) = True
isUnknownVariable _ = False

isTypeMismatch :: InferError -> Bool
isTypeMismatch (TypeMismatch _ _ _) = True
isTypeMismatch _ = False

isNamedPUnit :: InferError -> Bool
isNamedPUnit (NamedPUnit _ _) = True
isNamedPUnit _ = False

isNotAFunction :: InferError -> Bool
isNotAFunction (NotAFunction _ _) = True
isNotAFunction _ = False

isMissingSpread :: InferError -> Bool
isMissingSpread (MissingSpread _ _) = True
isMissingSpread _ = False

-- Shorthand for common types
intT :: TypeExpr
intT = TNamed (Ident "int")

float64T :: TypeExpr
float64T = TNamed (Ident "float64")

stringT :: TypeExpr
stringT = TNamed (Ident "string")

boolT :: TypeExpr
boolT = TNamed (Ident "bool")

spec :: Spec
spec = describe "Inference" $ do
  describe "positive tests" $ do
    it "int literal defaults to int" $
      inferType "42" `shouldBe` Right intT

    it "float literal defaults to float64" $
      inferType "3.14" `shouldBe` Right float64T

    it "string literal defaults to string" $
      inferType "\"hello\"" `shouldBe` Right stringT

    it "bool from prelude (true)" $
      inferType "true" `shouldBe` Right boolT

    -- Use newlines for let-in scoping (indentation based, not 'in' keyword).
    -- The body after the let is the "in" expression.
    it "arithmetic propagates type" $ do
      let result = inferType "let f x = x + 1\nf 5"
      result `shouldBe` Right intT

    it "arithmetic function has int -> int type" $ do
      let result = inferLetBindingType "let f x = x + 1\nf 5"
      result `shouldBe` Right (TFunc [intT] Nothing intT)

    it "comparison returns bool" $ do
      let result = inferType "let gt x = x > 0\ngt 5"
      result `shouldBe` Right boolT

    it "logical operators constrain to bool" $ do
      let result = inferType "let both x y = x && y\nboth true false"
      result `shouldBe` Right boolT

    it "if condition constrained to bool, result is int" $
      inferType "if true then 1 else 2" `shouldBe` Right intT

    it "if branches unified via annotation" $
      inferType "let f (x : int) = if true then x else 0\nf 5" `shouldBe` Right intT

    it "multi-parameter function" $
      inferType "let add x y = x + y + 1\nadd 2 3" `shouldBe` Right intT

    it "slice indexing" $
      inferType "let first (xs : []int) = xs[0]\nfirst [1, 2, 3]" `shouldBe` Right intT

    it "slice literal element types unified" $
      inferType "[1, 2, 3]" `shouldBe` Right (TSlice intT)

    it "recursive function" $ do
      let src = "let fib n =\n  if n < 2\n  then n\n  else fib (n - 1) + fib (n - 2)\nfib 10"
      inferType src `shouldBe` Right intT

    it "curried application (excess args applied to returned function)" $ do
      let src = "let f (x : int) => (int => int) = func (y : int) => int = x + y\nf 1 2"
      inferType src `shouldBe` Right intT

    it "curried application (3 levels deep)" $ do
      let src = "let f (x : int) => (int => (int => int)) = func (y : int) => (int => int) = func (z : int) => int = x + y + z\nf 1 2 3"
      inferType src `shouldBe` Right intT

    it "higher-order with unresolved function type" $ do
      let src = "let apply f x = f x\napply (func (n : int) => int = n + 1) 5"
      inferType src `shouldBe` Right intT

  describe "negative tests" $ do
    it "too many args to non-function return (curried)" $
      inferResult "let f (x : int) => int = x\nf 1 2" `shouldSatisfy` \r ->
        case r of
          Left errs -> any isNotAFunction errs
          Right _ -> False

    it "unknown variable" $
      inferResult "unknown_var" `shouldSatisfy` \r ->
        case r of
          Left errs -> any isUnknownVariable errs
          Right _ -> False

    -- Use annotated types to prevent unification from succeeding
    it "type mismatch in if condition (int vs bool)" $
      inferResult "let x : int = 42\nif x then 1 else 2" `shouldSatisfy` \r ->
        case r of
          Left errs -> any isTypeMismatch errs
          Right _ -> False

    it "type mismatch in if branches (int vs string)" $
      inferResult "let x : int = 1\nlet y : string = \"hello\"\nif true then x else y" `shouldSatisfy` \r ->
        case r of
          Left errs -> any isTypeMismatch errs
          Right _ -> False

    it "type mismatch in arithmetic (int vs string)" $
      inferResult "let x : int = 1\nlet y : string = \"hello\"\nx + y" `shouldSatisfy` \r ->
        case r of
          Left errs -> any isTypeMismatch errs
          Right _ -> False

    it "named unit param" $
      inferResult "let f (x : ()) = x\nf ()" `shouldSatisfy` \r ->
        case r of
          Left errs -> any isNamedPUnit errs
          Right _ -> False

    it "not a function" $
      inferResult "let x : int = 5\nx 10" `shouldSatisfy` \r ->
        case r of
          Left errs -> any isNotAFunction errs
          Right _ -> False

    it "missing spread: bare slice passed to variadic without ..." $
      inferResult "let f (args : ...int) => () = ()\nlet xs : []int = [1, 2, 3]\nf xs" `shouldSatisfy` \r ->
        case r of
          Left errs -> any isMissingSpread errs
          Right _ -> False

  describe "variadic spread" $ do
    it "spread slice into variadic succeeds" $
      inferType "let f (args : ...int) => () = ()\nlet xs : []int = [1, 2, 3]\nf xs..." `shouldBe` Right (TNamed (Ident "()"))
