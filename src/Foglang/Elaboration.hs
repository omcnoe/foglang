module Foglang.Elaboration (ElabError (..), preludeEnv, elabExpr) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Foglang.AST (Binding (..), Expr (..), Ident (..), Param (..), TypeExpr (..))
import Foglang.TAST (TBinding (..), TExpr (..), tExprType)

-- Environment mapping names to their types.
type Env = Map.Map Ident TypeExpr

data ElabError
  = UnknownVariable Ident
  | NotAFunction TypeExpr
  | MissingSpread TypeExpr    -- bare SliceType value in variadic slot without ...
  | InvalidSpread TypeExpr     -- ... applied to a non-SliceType value
  deriving (Eq, Show)

-- Build the full type for a let-binding given its params and declared return type.
-- For value bindings (empty params), returns retTy directly.
-- For function bindings, constructs a FuncType with an explicit optional variadic slot.
letBindingType :: [Param] -> TypeExpr -> TypeExpr
letBindingType [] retTy = retTy -- non-function value
letBindingType ps retTy = FuncType fixedTys mVarTy retTy
  where
    fixedTys = [ty | TypedParam _ ty <- ps]
    mVarTy = case reverse ps of
      (VariadicParam _ ty : _) -> Just ty
      _ -> Nothing

-- The result type when applying a function to nArgs arguments.
-- For variadic functions:
--   n <= nFixed  -> partial application (returns FuncType of remaining params including variadic)
--   n >  nFixed  -> full call (variadic args or () sentinel supplied)
applyType :: TypeExpr -> Int -> Either ElabError TypeExpr
applyType (FuncType fixed mVar ret) n =
  case mVar of
    Nothing
      | n >= length fixed -> Right ret
      | otherwise         -> Right (FuncType (drop n fixed) Nothing ret)
    Just varTy
      | n > length fixed  -> Right ret
      | n == length fixed -> Right (FuncType [] (Just varTy) ret)
      | otherwise         -> Right (FuncType (drop n fixed) (Just varTy) ret)
applyType (NamedType (Ident "opaque")) _ = Right (NamedType (Ident "opaque"))
applyType t _ = Left (NotAFunction t)

-- Result type of a binary operator: comparisons and logical operators always
-- return bool; arithmetic/bitwise operators return the type of the left operand.
binaryOpResultType :: T.Text -> TypeExpr -> TypeExpr
binaryOpResultType op lhsTy
  | op `elem` ["==", "!=", "<", ">", "<=", ">=", "&&", "||"] = NamedType (Ident "bool")
  | otherwise = lhsTy

-- Built-in names that are always in scope.
preludeEnv :: Env
preludeEnv =
  Map.fromList
    [ (Ident "true", NamedType (Ident "bool")),
      (Ident "false", NamedType (Ident "bool"))
    ]

-- Elaborate an expression in the given environment, producing a TExpr.
elabExpr :: Env -> Expr -> Either ElabError TExpr
elabExpr env (Var i@(Ident t)) =
  case Map.lookup i env of
    Just ty -> Right (TVar ty i)
    Nothing
      -- Qualified names (e.g. fmt.Println) have no fog type; treat as opaque.
      -- TODO: revisit when introducing multi-package fog (fog-defined qualified
      -- names will need to be resolved across package boundaries) and when
      -- improving Go integration (partially parsing Go package exports to read
      -- their types would let us give opaque names real types here).
      | "." `T.isInfixOf` t -> Right (TVar (NamedType (Ident "opaque")) i)
      | otherwise -> Left (UnknownVariable i)
elabExpr _ UnitLit =
  Right TUnitLit
elabExpr _ (IntLit lit) =
  Right (TIntLit (NamedType (Ident "int")) lit)
elabExpr _ (FloatLit lit) =
  Right (TFloatLit (NamedType (Ident "float64")) lit)
elabExpr _ (StrLit lit) =
  Right (TStrLit (NamedType (Ident "string")) lit)
elabExpr env (BinaryOp e1 op e2) = do
  te1 <- elabExpr env e1
  te2 <- elabExpr env e2
  Right (TBinaryOp (binaryOpResultType op (tExprType te1)) te1 op te2)
elabExpr env (If cond then' else') = do
  tcond <- elabExpr env cond
  tthen <- elabExpr env then'
  telse <- elabExpr env else'
  Right (TIf (tExprType tthen) tcond tthen telse)
elabExpr env (Sequence exprs) = do
  texprs <- mapM (elabExpr env) exprs
  let ty = case texprs of
        [] -> NamedType (Ident "unit")
        _ -> tExprType (last texprs)
  Right (TSequence ty texprs)
elabExpr env (Lambda (Binding params retTy body)) = do
  let paramEnv = Map.fromList $
        [(name, ty) | TypedParam name ty <- params] ++
        [(name, SliceType ty) | VariadicParam name ty <- params]
  tbody <- elabExpr (Map.union paramEnv env) body
  Right (TLambda (letBindingType params retTy) (TBinding params retTy tbody))
elabExpr env (Let name (Binding params retTy rhs) inExpr) = do
  let bindingTy = letBindingType params retTy
  let envWithSelf = Map.insert name bindingTy env
  let paramEnv = Map.fromList $
        [(n, ty) | TypedParam n ty <- params] ++
        [(n, SliceType ty) | VariadicParam n ty <- params]
  trhs <- elabExpr (Map.union paramEnv envWithSelf) rhs
  tin <- elabExpr envWithSelf inExpr
  Right (TLet (tExprType tin) name (TBinding params retTy trhs) tin)
elabExpr env (VariadicSpread e) = do
  te <- elabExpr env e
  case tExprType te of
    SliceType _ -> Right (TVariadicSpread (tExprType te) te)
    ty -> Left (InvalidSpread ty)
elabExpr env (Application f args) = do
  tf <- elabExpr env f
  targs <- mapM (elabExpr env) args
  let nFixed = case tExprType tf of
        FuncType fixed _ _ -> length fixed
        _ -> 0
      nSupplied = length targs
      mVarTy = case tExprType tf of
        FuncType _ mVar _ -> mVar
        _ -> Nothing
  case mVarTy of
    Just _ -> do
      let varArgs = drop nFixed targs
      -- These checks only apply when variadic args are actually being supplied.
      -- When nSupplied <= nFixed it's a partial application; no variadic slot involved.
      if nSupplied > nFixed
        then do
          -- TODO: emit a warning (not an error) when MissingSpread fires; warnings
          -- are not yet supported in the parser.
          case filter isBareVariadic varArgs of
            (ba : _) -> Left (MissingSpread (tExprType ba))
            []       -> pure ()
        else pure ()
    Nothing -> pure ()
  resultTy <- applyType (tExprType tf) (length targs)
  Right (TApplication resultTy tf targs)

-- A bare SliceType value that is not wrapped in TVariadicSpread is not allowed in a variadic slot.
isBareVariadic :: TExpr -> Bool
isBareVariadic TUnitLit = False
isBareVariadic te = case tExprType te of
  SliceType _ -> case te of
    TVariadicSpread _ _ -> False
    _ -> True
  _ -> False
