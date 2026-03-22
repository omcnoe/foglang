module Foglang.Elaboration (ElabError (..), preludeEnv, elabExpr) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Foglang.AST (Binding (..), Expr (..), Ident (..), MatchArm (..), Param (..), Pattern (..), TypeExpr (..))
import Foglang.TAST (TBinding (..), TExpr (..), TMatchArm (..), TPattern (..), tExprType)

-- Environment mapping names to their types.
type Env = Map.Map Ident TypeExpr

data ElabError
  = UnknownVariable Ident
  | NotAFunction TypeExpr
  | MissingSpread TypeExpr -- bare SliceType value in variadic slot without ...
  | InvalidSpread TypeExpr -- ... applied to a non-SliceType value
  | NamedUnitParam Ident -- (x : ()) is invalid; use (x : struct{}) instead
  deriving (Eq, Show)

-- Build the full type for a let-binding given its params and declared return type.
-- For value bindings (empty params), returns retTy directly.
-- A sole anonymous UnitParam is a zero-param rewrite: FuncType [] Nothing retTy.
-- In any other combination, each UnitParam contributes struct{} to fixedTys.
letBindingType :: [Param] -> TypeExpr -> TypeExpr
letBindingType [] retTy = retTy -- non-function value
letBindingType [UnitParam] retTy = FuncType [] Nothing retTy -- zero-param rewrite
letBindingType ps retTy = FuncType fixedTys mVarTy retTy
  where
    isVariadic (VariadicParam _ _) = True
    isVariadic _ = False
    toFixedTy UnitParam = NamedType (Ident "()")
    toFixedTy (TypedParam _ ty) = ty
    toFixedTy (VariadicParam _ _) = error "letBindingType: impossible"
    fixedTys = map toFixedTy (filter (not . isVariadic) ps)
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
      | otherwise -> Right (FuncType (drop n fixed) Nothing ret)
    Just varTy
      | n > length fixed -> Right ret
      | n == length fixed -> Right (FuncType [] (Just varTy) ret)
      | otherwise -> Right (FuncType (drop n fixed) (Just varTy) ret)
applyType (NamedType (Ident "opaque")) _ = Right (NamedType (Ident "opaque"))
applyType t _ = Left (NotAFunction t)

-- Result type of an infix operator: comparisons and logical operators always
-- return bool; arithmetic/bitwise operators return the type of the left operand.
infixOpResultType :: T.Text -> TypeExpr -> TypeExpr -> TypeExpr
infixOpResultType "::" _ rhsTy = rhsTy
infixOpResultType op lhsTy _
  | op `elem` ["==", "!=", "<", ">", "<=", ">=", "&&", "||"] = NamedType (Ident "bool")
  | otherwise = lhsTy

-- Built-in names that are always in scope.
preludeEnv :: Env
preludeEnv =
  Map.fromList
    [ (Ident "true", NamedType (Ident "bool")),
      (Ident "false", NamedType (Ident "bool")),
      -- Go builtins — opaque type since fog lacks generics to express their polymorphism
      (Ident "len", NamedType (Ident "opaque")),
      (Ident "append", NamedType (Ident "opaque")),
      (Ident "delete", NamedType (Ident "opaque")),
      -- External Go functions (from util.go) — will be removed when fog can express these
      (Ident "mapInsert", NamedType (Ident "opaque")),
      (Ident "mapDelete", NamedType (Ident "opaque")),
      (Ident "intRange", NamedType (Ident "opaque"))
    ]

-- Reject named unit params (name : ())
-- () as a named param type has no representable function type due to zero-param
-- rewrite. Use (name : struct{}) instead.
checkNoNamedUnitParams :: [Param] -> Either ElabError ()
checkNoNamedUnitParams [] = Right ()
checkNoNamedUnitParams (TypedParam name (NamedType (Ident "()")) : _) = Left (NamedUnitParam name)
checkNoNamedUnitParams (_ : rest) = checkNoNamedUnitParams rest

-- Elaborate an expression in the given environment, producing a TExpr.
elabExpr :: Env -> Expr -> Either ElabError TExpr
elabExpr env (EVar i@(Ident t)) =
  case Map.lookup i env of
    Just ty -> Right (TEVar ty i)
    Nothing
      -- Qualified names (e.g. fmt.Println) have no fog type; treat as opaque.
      -- TODO: revisit when introducing multi-package fog (fog-defined qualified
      -- names will need to be resolved across package boundaries) and when
      -- improving Go integration (partially parsing Go package exports to read
      -- their types would let us give opaque names real types here).
      | "." `T.isInfixOf` t -> Right (TEVar (NamedType (Ident "opaque")) i)
      | otherwise -> Left (UnknownVariable i)
elabExpr _ EUnitLit =
  Right TEUnitLit
elabExpr _ (EIntLit lit) =
  Right (TEIntLit (NamedType (Ident "int")) lit)
elabExpr _ (EFloatLit lit) =
  Right (TEFloatLit (NamedType (Ident "float64")) lit)
elabExpr _ (EStrLit lit) =
  Right (TEStrLit (NamedType (Ident "string")) lit)
elabExpr env (EInfixOp e1 op e2) = do
  te1 <- elabExpr env e1
  te2 <- elabExpr env e2
  Right (TEInfixOp (infixOpResultType op (tExprType te1) (tExprType te2)) te1 op te2)
elabExpr env (EIf cond then' else') = do
  tcond <- elabExpr env cond
  tthen <- elabExpr env then'
  telse <- elabExpr env else'
  Right (TEIf (tExprType tthen) tcond tthen telse)
elabExpr env (ESequence exprs) = do
  texprs <- mapM (elabExpr env) exprs
  let ty = case texprs of
        [] -> NamedType (Ident "()")
        _ -> tExprType (last texprs)
  Right (TESequence ty texprs)
elabExpr env (ELambda (Binding params retTy body)) = do
  checkNoNamedUnitParams params
  let paramEnv =
        Map.fromList $
          [(name, ty) | TypedParam name ty <- params]
            ++ [(name, SliceType ty) | VariadicParam name ty <- params]
  tbody <- elabExpr (Map.union paramEnv env) body
  Right (TELambda (letBindingType params retTy) (TBinding params retTy tbody))
elabExpr env (ELet name (Binding params retTy rhs) inExpr) = do
  checkNoNamedUnitParams params
  let bindingTy = letBindingType params retTy
  let envWithSelf = Map.insert name bindingTy env
  let paramEnv =
        Map.fromList $
          [(n, ty) | TypedParam n ty <- params]
            ++ [(n, SliceType ty) | VariadicParam n ty <- params]
  trhs <- elabExpr (Map.union paramEnv envWithSelf) rhs
  tin <- elabExpr envWithSelf inExpr
  Right (TELet (tExprType tin) name (TBinding params retTy trhs) tin)
elabExpr env (EIndex e idx) = do
  te <- elabExpr env e
  tidx <- elabExpr env idx
  -- Result type: for slices, the element type; for maps, the value type; otherwise opaque
  let resultTy = case tExprType te of
        SliceType elemTy -> elemTy
        MapType _ valTy -> valTy
        _ -> NamedType (Ident "opaque")
  Right (TEIndex resultTy te tidx)
elabExpr env (ESliceLit exprs) = do
  texprs <- mapM (elabExpr env) exprs
  let elemTy = case texprs of
        [] -> NamedType (Ident "opaque")
        (te : _) -> tExprType te
  Right (TESliceLit (SliceType elemTy) texprs)
elabExpr _ EMapLit =
  Right (TEMapLit (NamedType (Ident "opaque")))
elabExpr env (EMatch scrut arms) = do
  tscrut <- elabExpr env scrut
  tarms <- mapM (elabArm env tscrut) arms
  let ty = case tarms of
        (TMatchArm _ body : _) -> tExprType body
        [] -> NamedType (Ident "()")
  Right (TEMatch ty tscrut tarms)
elabExpr env (EVariadicSpread e) = do
  te <- elabExpr env e
  case tExprType te of
    SliceType _ -> Right (TEVariadicSpread (tExprType te) te)
    ty -> Left (InvalidSpread ty)
elabExpr env (EApplication f args) = do
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
            [] -> pure ()
        else pure ()
    Nothing -> pure ()
  resultTy <- applyType (tExprType tf) (length targs)
  Right (TEApplication resultTy tf targs)

-- A bare SliceType value that is not wrapped in TEVariadicSpread is not allowed in a variadic slot.
isBareVariadic :: TExpr -> Bool
isBareVariadic TEUnitLit = False
isBareVariadic (TESliceLit _ _) = False
isBareVariadic te = case tExprType te of
  SliceType _ -> case te of
    TEVariadicSpread _ _ -> False
    _ -> True
  _ -> False

-- Elaborate a match arm: extract bindings from pattern, elaborate body in extended env.
elabArm :: Env -> TExpr -> MatchArm -> Either ElabError TMatchArm
elabArm env tscrut (MatchArm pat body) = do
  let tpat = elabPattern pat
      patBindings = patternBindingsTyped (tExprType tscrut) pat
      armEnv = Map.union (Map.fromList patBindings) env
  tbody <- elabExpr armEnv body
  Right (TMatchArm tpat tbody)

-- Convert a Pattern to a TPattern (no type info needed for now).
elabPattern :: Pattern -> TPattern
elabPattern PWildcard = TPWildcard
elabPattern (PVar i) = TPVar i
elabPattern (PIntLit lit) = TPIntLit lit
elabPattern (PBoolLit b) = TPBoolLit b
elabPattern PSliceEmpty = TPSliceEmpty
elabPattern (PCons hd tl) = TPCons (elabPattern hd) (elabPattern tl)
elabPattern (PTuple pats) = TPTuple (map elabPattern pats)

-- Extract variable bindings from a pattern, using the scrutinee type to give
-- pattern variables real types where possible. Falls back to opaque when the
-- scrutinee type doesn't provide enough information.
patternBindingsTyped :: TypeExpr -> Pattern -> [(Ident, TypeExpr)]
patternBindingsTyped _ PWildcard = []
patternBindingsTyped ty (PVar i) = [(i, ty)]
patternBindingsTyped _ (PIntLit _) = []
patternBindingsTyped _ (PBoolLit _) = []
patternBindingsTyped _ PSliceEmpty = []
patternBindingsTyped ty (PCons hd tl) =
  let elemTy = case ty of
        SliceType t -> t
        _ -> NamedType (Ident "opaque")
   in patternBindingsTyped elemTy hd ++ patternBindingsTyped ty tl
patternBindingsTyped _ (PTuple pats) =
  -- Tuple components come from Go multi-return (e.g. map comma-ok);
  -- no type information available per-component, so fall back to opaque.
  concatMap (patternBindingsTyped (NamedType (Ident "opaque"))) pats
