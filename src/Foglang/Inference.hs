module Foglang.Inference (InferError (..), inferAndResolve) where

import Control.Monad.State.Strict (StateT, get, gets, put, modify, runStateT, lift)
import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map
import Data.Semigroup (Max (..))
import Data.Text qualified as T
import Foglang.AST (Binding (..), Expr (..), Ident (..), MatchArm (..), Param (..), Pattern (..), TypeExpr (..), TypeSet (..), pattern UnitType, bindingType, exprPos, exprType, exprTypes)
import Text.Megaparsec.Pos (SourcePos)

-- Environment mapping names to their types.
type Env = Map.Map Ident TypeExpr

-- Substitution: maps TVar IDs to their resolved types
type Subst = Map.Map Int TypeExpr

data InferError
  = UnknownVariable SourcePos Ident
  | TypeMismatch SourcePos TypeExpr TypeExpr -- expected, actual
  | OccursCheck SourcePos Int TypeExpr
  | NotAFunction SourcePos TypeExpr
  | CannotInferType SourcePos
  | InvalidSpread SourcePos TypeExpr
  | NamedPUnit SourcePos Ident
  | MissingSpread SourcePos TypeExpr -- bare slice passed to variadic without ...
  deriving (Eq, Show)

-- Inference state: substitution + fresh TVar counter
data InferState = InferState
  { inferSubst :: !Subst
  , inferNextId :: !Int
  }

type Infer a = StateT InferState (Either InferError) a

freshTVar :: Infer TypeExpr
freshTVar = do
  st <- get
  put st { inferNextId = inferNextId st + 1 }
  return (TVar (inferNextId st))

freshConstrained :: TypeSet -> Infer TypeExpr
freshConstrained ts = do
  st <- get
  put st { inferNextId = inferNextId st + 1 }
  return (TConstrained (inferNextId st) ts)

getSubst :: Infer Subst
getSubst = gets inferSubst

putSubst :: Subst -> Infer ()
putSubst s = modify (\st -> st { inferSubst = s })

-- Wrapper: run pure unify, update state on success, throw on error
unifyM :: SourcePos -> TypeExpr -> TypeExpr -> Infer ()
unifyM pos t1 t2 = do
  s <- getSubst
  case unify pos s t1 t2 of
    Left err -> lift (Left err)
    Right s' -> putSubst s'

-- Wrapper: apply current substitution
applySubstM :: TypeExpr -> Infer TypeExpr
applySubstM ty = do
  s <- getSubst
  return (applySubst s ty)

-- Built-in names that are always in scope.
preludeEnv :: Env
preludeEnv =
  Map.fromList
    [ (Ident "true", TNamed (Ident "bool")),
      (Ident "false", TNamed (Ident "bool")),
      -- Go builtins — opaque type since fog lacks generics to express their polymorphism
      (Ident "len", TNamed (Ident "opaque")),
      (Ident "append", TNamed (Ident "opaque")),
      (Ident "delete", TNamed (Ident "opaque")),
      -- External Go functions (from util.go) — will be removed when fog can express these
      (Ident "mapInsert", TNamed (Ident "opaque")),
      (Ident "mapDelete", TNamed (Ident "opaque")),
      (Ident "intRange", TNamed (Ident "opaque"))
    ]

-- Apply substitution to a type, chasing TVar chains.
applySubst :: Subst -> TypeExpr -> TypeExpr
applySubst s (TVar n) =
  case Map.lookup n s of
    Just t -> applySubst s t -- chase chains
    Nothing -> TVar n
applySubst s (TSlice t) = TSlice (applySubst s t)
applySubst s (TMap k v) = TMap (applySubst s k) (applySubst s v)
applySubst s (TFunc params mVar ret) =
  TFunc (map (applySubst s) params) (fmap (applySubst s) mVar) (applySubst s ret)
applySubst s (TConstrained n ts) =
  case Map.lookup n s of
    Just t -> applySubst s t
    Nothing -> TConstrained n ts
applySubst _ t = t -- TNamed

-- Check if a TVar ID occurs in a type expression.
-- The type expression should already have substitution applied by the caller (unify').
occursIn :: Int -> TypeExpr -> Bool
occursIn n (TVar m) = n == m
occursIn n (TConstrained m _) = n == m
occursIn n (TSlice t) = occursIn n t
occursIn n (TMap k v) = occursIn n k || occursIn n v
occursIn n (TFunc ps mVar ret) =
  any (occursIn n) ps || maybe False (occursIn n) mVar || occursIn n ret
occursIn _ (TNamed _) = False

-- Is a type opaque or any (wildcard types that unify freely)?
isWildcard :: TypeExpr -> Bool
isWildcard (TNamed (Ident "opaque")) = True
isWildcard (TNamed (Ident "any")) = True
isWildcard _ = False

-- Do () and struct{} unify?
isUnitLike :: Ident -> Bool
isUnitLike (Ident "()") = True
isUnitLike (Ident "struct{}") = True
isUnitLike _ = False

-- Check if a named type belongs to a type set.
inTypeSet :: TypeSet -> Ident -> Bool
inTypeSet TSInt (Ident t) = t `elem` ["int", "int8", "int16", "int32", "int64",
                                       "uint", "uint8", "uint16", "uint32", "uint64",
                                       "uintptr", "byte", "rune"]
inTypeSet TSFloat (Ident t) = t `elem` ["float32", "float64"]

-- Intersect two type sets. Returns Nothing if disjoint.
intersectTypeSet :: TypeSet -> TypeSet -> Maybe TypeSet
intersectTypeSet TSInt TSInt = Just TSInt
intersectTypeSet TSFloat TSFloat = Just TSFloat
intersectTypeSet _ _ = Nothing

-- Operator classification
comparisonOps :: [T.Text]
comparisonOps = ["==", "!=", "<", ">", "<=", ">="]

logicalOps :: [T.Text]
logicalOps = ["&&", "||"]

-- Unify two types, returning updated substitution or error.
-- IMPORTANT: applies current substitution to both sides first.
unify :: SourcePos -> Subst -> TypeExpr -> TypeExpr -> Either InferError Subst
unify pos s t1 t2 = unify' pos s (applySubst s t1) (applySubst s t2)

-- Unify two types that have already had substitution applied.
unify' :: SourcePos -> Subst -> TypeExpr -> TypeExpr -> Either InferError Subst
-- Wildcard types unify freely with anything
unify' _ s t1 _ | isWildcard t1 = Right s
unify' _ s _ t2 | isWildcard t2 = Right s
-- TConstrained ~ TConstrained: intersect the sets
unify' pos s (TConstrained n s1) (TConstrained m s2)
  | n == m = Right s
  | otherwise = case intersectTypeSet s1 s2 of
      Just si -> Right (Map.insert m (TConstrained n si) s)
      Nothing -> Left (TypeMismatch pos (TConstrained n s1) (TConstrained m s2))
-- TConstrained ~ TNamed: check membership
unify' pos s (TConstrained n ts) (TNamed t)
  | inTypeSet ts t = Right (Map.insert n (TNamed t) s)
  | otherwise = Left (TypeMismatch pos (TConstrained n ts) (TNamed t))
unify' pos s (TNamed t) (TConstrained n ts)
  | inTypeSet ts t = Right (Map.insert n (TNamed t) s)
  | otherwise = Left (TypeMismatch pos (TNamed t) (TConstrained n ts))
-- TConstrained ~ TVar: propagate the constraint
unify' _ s (TConstrained n ts) (TVar m)
  | n == m = Right s
  | otherwise = Right (Map.insert m (TConstrained n ts) s)
unify' _ s (TVar m) (TConstrained n ts)
  | n == m = Right s
  | otherwise = Right (Map.insert m (TConstrained n ts) s)
-- TVar on left
unify' pos s (TVar n) t2
  | TVar n == t2 = Right s
  | occursIn n t2 = Left (OccursCheck pos n t2)
  | otherwise = Right (Map.insert n t2 s)
-- TVar on right
unify' pos s t1 (TVar n)
  | occursIn n t1 = Left (OccursCheck pos n t1)
  | otherwise = Right (Map.insert n t1 s)
-- Named types
unify' pos s (TNamed a) (TNamed b)
  | a == b = Right s
  | isUnitLike a && isUnitLike b = Right s
  | otherwise = Left (TypeMismatch pos (TNamed a) (TNamed b))
-- Slice types
unify' pos s (TSlice a) (TSlice b) = unify pos s a b
-- Map types
unify' pos s (TMap k1 v1) (TMap k2 v2) = do
  s' <- unify pos s k1 k2
  unify pos s' v1 v2
-- Function types
unify' pos s (TFunc as va ra) (TFunc bs vb rb)
  | length as /= length bs = Left (TypeMismatch pos (TFunc as va ra) (TFunc bs vb rb))
  | otherwise = do
      s' <- unifyList pos s as bs
      s'' <- case (va, vb) of
        (Nothing, Nothing) -> Right s'
        (Just a, Just b) -> unify pos s' a b
        _ -> Left (TypeMismatch pos (TFunc as va ra) (TFunc bs vb rb))
      unify pos s'' ra rb
-- Mismatched constructors
unify' pos _ t1 t2 = Left (TypeMismatch pos t1 t2)

-- Unify two lists element-wise.
unifyList :: SourcePos -> Subst -> [TypeExpr] -> [TypeExpr] -> Either InferError Subst
unifyList _ s [] [] = Right s
unifyList pos s (a : as) (b : bs) = do
  s' <- unify pos s a b
  unifyList pos s' as bs
unifyList _ _ _ _ = error "unifyList: impossible - caller must ensure equal lengths"

-- The result type when applying a function to nArgs arguments.
-- Collect up to n param types from a (possibly nested) function type for unification.
collectParamTypes :: Int -> TypeExpr -> [TypeExpr]
collectParamTypes 0 _ = []
collectParamTypes n (TFunc fixed Nothing ret)
  | n <= length fixed = take n fixed
  | otherwise = fixed ++ collectParamTypes (n - length fixed) ret
collectParamTypes n (TFunc fixed (Just _) _) = take n fixed  -- variadic stops flattening
collectParamTypes _ _ = []  -- non-function, will be caught by applyType

applyType :: SourcePos -> TypeExpr -> Int -> Either InferError TypeExpr
applyType pos (TFunc fixed mVar ret) n =
  case mVar of
    Nothing
      | n == length fixed -> Right ret
      | n < length fixed -> Right (TFunc (drop n fixed) Nothing ret)
      | otherwise -> applyType pos ret (n - length fixed)  -- recurse into return type
    Just varTy
      | n > length fixed -> Right ret
      | n == length fixed -> Right (TFunc [] (Just varTy) ret)
      | otherwise -> Right (TFunc (drop n fixed) (Just varTy) ret)
applyType _ (TNamed (Ident "opaque")) _ = Right (TNamed (Ident "opaque"))
applyType pos t _ = Left (NotAFunction pos t)

-- Reject named unit params (name : ())
-- () as a named param type has no representable function type due to zero-param rewrite.
-- Use (name : struct{}) instead.
checkNoNamedPUnits :: SourcePos -> [Param] -> Either InferError ()
checkNoNamedPUnits _ [] = Right ()
checkNoNamedPUnits pos (PTyped name UnitType : _) = Left (NamedPUnit pos name)
checkNoNamedPUnits pos (_ : rest) = checkNoNamedPUnits pos rest

-- Extract variable bindings from a pattern, using the scrutinee type to give
-- pattern variables their types. The scrutinee type should have substitution
-- applied before calling this function.
patternBindingsTyped :: TypeExpr -> Pattern -> Infer [(Ident, TypeExpr)]
patternBindingsTyped _ PtWildcard = return []
patternBindingsTyped ty (PtVar i) = return [(i, ty)]
patternBindingsTyped _ (PtIntLit _) = return []
patternBindingsTyped _ (PtBoolLit _) = return []
patternBindingsTyped _ PtSliceEmpty = return []
patternBindingsTyped ty (PtCons hd tl) = do
  elemTy <- case ty of
    TSlice t -> return t
    _ -> freshTVar
  hdBindings <- patternBindingsTyped elemTy hd
  tlBindings <- patternBindingsTyped ty tl
  return (hdBindings ++ tlBindings)
patternBindingsTyped _ (PtTuple pats) = do
  results <- mapM (\p -> do { tv <- freshTVar; patternBindingsTyped tv p }) pats
  return (concat results)

-- Generate constraints from a pattern against the scrutinee type.
-- Patterns like PtBoolLit constrain the scrutinee; others just bind variables.
unifyPattern :: SourcePos -> TypeExpr -> Pattern -> Infer ()
unifyPattern _ _ PtWildcard = return ()
unifyPattern _ _ (PtVar _) = return ()
unifyPattern pos scrutTy (PtBoolLit _) = unifyM pos scrutTy (TNamed (Ident "bool"))
unifyPattern pos scrutTy (PtIntLit _) = do
  tc <- freshConstrained TSInt
  unifyM pos scrutTy tc
unifyPattern pos scrutTy PtSliceEmpty = do
  elemTv <- freshTVar
  unifyM pos scrutTy (TSlice elemTv)
unifyPattern pos scrutTy (PtCons _ _) = do
  elemTv <- freshTVar
  unifyM pos scrutTy (TSlice elemTv)
unifyPattern _ _ (PtTuple _) = return ()

-- Result type of an infix operator: comparisons and logical operators always
-- return bool; cons returns rhs type; arithmetic/bitwise operators return lhs type.
infixOpResultType :: T.Text -> TypeExpr -> TypeExpr -> TypeExpr
infixOpResultType "::" _ rhsTy = rhsTy
infixOpResultType op lhsTy _
  | op `elem` comparisonOps || op `elem` logicalOps = TNamed (Ident "bool")
  | otherwise = lhsTy

-- The core inference function. Walks the Expr tree using the Infer monad.
inferExpr :: Env -> Expr -> Infer Expr
inferExpr env (EVar pos origTy i@(Ident t)) =
  case Map.lookup i env of
    Just ty -> do
      unifyM pos origTy ty
      return (EVar pos ty i)
    Nothing
      -- Qualified names (e.g. fmt.Println) have no fog type; treat as opaque.
      | "." `T.isInfixOf` t -> return (EVar pos (TNamed (Ident "opaque")) i)
      | otherwise -> lift (Left (UnknownVariable pos i))
inferExpr _ (EUnitLit pos) =
  return (EUnitLit pos)
inferExpr _ (EIntLit pos ty lit) =
  -- Keep the TVar if present — will be resolved by context or defaulted later
  return (EIntLit pos ty lit)
inferExpr _ (EFloatLit pos ty lit) =
  return (EFloatLit pos ty lit)
inferExpr _ (EStrLit pos ty lit) =
  return (EStrLit pos ty lit)
inferExpr env (EInfixOp pos _ e1 op e2) = do
  te1 <- inferExpr env e1
  te2 <- inferExpr env e2
  let lhsTy = exprType te1
      rhsTy = exprType te2
  case op of
    "::" -> unifyM pos rhsTy (TSlice lhsTy)
    _ | op `elem` comparisonOps ->
        unifyM pos lhsTy rhsTy
    _ | op `elem` logicalOps -> do
        unifyM pos lhsTy (TNamed (Ident "bool"))
        unifyM pos rhsTy (TNamed (Ident "bool"))
    _ -> -- arithmetic/bitwise
        unifyM pos lhsTy rhsTy
  s3 <- getSubst
  let resultTy = infixOpResultType op (applySubst s3 (exprType te1)) (applySubst s3 (exprType te2))
  return (EInfixOp pos resultTy te1 op te2)
inferExpr env (EIf pos _ cond then' else') = do
  tcond <- inferExpr env cond
  unifyM pos (exprType tcond) (TNamed (Ident "bool"))
  tthen <- inferExpr env then'
  telse <- inferExpr env else'
  unifyM pos (exprType tthen) (exprType telse)
  s5 <- getSubst
  return (EIf pos (applySubst s5 (exprType tthen)) tcond tthen telse)
inferExpr env (ESequence pos _ exprs) = do
  texprs <- inferExprs env exprs
  let ty = case texprs of
        [] -> UnitType
        _ -> exprType (last texprs)
  return (ESequence pos ty texprs)
inferExpr env (ELambda pos _ (Binding params retTy body)) = do
  lift (checkNoNamedPUnits pos params)
  let paramEnv =
        Map.fromList $
          [(name, ty) | PTyped name ty <- params]
            ++ [(name, TSlice ty) | PVariadic name ty <- params]
  tbody <- inferExpr (Map.union paramEnv env) body
  -- Unify the declared return type with the inferred body type
  unifyM pos retTy (exprType tbody)
  s2 <- getSubst
  let lambdaTy = bindingType params (applySubst s2 retTy)
  return (ELambda pos lambdaTy (Binding params retTy tbody))
inferExpr env (ELet pos _ name (Binding params retTy rhs) mInExpr) = do
  lift (checkNoNamedPUnits pos params)
  let bindingTy = bindingType params retTy
  let envWithSelf = Map.insert name bindingTy env
  let paramEnv =
        Map.fromList $
          [(n, ty) | PTyped n ty <- params]
            ++ [(n, TSlice ty) | PVariadic n ty <- params]
  trhs <- inferExpr (Map.union paramEnv envWithSelf) rhs
  -- Unify the declared return type with the inferred body type
  unifyM pos retTy (exprType trhs)
  -- Use resolved binding type for the continuation environment
  s2 <- getSubst
  let resolvedBindingTy = applySubst s2 bindingTy
  let envForCont = Map.insert name resolvedBindingTy env
  mtin <- traverse (inferExpr envForCont) mInExpr
  let resultTy = maybe UnitType exprType mtin
  return (ELet pos resultTy name (Binding params retTy trhs) mtin)
inferExpr env (EIndex pos _ e idx) = do
  te <- inferExpr env e
  tidx <- inferExpr env idx
  containerTy <- applySubstM (exprType te)
  case containerTy of
    TSlice elemTy -> do
      unifyM pos (exprType tidx) (TNamed (Ident "int"))
      return (EIndex pos elemTy te tidx)
    TMap keyTy valTy -> do
      unifyM pos (exprType tidx) keyTy
      return (EIndex pos valTy te tidx)
    _ -> -- opaque, TVar, or other — can't determine index/result types
      return (EIndex pos (TNamed (Ident "opaque")) te tidx)
inferExpr env (ESliceLit pos _ exprs) = do
  texprs <- inferExprs env exprs
  case texprs of
    [] -> do
      elemTv <- freshTVar
      return (ESliceLit pos (TSlice elemTv) texprs)
    (te : rest) -> do
      -- Unify all element types together
      mapM_ (\e' -> unifyM pos (exprType te) (exprType e')) rest
      s'' <- getSubst
      let elemTy = applySubst s'' (exprType te)
      return (ESliceLit pos (TSlice elemTy) texprs)
inferExpr _ (EMapLit pos _) = do
  kTv <- freshTVar
  vTv <- freshTVar
  return (EMapLit pos (TMap kTv vTv))
inferExpr env (EMatch pos _ scrut arms) = do
  tscrut <- inferExpr env scrut
  tarms <- inferArms env tscrut arms
  -- Unify all arm body types together
  case tarms of
    [] -> return ()
    (MatchArm _ _ firstBody : rest) ->
      mapM_ (\(MatchArm _ _ body) -> unifyM pos (exprType firstBody) (exprType body)) rest
  s3 <- getSubst
  let resultTy = case tarms of
        (MatchArm _ _ body : _) -> applySubst s3 (exprType body)
        [] -> UnitType
  return (EMatch pos resultTy tscrut tarms)
inferExpr env (EVariadicSpread pos _ e) = do
  te <- inferExpr env e
  s1 <- getSubst
  case applySubst s1 (exprType te) of
    st@(TSlice _) -> return (EVariadicSpread pos st te)
    ty@(TNamed (Ident "opaque")) -> return (EVariadicSpread pos ty te)
    ty@(TVar _) -> return (EVariadicSpread pos ty te) -- may resolve later
    ty -> lift (Left (InvalidSpread pos ty))
inferExpr env (EApplication pos _ f args) = do
  tf <- inferExpr env f
  targs <- inferExprs env args
  s2 <- getSubst
  let fTy = applySubst s2 (exprType tf)
  case fTy of
    TFunc fixed mVar _ -> do
      -- A zero-param non-variadic function called with () has nSupplied=1 from parser;
      -- treat the unit arg as a zero-arg call so we don't recurse into ret.
      let isUnitCall = null fixed && case (mVar, targs) of (Nothing, [EUnitLit _]) -> True; _ -> False
          nFixed = length fixed
          nSupplied = if isUnitCall then 0 else length targs
      -- Unify each arg with corresponding param type (including curried inner params)
      case mVar of
        Nothing -> do
          let allParamTys = collectParamTypes nSupplied fTy
              allPairs = zip targs allParamTys
          mapM_ (\(arg, paramTy) ->
                    unifyM pos (exprType arg) paramTy) allPairs
        Just varTy -> do
          -- Fixed params first
          let fixedPairs = zip targs fixed
          mapM_ (\(arg, paramTy) ->
                    unifyM pos (exprType arg) paramTy) fixedPairs
          -- Variadic args
          if nSupplied > nFixed
            then do
              let varArgs = drop nFixed targs
              mapM_ (\arg ->
                case arg of
                  EVariadicSpread {} -> unifyM pos (exprType arg) (TSlice varTy)
                  _ -> do
                    resolvedArgTy <- applySubstM (exprType arg)
                    case resolvedArgTy of
                      TSlice _ -> lift (Left (MissingSpread (exprPos arg) resolvedArgTy))
                      _ -> unifyM pos (exprType arg) varTy) varArgs
            else return ()
      -- Use post-unification function type for accurate result type
      s4 <- getSubst
      resultTy <- lift (applyType pos (applySubst s4 fTy) nSupplied)
      return (EApplication pos resultTy tf targs)
    nt@(TNamed (Ident "opaque")) -> do
      resultTy <- lift (applyType pos nt (length targs))
      return (EApplication pos resultTy tf targs)
    TVar _ -> do
      resultTv <- freshTVar
      let argTypes = map exprType targs
      unifyM pos fTy (TFunc argTypes Nothing resultTv)
      return (EApplication pos resultTv tf targs)
    _ -> lift (Left (NotAFunction pos fTy))

-- Infer a list of expressions left-to-right, threading substitution through
-- so that constraints from earlier expressions are visible to later ones.
inferExprs :: Env -> [Expr] -> Infer [Expr]
inferExprs env = mapM (inferExpr env)

-- Infer match arms, threading substitution.
-- Applies current substitution to the scrutinee type for each arm so that
-- pattern bindings see resolved types from earlier arms.
inferArms :: Env -> Expr -> [MatchArm] -> Infer [MatchArm]
inferArms _ _ [] = return []
inferArms env tscrut (MatchArm armPos pat body : rest) = do
  s <- getSubst
  let scrutTy = applySubst s (exprType tscrut)
      pos = exprPos tscrut
  -- Generate constraints from the pattern
  unifyPattern pos scrutTy pat
  s0 <- getSubst
  patBindings <- patternBindingsTyped (applySubst s0 scrutTy) pat
  let armEnv = Map.union (Map.fromList patBindings) env
  tbody <- inferExpr armEnv body
  trest <- inferArms env tscrut rest
  return (MatchArm armPos pat tbody : trest)

-- ---------------------------------------------------------------------------
-- Shared AST traversal combinators
-- ---------------------------------------------------------------------------

-- | Fold a monoid over every Expr node in the tree (depth-first, pre-order).
-- The caller supplies a per-node function; recursion into children is automatic.
foldMapExpr :: Monoid m => (Expr -> m) -> Expr -> m
foldMapExpr f expr = f expr <> children
  where
    children = case expr of
      EInfixOp _ _ e1 _ e2       -> foldMapExpr f e1 <> foldMapExpr f e2
      EIf _ _ c t e              -> foldMapExpr f c <> foldMapExpr f t <> foldMapExpr f e
      ESequence _ _ es           -> foldMap (foldMapExpr f) es
      ELambda _ _ (Binding _ _ body) -> foldMapExpr f body
      ELet _ _ _ (Binding _ _ rhs) mInE -> foldMapExpr f rhs <> foldMap (foldMapExpr f) mInE
      EIndex _ _ e idx           -> foldMapExpr f e <> foldMapExpr f idx
      ESliceLit _ _ es           -> foldMap (foldMapExpr f) es
      EVariadicSpread _ _ e      -> foldMapExpr f e
      EApplication _ _ fn args   -> foldMapExpr f fn <> foldMap (foldMapExpr f) args
      EMatch _ _ scrut arms      -> foldMapExpr f scrut <> foldMap (\(MatchArm _ _ body) -> foldMapExpr f body) arms
      _                          -> mempty  -- EVar, EUnitLit, EIntLit, EFloatLit, EStrLit, EMapLit

-- | Map a function over every TypeExpr in an Expr tree.
-- Transforms the type on each node, types inside Binding params, and Binding retTy.
-- Recursion into child Expr nodes is handled automatically.
mapExprTypes :: (TypeExpr -> TypeExpr) -> Expr -> Expr
mapExprTypes f = go
  where
    go (EVar p t i)            = EVar p (f t) i
    go (EUnitLit p)            = EUnitLit p
    go (EIntLit p t lit)       = EIntLit p (f t) lit
    go (EFloatLit p t lit)     = EFloatLit p (f t) lit
    go (EStrLit p t lit)       = EStrLit p (f t) lit
    go (EInfixOp p t e1 op e2) = EInfixOp p (f t) (go e1) op (go e2)
    go (EIf p t c th el)      = EIf p (f t) (go c) (go th) (go el)
    go (ESequence p t es)      = ESequence p (f t) (map go es)
    go (ELambda p t (Binding params retTy body)) =
      ELambda p (f t) (Binding (map goParam params) (f retTy) (go body))
    go (ELet p t name (Binding params retTy rhs) mInE) =
      ELet p (f t) name (Binding (map goParam params) (f retTy) (go rhs)) (fmap go mInE)
    go (EIndex p t e idx)      = EIndex p (f t) (go e) (go idx)
    go (ESliceLit p t es)      = ESliceLit p (f t) (map go es)
    go (EMapLit p t)           = EMapLit p (f t)
    go (EVariadicSpread p t e) = EVariadicSpread p (f t) (go e)
    go (EApplication p t fn args) = EApplication p (f t) (go fn) (map go args)
    go (EMatch p t scrut arms) = EMatch p (f t) (go scrut) (map goArm arms)

    goArm (MatchArm armPos pat body)  = MatchArm armPos pat (go body)

    goParam PUnit              = PUnit
    goParam (PTyped name ty)   = PTyped name (f ty)
    goParam (PVariadic name ty) = PVariadic name (f ty)

-- ---------------------------------------------------------------------------
-- Functions built on the combinators
-- ---------------------------------------------------------------------------

-- Apply substitution to every TypeExpr in an Expr tree.
applySubstExpr :: Subst -> Expr -> Expr
applySubstExpr s = mapExprTypes (applySubst s)

-- Two-pass literal defaulting:
-- Pass 1: Collect defaults from literal nodes (TVar ID -> concrete type)
-- Pass 2: Apply defaults as substitution, then check for remaining TVars

-- Collect default type mappings from literal nodes.
collectLiteralDefaults :: Expr -> Subst
collectLiteralDefaults = foldMapExpr collectNode
  where
    collectNode (EIntLit _ (TConstrained n TSInt) _)      = Map.singleton n (TNamed (Ident "int"))
    collectNode (EFloatLit _ (TConstrained n TSFloat) _) = Map.singleton n (TNamed (Ident "float64"))
    collectNode _                                        = Map.empty

-- Default remaining TVars to opaque, but only in targeted positions:
-- 1. Standalone TVars on expression nodes (e.g. from tuple pattern bindings
--    where Go's comma-ok idiom is not modeled in fog's type system).
-- 2. TVars nested inside collection types (TSlice, TMap), e.g. empty
--    slice/map literals whose element types were not constrained by context.
-- TVars that appear as direct parameters or return types of TFunc are NOT
-- defaulted — those indicate genuine inference failures and will be caught
-- by checkNoTVars as CannotInferType errors.


-- TODO: Case 1 (standalone TVars) exists because fog lacks a TupleType and
-- Go builtins like map access are opaque, so tuple-destructured variables
-- interacting only with opaque functions have no type constraints. Once fog
-- models Go builtin signatures with real types (or adds TupleType), this
-- blanket standalone-TVar defaulting can be removed.
defaultRemainingTVars :: Expr -> Subst
defaultRemainingTVars = foldMapExpr defaultNode
  where
    defaultNode expr = Map.unions (map goType (exprTypes expr))

    -- Walk into a type: default standalone TVars and TVars inside collections,
    -- but for TFunc params/returns, only default TVars inside collections.
    goType (TVar n) = Map.singleton n (TNamed (Ident "opaque"))
    goType (TConstrained n _) = Map.singleton n (TNamed (Ident "opaque"))
    goType (TSlice t) = defaultInCollection t
    goType (TMap k v) = Map.union (defaultInCollection k) (defaultInCollection v)
    goType (TFunc ps mVar ret) = Map.unions (map goCollectionOnly ps ++ maybe [] (\v -> [goCollectionOnly v]) mVar ++ [goCollectionOnly ret])
    goType (TNamed _) = Map.empty

    -- Inside TFunc params/returns: only default TVars inside collection types.
    goCollectionOnly (TSlice t) = defaultInCollection t
    goCollectionOnly (TMap k v) = Map.union (defaultInCollection k) (defaultInCollection v)
    goCollectionOnly (TFunc ps mVar ret) = Map.unions (map goCollectionOnly ps ++ maybe [] (\v -> [goCollectionOnly v]) mVar ++ [goCollectionOnly ret])
    goCollectionOnly _ = Map.empty

    -- Default a TVar that's inside a collection type
    defaultInCollection (TVar n) = Map.singleton n (TNamed (Ident "opaque"))
    defaultInCollection (TConstrained n _) = Map.singleton n (TNamed (Ident "opaque"))
    defaultInCollection other = goCollectionOnly other  -- recurse for nested types

-- Collect all remaining TVars in an Expr tree as errors.
checkNoTVars :: Expr -> [InferError]
checkNoTVars = foldMapExpr (\e -> let pos = exprPos e in concatMap (collectTVars pos) (exprTypes e))
  where
    collectTVars pos (TVar _)          = [CannotInferType pos]
    collectTVars pos (TConstrained _ _) = [CannotInferType pos]
    collectTVars pos (TSlice t)        = collectTVars pos t
    collectTVars pos (TMap k v)        = collectTVars pos k ++ collectTVars pos v
    collectTVars pos (TFunc ps mVar ret) =
      concatMap (collectTVars pos) ps ++ maybe [] (collectTVars pos) mVar ++ collectTVars pos ret
    collectTVars _ (TNamed _)          = []


-- Find the maximum TVar ID in an Expr tree, so we can seed the counter above it.
maxTVarExpr :: Expr -> Int
maxTVarExpr = getMax . foldMapExpr (\e -> Max (maximum (0 : map maxTy (exprTypes e))))
  where
    maxTy (TVar n) = n
    maxTy (TConstrained n _) = n
    maxTy (TSlice t) = maxTy t
    maxTy (TMap k v) = max (maxTy k) (maxTy v)
    maxTy (TFunc ps mVar ret) = maximum (0 : map maxTy ps ++ maybe [] (\v -> [maxTy v]) mVar ++ [maxTy ret])
    maxTy (TNamed _) = 0

-- Main entry: infer types for a top-level expression, apply substitution, default literals.
inferAndResolve :: Expr -> Either [InferError] Expr
inferAndResolve expr = do
  let initState = InferState Map.empty (maxTVarExpr expr + 1)
  (typedExpr, finalState) <- first (:[]) $ runStateT (inferExpr preludeEnv expr) initState
  let resolved = applySubstExpr (inferSubst finalState) typedExpr
  -- Pass 1: collect defaults from literal nodes
  let defaults = collectLiteralDefaults resolved
  -- Apply defaults as an additional substitution pass
  let defaulted = applySubstExpr defaults resolved
  -- Pass 2: default remaining TVars to opaque
  let collectionDefaults = defaultRemainingTVars defaulted
  let final = applySubstExpr collectionDefaults defaulted
  -- Pass 3: check no TVars remain
  case checkNoTVars final of
    [] -> Right final
    errs -> Left errs
