module Foglang.Inference (InferError (..), inferAndResolve) where

import Control.Monad.State.Strict (StateT, get, gets, put, modify, runStateT, lift)
import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map
import Data.Semigroup (Max (..))
import Data.Text qualified as T
import Foglang.AST (Binding (..), Coercion (..), Expr (..), ExprAnn (..), Ident (..), MatchArm (..), Param (..), Pattern (..), TypeExpr (..), TypeSet (..), pattern UnitType, bindingType, exprAnn, exprPos, exprType, exprTypes)
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
unifyM p t1 t2 = do
  s <- getSubst
  case unify p s t1 t2 of
    Left err -> lift (Left err)
    Right s' -> putSubst s'

-- Wrapper: apply current substitution
applySubstM :: TypeExpr -> Infer TypeExpr
applySubstM t = do
  s <- getSubst
  return (applySubst s t)



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
unify p s t1 t2 = unify' p s (applySubst s t1) (applySubst s t2)

-- Unify two types that have already had substitution applied.
unify' :: SourcePos -> Subst -> TypeExpr -> TypeExpr -> Either InferError Subst
-- Wildcard types unify freely with anything
unify' _ s t1 _ | isWildcard t1 = Right s
unify' _ s _ t2 | isWildcard t2 = Right s
-- TConstrained ~ TConstrained: intersect the sets
unify' p s (TConstrained n s1) (TConstrained m s2)
  | n == m = Right s
  | otherwise = case intersectTypeSet s1 s2 of
      Just si -> Right (Map.insert m (TConstrained n si) s)
      Nothing -> Left (TypeMismatch p (TConstrained n s1) (TConstrained m s2))
-- TConstrained ~ TNamed: check membership
unify' p s (TConstrained n ts) (TNamed t)
  | inTypeSet ts t = Right (Map.insert n (TNamed t) s)
  | otherwise = Left (TypeMismatch p (TConstrained n ts) (TNamed t))
unify' p s (TNamed t) (TConstrained n ts)
  | inTypeSet ts t = Right (Map.insert n (TNamed t) s)
  | otherwise = Left (TypeMismatch p (TNamed t) (TConstrained n ts))
-- TConstrained ~ TVar: propagate the constraint
unify' _ s (TConstrained n ts) (TVar m)
  | n == m = Right s
  | otherwise = Right (Map.insert m (TConstrained n ts) s)
unify' _ s (TVar m) (TConstrained n ts)
  | n == m = Right s
  | otherwise = Right (Map.insert m (TConstrained n ts) s)
-- TVar on left
unify' p s (TVar n) t2
  | TVar n == t2 = Right s
  | occursIn n t2 = Left (OccursCheck p n t2)
  | otherwise = Right (Map.insert n t2 s)
-- TVar on right
unify' p s t1 (TVar n)
  | occursIn n t1 = Left (OccursCheck p n t1)
  | otherwise = Right (Map.insert n t1 s)
-- Named types
unify' p s (TNamed a) (TNamed b)
  | a == b = Right s
  | isUnitLike a && isUnitLike b = Right s
  | otherwise = Left (TypeMismatch p (TNamed a) (TNamed b))
-- Slice types
unify' p s (TSlice a) (TSlice b) = unify p s a b
-- Map types
unify' p s (TMap k1 v1) (TMap k2 v2) = do
  s' <- unify p s k1 k2
  unify p s' v1 v2
-- Function types
unify' p s (TFunc as va ra) (TFunc bs vb rb)
  | length as /= length bs = Left (TypeMismatch p (TFunc as va ra) (TFunc bs vb rb))
  | otherwise = do
      s' <- unifyList p s as bs
      s'' <- case (va, vb) of
        (Nothing, Nothing) -> Right s'
        (Just a, Just b) -> unify p s' a b
        _ -> Left (TypeMismatch p (TFunc as va ra) (TFunc bs vb rb))
      unify p s'' ra rb
-- Mismatched constructors
unify' p _ t1 t2 = Left (TypeMismatch p t1 t2)

-- Unify two lists element-wise.
unifyList :: SourcePos -> Subst -> [TypeExpr] -> [TypeExpr] -> Either InferError Subst
unifyList _ s [] [] = Right s
unifyList p s (a : as) (b : bs) = do
  s' <- unify p s a b
  unifyList p s' as bs
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
applyType p (TFunc fixed mVar ret) n =
  case mVar of
    Nothing
      | n == length fixed -> Right ret
      | n < length fixed -> Right (TFunc (drop n fixed) Nothing ret)
      | otherwise -> applyType p ret (n - length fixed)  -- recurse into return type
    Just varTy
      | n > length fixed -> Right ret
      | n == length fixed -> Right (TFunc [] (Just varTy) ret)
      | otherwise -> Right (TFunc (drop n fixed) (Just varTy) ret)
applyType _ (TNamed (Ident "opaque")) _ = Right (TNamed (Ident "opaque"))
applyType p t _ = Left (NotAFunction p t)

-- Reject named unit params (name : ())
-- () as a named param type has no representable function type due to zero-param rewrite.
-- Use (name : struct{}) instead.
checkNoNamedPUnits :: SourcePos -> [Param] -> Either InferError ()
checkNoNamedPUnits _ [] = Right ()
checkNoNamedPUnits p (PTyped name UnitType : _) = Left (NamedPUnit p name)
checkNoNamedPUnits p (_ : rest) = checkNoNamedPUnits p rest

-- Extract variable bindings from a pattern, using the scrutinee type to give
-- pattern variables their types. The scrutinee type should have substitution
-- applied before calling this function.
patternBindingsTyped :: TypeExpr -> Pattern -> Infer [(Ident, TypeExpr)]
patternBindingsTyped _ PtWildcard = return []
patternBindingsTyped t (PtVar i) = return [(i, t)]
patternBindingsTyped _ (PtIntLit _) = return []
patternBindingsTyped _ (PtStrLit _) = return []
patternBindingsTyped _ (PtBoolLit _) = return []
patternBindingsTyped _ PtSliceEmpty = return []
patternBindingsTyped t (PtCons hd tl) = do
  elemTy <- case t of
    TSlice et -> return et
    _ -> freshTVar
  hdBindings <- patternBindingsTyped elemTy hd
  tlBindings <- patternBindingsTyped t tl
  return (hdBindings ++ tlBindings)
patternBindingsTyped _ (PtTuple pats) = do
  results <- mapM (\p -> do { tv <- freshTVar; patternBindingsTyped tv p }) pats
  return (concat results)

-- Generate constraints from a pattern against the scrutinee type.
-- Patterns like PtBoolLit constrain the scrutinee; others just bind variables.
unifyPattern :: SourcePos -> TypeExpr -> Pattern -> Infer ()
unifyPattern _ _ PtWildcard = return ()
unifyPattern _ _ (PtVar _) = return ()
unifyPattern p scrutTy (PtBoolLit _) = unifyM p scrutTy (TNamed (Ident "bool"))
unifyPattern p scrutTy (PtIntLit _) = do
  tc <- freshConstrained TSInt
  unifyM p scrutTy tc
unifyPattern p scrutTy (PtStrLit _) = unifyM p scrutTy (TNamed (Ident "string"))
unifyPattern p scrutTy PtSliceEmpty = do
  elemTv <- freshTVar
  unifyM p scrutTy (TSlice elemTv)
unifyPattern p scrutTy (PtCons _ _) = do
  elemTv <- freshTVar
  unifyM p scrutTy (TSlice elemTv)
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
inferExpr env (EVar ExprAnn{pos = p, ty = origTy} i@(Ident t)) =
  case Map.lookup i env of
    Just envTy -> do
      unifyM p origTy envTy
      return (EVar ExprAnn { pos = p, ty = envTy, isStmt = False } i)
    Nothing
      -- Qualified names (e.g. fmt.Println) have no fog type; treat as opaque.
      | "." `T.isInfixOf` t -> return (EVar ExprAnn { pos = p, ty = TNamed (Ident "opaque"), isStmt = False } i)
      | otherwise -> lift (Left (UnknownVariable p i))
inferExpr _ (EUnitLit a) =
  return (EUnitLit a)
inferExpr _ (EIntLit a lit) =
  -- Keep the TVar if present — will be resolved by context or defaulted later
  return (EIntLit a lit)
inferExpr _ (EFloatLit a lit) =
  return (EFloatLit a lit)
inferExpr _ (EStrLit a lit) =
  return (EStrLit a lit)
inferExpr env (EInfixOp ExprAnn{pos = p} e1 op e2) = do
  te1 <- inferExpr env e1
  te2 <- inferExpr env e2
  let lhsTy = exprType te1
      rhsTy = exprType te2
  case op of
    "::" -> unifyM p rhsTy (TSlice lhsTy)
    _ | op `elem` comparisonOps ->
        unifyM p lhsTy rhsTy
    _ | op `elem` logicalOps -> do
        unifyM p lhsTy (TNamed (Ident "bool"))
        unifyM p rhsTy (TNamed (Ident "bool"))
    _ -> -- arithmetic/bitwise
        unifyM p lhsTy rhsTy
  s3 <- getSubst
  let resultTy = infixOpResultType op (applySubst s3 (exprType te1)) (applySubst s3 (exprType te2))
  return (EInfixOp ExprAnn { pos = p, ty = resultTy, isStmt = False } te1 op te2)
inferExpr env (EIf ExprAnn{pos = p} cond then' else') = do
  tcond <- inferExpr env cond
  unifyM p (exprType tcond) (TNamed (Ident "bool"))
  tthen <- inferExpr env then'
  telse <- inferExpr env else'
  unifyM p (exprType tthen) (exprType telse)
  s5 <- getSubst
  return (EIf ExprAnn { pos = p, ty = applySubst s5 (exprType tthen), isStmt = False } tcond tthen telse)
inferExpr env (ESequence ExprAnn{pos = p} exprs) = do
  texprs <- inferExprs env exprs
  let resultTy = case texprs of
        [] -> UnitType
        _ -> exprType (last texprs)
  return (ESequence ExprAnn { pos = p, ty = resultTy, isStmt = False } texprs)
inferExpr env (ELambda ExprAnn{pos = p} (Binding params retTy body)) = do
  lift (checkNoNamedPUnits p params)
  let paramEnv =
        Map.fromList $
          [(name, t) | PTyped name t <- params]
            ++ [(name, TSlice t) | PVariadic name t <- params]
  tbody <- inferExpr (Map.union paramEnv env) body
  -- Unify the declared return type with the inferred body type
  unifyM p retTy (exprType tbody)
  s2 <- getSubst
  let lambdaTy = bindingType params (applySubst s2 retTy)
  return (ELambda ExprAnn { pos = p, ty = lambdaTy, isStmt = False } (Binding params retTy tbody))
inferExpr env (ELet ExprAnn{pos = p} name (Binding params retTy rhs) mInExpr) = do
  lift (checkNoNamedPUnits p params)
  let bindTy = bindingType params retTy
  let envWithSelf = Map.insert name bindTy env
  let paramEnv =
        Map.fromList $
          [(n, t) | PTyped n t <- params]
            ++ [(n, TSlice t) | PVariadic n t <- params]
  trhs <- inferExpr (Map.union paramEnv envWithSelf) rhs
  -- Unify the declared return type with the inferred body type
  unifyM p retTy (exprType trhs)
  -- Use resolved binding type for the continuation environment
  s2 <- getSubst
  let resolvedBindTy = applySubst s2 bindTy
  let envForCont = Map.insert name resolvedBindTy env
  mtin <- traverse (inferExpr envForCont) mInExpr
  let resultTy = maybe UnitType exprType mtin
  return (ELet ExprAnn { pos = p, ty = resultTy, isStmt = True } name (Binding params retTy trhs) mtin)
inferExpr env (EIndex ExprAnn{pos = p} e idx) = do
  te <- inferExpr env e
  tidx <- inferExpr env idx
  containerTy <- applySubstM (exprType te)
  case containerTy of
    TSlice elemTy -> do
      unifyM p (exprType tidx) (TNamed (Ident "int"))
      return (EIndex ExprAnn { pos = p, ty = elemTy, isStmt = False } te tidx)
    TMap keyTy valTy -> do
      unifyM p (exprType tidx) keyTy
      return (EIndex ExprAnn { pos = p, ty = valTy, isStmt = False } te tidx)
    _ -> -- opaque, TVar, or other — can't determine index/result types
      return (EIndex ExprAnn { pos = p, ty = TNamed (Ident "opaque"), isStmt = False } te tidx)
inferExpr env (ESliceLit ExprAnn{pos = p} exprs) = do
  texprs <- inferExprs env exprs
  case texprs of
    [] -> do
      elemTv <- freshTVar
      return (ESliceLit ExprAnn { pos = p, ty = TSlice elemTv, isStmt = False } texprs)
    (te : rest) -> do
      -- Unify all element types together
      mapM_ (\e' -> unifyM p (exprType te) (exprType e')) rest
      s'' <- getSubst
      let elemTy = applySubst s'' (exprType te)
      return (ESliceLit ExprAnn { pos = p, ty = TSlice elemTy, isStmt = False } texprs)
inferExpr _ (EMapLit ExprAnn{pos = p}) = do
  kTv <- freshTVar
  vTv <- freshTVar
  return (EMapLit ExprAnn { pos = p, ty = TMap kTv vTv, isStmt = False })
inferExpr env (EMatch ExprAnn{pos = p} scrut arms) = do
  tscrut <- inferExpr env scrut
  tarms <- inferArms env tscrut arms
  -- Unify all arm body types together
  case tarms of
    [] -> return ()
    (MatchArm _ _ firstBody : rest) ->
      mapM_ (\(MatchArm _ _ body) -> unifyM p (exprType firstBody) (exprType body)) rest
  s3 <- getSubst
  let resultTy = case tarms of
        (MatchArm _ _ body : _) -> applySubst s3 (exprType body)
        [] -> UnitType
  return (EMatch ExprAnn { pos = p, ty = resultTy, isStmt = False } tscrut tarms)
-- ECoerce is inserted post-inference; it should never appear during inference.
inferExpr _ (ECoerce ExprAnn{pos = p} _ _) = error $ "inferExpr: unexpected ECoerce at " <> show p
inferExpr env (EVariadicSpread ExprAnn{pos = p} e) = do
  te <- inferExpr env e
  s1 <- getSubst
  case applySubst s1 (exprType te) of
    st@(TSlice _) -> return (EVariadicSpread ExprAnn { pos = p, ty = st, isStmt = False } te)
    t@(TNamed (Ident "opaque")) -> return (EVariadicSpread ExprAnn { pos = p, ty = t, isStmt = False } te)
    t@(TVar _) -> return (EVariadicSpread ExprAnn { pos = p, ty = t, isStmt = False } te) -- may resolve later
    t -> lift (Left (InvalidSpread p t))
inferExpr env (EApplication ExprAnn{pos = p} f args) = do
  tf <- inferExpr env f
  targs <- inferExprs env args
  s2 <- getSubst
  let fTy = applySubst s2 (exprType tf)
  case fTy of
    TFunc fixed mVar _ -> do
      -- `f ()` is ambiguous at parse time — EApplication f [EUnitLit] could mean:
      --   1. Call zero-param non-variadic function (() is call syntax)
      --   2. Call variadic function with no variadic args (() is empty sentinel)
      --   3. Call function with one arg, the unit value ()
      -- Only the type of f disambiguates, so we detect case 1 here.
      let isZeroArgCall = null fixed && case (mVar, targs) of (Nothing, [EUnitLit _]) -> True; _ -> False
          nFixed = length fixed
          nSupplied = if isZeroArgCall then 0 else length targs
      -- Unify each arg with corresponding param type (including curried inner params)
      case mVar of
        Nothing -> do
          let allParamTys = collectParamTypes nSupplied fTy
              allPairs = zip targs allParamTys
          mapM_ (\(arg, paramTy) ->
                    unifyM p (exprType arg) paramTy) allPairs
        Just varTy -> do
          -- Fixed params first
          let fixedPairs = zip targs fixed
          mapM_ (\(arg, paramTy) ->
                    unifyM p (exprType arg) paramTy) fixedPairs
          -- Variadic args: a lone () after fixed args is the "no variadic args"
          -- sentinel from the parser (same ambiguity as isZeroArgCall above).
          let varArgs = drop nFixed targs
              isZeroVariadicCall = case varArgs of [EUnitLit _] -> True; _ -> False
          if not isZeroVariadicCall && nSupplied > nFixed
            then
              mapM_ (\arg ->
                case arg of
                  EVariadicSpread {} -> unifyM p (exprType arg) (TSlice varTy)
                  _ -> do
                    resolvedArgTy <- applySubstM (exprType arg)
                    case resolvedArgTy of
                      TSlice _ -> lift (Left (MissingSpread (exprPos arg) resolvedArgTy))
                      _ -> unifyM p (exprType arg) varTy) varArgs
            else return ()
      -- Use post-unification function type for accurate result type
      s4 <- getSubst
      resultTy <- lift (applyType p (applySubst s4 fTy) nSupplied)
      return (EApplication ExprAnn { pos = p, ty = resultTy, isStmt = True } tf targs)
    nt@(TNamed (Ident "opaque")) -> do
      resultTy <- lift (applyType p nt (length targs))
      return (EApplication ExprAnn { pos = p, ty = resultTy, isStmt = True } tf targs)
    TVar _ -> do
      resultTv <- freshTVar
      let argTypes = map exprType targs
      unifyM p fTy (TFunc argTypes Nothing resultTv)
      return (EApplication ExprAnn { pos = p, ty = resultTv, isStmt = True } tf targs)
    _ -> lift (Left (NotAFunction p fTy))

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
      p = exprPos tscrut
  -- Generate constraints from the pattern
  unifyPattern p scrutTy pat
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
      EInfixOp _ e1 _ e2          -> foldMapExpr f e1 <> foldMapExpr f e2
      EIf _ c t e                 -> foldMapExpr f c <> foldMapExpr f t <> foldMapExpr f e
      ESequence _ es              -> foldMap (foldMapExpr f) es
      ELambda _ (Binding _ _ body) -> foldMapExpr f body
      ELet _ _ (Binding _ _ rhs) mInE -> foldMapExpr f rhs <> foldMap (foldMapExpr f) mInE
      EIndex _ e idx              -> foldMapExpr f e <> foldMapExpr f idx
      ESliceLit _ es              -> foldMap (foldMapExpr f) es
      EVariadicSpread _ e         -> foldMapExpr f e
      EApplication _ fn args      -> foldMapExpr f fn <> foldMap (foldMapExpr f) args
      EMatch _ scrut arms         -> foldMapExpr f scrut <> foldMap (\(MatchArm _ _ body) -> foldMapExpr f body) arms
      ECoerce _ _ inner           -> foldMapExpr f inner
      _                           -> mempty  -- EVar, EUnitLit, EIntLit, EFloatLit, EStrLit, EMapLit

-- | Map a function over every TypeExpr in an Expr tree.
-- Transforms the type on each node, types inside Binding params, and Binding retTy.
-- Recursion into child Expr nodes is handled automatically.
mapExprTypes :: (TypeExpr -> TypeExpr) -> Expr -> Expr
mapExprTypes f = go
  where
    go (EVar a@ExprAnn{ty = t} i)            = EVar a{ty = f t} i
    go (EUnitLit a)                           = EUnitLit a
    go (EIntLit a@ExprAnn{ty = t} lit)        = EIntLit a{ty = f t} lit
    go (EFloatLit a@ExprAnn{ty = t} lit)      = EFloatLit a{ty = f t} lit
    go (EStrLit a@ExprAnn{ty = t} lit)        = EStrLit a{ty = f t} lit
    go (EInfixOp a@ExprAnn{ty = t} e1 op e2) = EInfixOp a{ty = f t} (go e1) op (go e2)
    go (EIf a@ExprAnn{ty = t} c th el)       = EIf a{ty = f t} (go c) (go th) (go el)
    go (ESequence a@ExprAnn{ty = t} es)       = ESequence a{ty = f t} (map go es)
    go (ELambda a@ExprAnn{ty = t} (Binding params retTy body)) =
      ELambda a{ty = f t} (Binding (map goParam params) (f retTy) (go body))
    go (ELet a@ExprAnn{ty = t} name (Binding params retTy rhs) mInE) =
      ELet a{ty = f t} name (Binding (map goParam params) (f retTy) (go rhs)) (fmap go mInE)
    go (EIndex a@ExprAnn{ty = t} e idx)       = EIndex a{ty = f t} (go e) (go idx)
    go (ESliceLit a@ExprAnn{ty = t} es)       = ESliceLit a{ty = f t} (map go es)
    go (EMapLit a@ExprAnn{ty = t})            = EMapLit a{ty = f t}
    go (EVariadicSpread a@ExprAnn{ty = t} e)  = EVariadicSpread a{ty = f t} (go e)
    go (EApplication a@ExprAnn{ty = t} fn args) = EApplication a{ty = f t} (go fn) (map go args)
    go (EMatch a@ExprAnn{ty = t} scrut arms)  = EMatch a{ty = f t} (go scrut) (map goArm arms)
    go (ECoerce a@ExprAnn{ty = t} c inner)     = ECoerce a{ty = f t} c (go inner)

    goArm (MatchArm armPos pat body)  = MatchArm armPos pat (go body)

    goParam PUnit              = PUnit
    goParam (PTyped name t)    = PTyped name (f t)
    goParam (PVariadic name t) = PVariadic name (f t)

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
    collectNode (EIntLit ExprAnn{ty = TConstrained n TSInt} _)    = Map.singleton n (TNamed (Ident "int"))
    collectNode (EFloatLit ExprAnn{ty = TConstrained n TSFloat} _) = Map.singleton n (TNamed (Ident "float64"))
    collectNode _                                                  = Map.empty

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
defaultRemainingTVars = foldMapExpr (\expr -> Map.unions (map (walk True) (exprTypes expr)))
  where
    -- Walk a type, defaulting TVars to opaque when `defTVars` is True.
    -- Collections (TSlice, TMap) enable defaulting; TFunc disables it
    -- (unconstrained func params/returns are real inference errors).
    walk True  (TVar n)           = Map.singleton n (TNamed (Ident "opaque"))
    walk True  (TConstrained n _) = Map.singleton n (TNamed (Ident "opaque"))
    walk False (TVar _)           = Map.empty
    walk False (TConstrained _ _) = Map.empty
    walk _     (TSlice t)         = walk True t
    walk _     (TMap k v)         = Map.union (walk True k) (walk True v)
    walk _     (TFunc ps mVar ret) =
      Map.unions (map (walk False) ps ++ maybe [] (\v -> [walk False v]) mVar ++ [walk False ret])
    walk _     (TNamed _)         = Map.empty

-- Collect all remaining TVars in an Expr tree as errors.
checkNoTVars :: Expr -> [InferError]
checkNoTVars = foldMapExpr (\e -> let p = exprPos e in concatMap (collectTVars p) (exprTypes e))
  where
    collectTVars p (TVar _)          = [CannotInferType p]
    collectTVars p (TConstrained _ _) = [CannotInferType p]
    collectTVars p (TSlice t)        = collectTVars p t
    collectTVars p (TMap k v)        = collectTVars p k ++ collectTVars p v
    collectTVars p (TFunc ps mVar ret) =
      concatMap (collectTVars p) ps ++ maybe [] (collectTVars p) mVar ++ collectTVars p ret
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

-- Post-order traversal: go each node's isStmt based on whether it or any
-- child can standalone as go statement. EApplication and ELet are inherently
-- statement-valid; all other nodes are statement-valid if any child is.
computeIsStmt :: Expr -> Expr
computeIsStmt = go
  where
    go (EApplication a f args) =
      let f' = go f; args' = map go args
      in EApplication a{isStmt = True} f' args'
    go (ELet a name (Binding ps retTy rhs) mtin) =
      let rhs' = go rhs; mtin' = fmap go mtin
      in ELet a{isStmt = True} name (Binding ps retTy rhs') mtin'
    go (EIf a c th el) =
      let c' = go c; th' = go th; el' = go el
          s = isStmt (exprAnn c') || isStmt (exprAnn th') || isStmt (exprAnn el')
      in EIf a{isStmt = s} c' th' el'
    go (EMatch a scrut arms) =
      let scrut' = go scrut
          arms' = map goArm arms
          s = isStmt (exprAnn scrut') || any (\(MatchArm _ _ body) -> isStmt (exprAnn body)) arms'
      in EMatch a{isStmt = s} scrut' arms'
    go (ESequence a es) =
      let es' = map go es
          s = any (isStmt . exprAnn) es'
      in ESequence a{isStmt = s} es'
    go (EInfixOp a e1 op e2) =
      let e1' = go e1; e2' = go e2
          s = isStmt (exprAnn e1') || isStmt (exprAnn e2')
      in EInfixOp a{isStmt = s} e1' op e2'
    go (ELambda a (Binding ps retTy body)) =
      let body' = go body
      in ELambda a (Binding ps retTy body')
    go (EIndex a e idx) =
      let e' = go e; idx' = go idx
          s = isStmt (exprAnn e') || isStmt (exprAnn idx')
      in EIndex a{isStmt = s} e' idx'
    go (ESliceLit a es) =
      let es' = map go es
          s = any (isStmt . exprAnn) es'
      in ESliceLit a{isStmt = s} es'
    go (EVariadicSpread a e) =
      let e' = go e
      in EVariadicSpread a{isStmt = isStmt (exprAnn e')} e'
    go e@(EVar _ _) = e
    go e@(EIntLit _ _) = e
    go e@(EFloatLit _ _) = e
    go e@(EStrLit _ _) = e
    go e@(EUnitLit _) = e
    go e@(EMapLit _) = e
    go (ECoerce a c inner) =
      let inner' = go inner
      in ECoerce a{isStmt = isStmt (exprAnn inner')} c inner'

    goArm (MatchArm p pat body) = MatchArm p pat (go body)

-- Post-inference pass: insert ECoerce at type boundaries where function
-- return types disagree in the unit<->struct{} dimension.
insertCoercions :: Expr -> Expr
insertCoercions = go
  where
    -- Do two function types differ only in their return type's unit<->struct{} name?
    funcVoidMismatch (TFunc _ _ (TNamed r1)) (TFunc _ _ (TNamed r2)) =
      isUnitLike r1 && isUnitLike r2 && r1 /= r2
    funcVoidMismatch _ _ = False

    coerceIfNeeded expectedTy expr
      | funcVoidMismatch expectedTy (exprType expr) =
          ECoerce (exprAnn expr){ty = expectedTy} FuncVoidCoerce expr
      | otherwise = expr

    -- Type boundaries: coerce where declared types meet expression types.
    go (ELet a name (Binding params retTy rhs) mtin) =
      ELet a name (Binding params retTy (coerceIfNeeded retTy (go rhs))) (fmap go mtin)
    go (ELambda a (Binding params retTy body)) =
      ELambda a (Binding params retTy (coerceIfNeeded retTy (go body)))
    go (EApplication a tf targs) =
      let tf' = go tf
      in EApplication a tf' (coerceArgs (exprType tf') targs)

    -- Recurse into children.
    go (EIf a c th el) = EIf a (go c) (go th) (go el)
    go (EMatch a scrut arms) = EMatch a (go scrut) (map goArm arms)
    go (ESequence a es) = ESequence a (map go es)
    go (EInfixOp a e1 op e2) = EInfixOp a (go e1) op (go e2)
    go (EIndex a e idx) = EIndex a (go e) (go idx)
    go (ESliceLit a es) = ESliceLit a (map go es)
    go (EVariadicSpread a e) = EVariadicSpread a (go e)
    go (ECoerce a c inner) = ECoerce a c (go inner)
    go e@(EVar {}) = e
    go e@(EUnitLit {}) = e
    go e@(EIntLit {}) = e
    go e@(EFloatLit {}) = e
    go e@(EStrLit {}) = e
    go e@(EMapLit {}) = e

    goArm (MatchArm p pat body) = MatchArm p pat (go body)

    -- Coerce fixed args where param types disagree; pass variadic args through.
    coerceArgs (TFunc fixedTys _ _) args =
      zipWith (\t a -> coerceIfNeeded t (go a)) fixedTys args
        ++ map go (drop (length fixedTys) args)
    coerceArgs _ args = map go args

-- Main entry: runs the full inference pipeline on a parsed Expr tree.
--
--   1. Infer types (constraint generation + unification)
--   2. Apply substitution to resolve TVars
--   3. Default literal types (int literals → int, float literals → float64)
--   4. Default remaining TVars in collection positions to opaque
--   5. Reject any unresolved TVars as errors
--   6. Compute isStmt annotations for codegen
inferAndResolve :: Expr -> Either [InferError] Expr
inferAndResolve expr = do
  let initState = InferState { inferSubst = Map.empty, inferNextId = maxTVarExpr expr + 1 }

  -- Step 1: infer
  (inferred, finalState) <- first (:[]) $ runStateT (inferExpr preludeEnv expr) initState

  -- Step 2: apply substitution
  let resolved = applySubstExpr (inferSubst finalState) inferred

  -- Step 3: default literal types
  let afterLiteralDefaults = applySubstExpr (collectLiteralDefaults resolved) resolved

  -- Step 4: default remaining TVars in collections to opaque
  let afterCollectionDefaults = applySubstExpr (defaultRemainingTVars afterLiteralDefaults) afterLiteralDefaults

  -- Step 5: reject unresolved TVars
  case checkNoTVars afterCollectionDefaults of
    [] -> pure ()
    errs -> Left errs

  -- Step 6: compute statement annotations
  let afterIsStmt = computeIsStmt afterCollectionDefaults

  -- Step 7: insert unit<->struct{} coercion wrappers
  Right (insertCoercions afterIsStmt)
