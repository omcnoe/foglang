module Foglang.Inference (InferError (..), inferAndResolve, prettyInferError) where

import Control.Monad.State.Strict (StateT, get, gets, put, modify, runStateT, lift)
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)
import Foglang.AST (Binding (..), Coercion (..), Expr (..), ExprAnn (..), Ident (..), MatchArm (..), Param (..), Pattern (..), TypeExpr (..), TypeSet (..), pattern UnitType, bindingType, exprAnn, exprPos, exprType, exprTypes, tsInt, isWildcard, isUnitLike)
import Foglang.Parser (ParserState (..))

-- Environment maps names to their types.
type Env = Map.Map Ident TypeExpr

-- Substitution: maps TypeVar IDs to their resolved types
type Subst = Map.Map Int TypeExpr
-- TODO: model of Subst is fundamentally flawed and imposes unworkable scaling limitations on inference.
-- Subst = Map Int TypeExpr where TypeExpr can itself contain TypeVars means every entry is potentially
-- a pointer into other entries, and the map as a whole forms a directed graph. Chasing that
-- graph causes quadratic cost for pathological (and not so pathological) inputs.
-- Need to refactor design to cleanly separate "link/pointer (type variables)" substitutions from "root/concrete (actual type)" substitutions in the subst map.

data InferError
  = UnknownVariable SourcePos Ident
  | TypeMismatch SourcePos TypeExpr TypeExpr -- expected, actual
  | InfiniteType SourcePos Int TypeExpr -- TypeVar appears inside the type it's being unified with (not recursive function, but "structurally infinite" type)
  | NotAFunction SourcePos TypeExpr
  | NotAnIndexable SourcePos TypeExpr -- tried to index a non-indexable concrete type
  | CannotInferType SourcePos
  | NamedPUnit SourcePos Ident
  | MissingSpread SourcePos TypeExpr -- bare slice passed to variadic without ...
  deriving (Eq, Show)

-- Inference state: substitution + fresh TypeVar counter
data InferState = InferState { inferSubst :: !Subst, iNextTypeVarId :: !Int }

-- StateT to allow short circuit on first inference error via Either
type Infer = StateT InferState (Either InferError)

freshTypeVarId :: Infer Int
freshTypeVarId = do
  s <- get
  put s { iNextTypeVarId = iNextTypeVarId s + 1 }
  pure (iNextTypeVarId s)

freshTypeVar :: Infer TypeExpr
freshTypeVar = TypeVar <$> freshTypeVarId

freshTypeVarConstrained :: TypeSet -> Infer TypeExpr
freshTypeVarConstrained ts = (`TypeVarConstrained` ts) <$> freshTypeVarId

freshTypeVarIndexable :: TypeExpr -> TypeExpr -> Infer TypeExpr
freshTypeVarIndexable k v = (\i -> TypeVarIndexable i k v) <$> freshTypeVarId

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

-- Apply substitution map to a type expression, chasing TypeVar chains.
applySubst :: Subst -> TypeExpr -> TypeExpr
applySubst _ (TNamed t) = (TNamed t)
applySubst s (TSlice t) = TSlice (applySubst s t)
applySubst s (TMap k v) = TMap (applySubst s k) (applySubst s v)
applySubst s (TFunc params mVar ret) = TFunc (map (applySubst s) params) (fmap (applySubst s) mVar) (applySubst s ret)
-- chase chains:
applySubst s (TypeVar n) =
  case Map.lookup n s of
    Just t -> applySubst s t
    Nothing -> TypeVar n
applySubst s (TypeVarConstrained n ts) =
  case Map.lookup n s of
    Just t -> applySubst s t
    Nothing -> TypeVarConstrained n ts
applySubst s (TypeVarIndexable n k v) =
  case Map.lookup n s of
    Just t -> applySubst s t
    Nothing -> TypeVarIndexable n (applySubst s k) (applySubst s v)

-- Built-in names that are always in scope.
-- TODO better approach for this
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
      -- TODO need to eventually remove ALL usages of "opaque"
    ]

-- Unify two types, returning updated substitution or error.
-- Applies current substitution to both sides before matching.
unify :: SourcePos -> Subst -> TypeExpr -> TypeExpr -> Either InferError Subst
unify p s rawT1 rawT2 =
  let mismatch = Left (TypeMismatch p t1 t2)

      t1 = applySubst s rawT1
      t2 = applySubst s rawT2

      -- Check if a TypeVar ID occurs inside it's own definition - illegal infinite type.
      -- The type expression should already have substitution applied by the caller (unify').
      isInfiniteType :: Int -> TypeExpr -> Bool
      isInfiniteType _ (TNamed _) = False
      isInfiniteType n (TSlice t) = isInfiniteType n t
      isInfiniteType n (TMap k v) = isInfiniteType n k || isInfiniteType n v
      isInfiniteType n (TFunc ps mVar ret) =
        any (isInfiniteType n) ps || maybe False (isInfiniteType n) mVar || isInfiniteType n ret
      isInfiniteType n (TypeVar m) = n == m
      isInfiniteType n (TypeVarConstrained m _) = n == m
      isInfiniteType n (TypeVarIndexable m k v) = n == m || isInfiniteType n k || isInfiniteType n v

      bindTypeVar n t
        | TypeVar n == t  = Right s
        | isInfiniteType n t = Left (InfiniteType p n t)
        | otherwise    = Right (Map.insert n t s)
      resolveTypeVarConstrained n ts t
        | t `Set.member` tsMembers ts = Right (Map.insert n (TNamed t) s)
        | otherwise = mismatch
      bindTypeVarConstrained n ts m
        | n == m    = Right s
        | otherwise = Right (Map.insert m (TypeVarConstrained n ts) s)
      bindTypeVarIndexable n kTy vTy m
        | n == m    = Right s
        | isInfiniteType m (TypeVarIndexable n kTy vTy) = Left (InfiniteType p m (TypeVarIndexable n kTy vTy))
        | otherwise = Right (Map.insert m (TypeVarIndexable n kTy vTy) s)
      unifyPairwise sub [] [] = Right sub
      unifyPairwise sub (a : as') (b : bs') = do
        sub' <- unify p sub a b
        unifyPairwise sub' as' bs'
      unifyPairwise _ _ _ = mismatch
  in case (t1, t2) of
    -- Wildcard types unify freely with anything
    _ | isWildcard t1 || isWildcard t2 -> Right s
    -- Unit-like types unify freely with other unit-like
    _ | isUnitLike t1 && isUnitLike t2 -> Right s
    -- TypeVarConstrained ~ TypeVarConstrained: must be the same type set
    (TypeVarConstrained n ts1, TypeVarConstrained m ts2)
      | n == m    -> Right s
      | ts1 == ts2 -> Right (Map.insert m (TypeVarConstrained n ts1) s)
      | otherwise -> mismatch
    -- TypeVarConstrained ~ TNamed: check membership
    (TypeVarConstrained n ts, TNamed t) -> resolveTypeVarConstrained n ts t
    (TNamed t, TypeVarConstrained n ts) -> resolveTypeVarConstrained n ts t
    -- TypeVarConstrained ~ TypeVar: propagate the constraint
    (TypeVarConstrained n ts, TypeVar m) -> bindTypeVarConstrained n ts m
    (TypeVar m, TypeVarConstrained n ts) -> bindTypeVarConstrained n ts m
    -- Numeric literals (TypeVarConstrained) are not indexable.
    (TypeVarConstrained {}, TypeVarIndexable {}) -> Left (NotAnIndexable p t1)
    (TypeVarIndexable {}, TypeVarConstrained {}) -> Left (NotAnIndexable p t2)
    -- TypeVarIndexable ~ TypeVarIndexable: merge by unifying keys and values.
    (TypeVarIndexable n k1 v1, TypeVarIndexable m k2 v2)
      | n == m    -> Right s
      | otherwise -> do
          s' <- unify p s k1 k2
          s'' <- unify p s' v1 v2
          Right (Map.insert m (TypeVarIndexable n k1 v1) s'')
    -- TypeVarIndexable ~ TSlice: key must be int, value must be the slice elem.
    (TypeVarIndexable n k v, TSlice elemTy) -> do
      s' <- unify p s k (TNamed (Ident "int"))
      s'' <- unify p s' v elemTy
      Right (Map.insert n (TSlice elemTy) s'')
    (TSlice elemTy, TypeVarIndexable n k v) -> do
      s' <- unify p s k (TNamed (Ident "int"))
      s'' <- unify p s' v elemTy
      Right (Map.insert n (TSlice elemTy) s'')
    -- TypeVarIndexable ~ TMap: key unifies with map key, value with map value.
    (TypeVarIndexable n k v, TMap mk mv) -> do
      s' <- unify p s k mk
      s'' <- unify p s' v mv
      Right (Map.insert n (TMap mk mv) s'')
    (TMap mk mv, TypeVarIndexable n k v) -> do
      s' <- unify p s k mk
      s'' <- unify p s' v mv
      Right (Map.insert n (TMap mk mv) s'')
    -- TypeVarIndexable ~ string: key must be int, value must be byte.
    (TypeVarIndexable n k v, TNamed (Ident "string")) -> do
      s' <- unify p s k (TNamed (Ident "int"))
      s'' <- unify p s' v (TNamed (Ident "byte"))
      Right (Map.insert n (TNamed (Ident "string")) s'')
    (TNamed (Ident "string"), TypeVarIndexable n k v) -> do
      s' <- unify p s k (TNamed (Ident "int"))
      s'' <- unify p s' v (TNamed (Ident "byte"))
      Right (Map.insert n (TNamed (Ident "string")) s'')
    -- TypeVarIndexable ~ TypeVar: propagate the constraint.
    (TypeVarIndexable n k v, TypeVar m) -> bindTypeVarIndexable n k v m
    (TypeVar m, TypeVarIndexable n k v) -> bindTypeVarIndexable n k v m
    -- TypeVarIndexable ~ concrete non-container: not indexable.
    (TypeVarIndexable {}, TNamed _) -> Left (NotAnIndexable p t2) -- TODO better type system needed to allow named type indexing
    (TNamed _, TypeVarIndexable {}) -> Left (NotAnIndexable p t1)
    (TypeVarIndexable {}, TFunc {}) -> Left (NotAnIndexable p t2)
    (TFunc {}, TypeVarIndexable {}) -> Left (NotAnIndexable p t1)
    -- TypeVar: bind to the other type (with infinite type definition check)
    (TypeVar n, _) -> bindTypeVar n t2
    (_, TypeVar n) -> bindTypeVar n t1
    -- Named types
    (TNamed a, TNamed b)
      | a == b -> Right s
      | otherwise -> mismatch
    -- Slice types
    (TSlice a, TSlice b) -> unify p s a b
    -- Map types
    (TMap k1 v1, TMap k2 v2) -> do
      s' <- unify p s k1 k2
      unify p s' v1 v2
    -- Function types
    (TFunc as va ra, TFunc bs vb rb) -> do
      s' <- unifyPairwise s as bs
      s'' <- case (va, vb) of
        (Nothing, Nothing) -> Right s'
        (Just a, Just b)   -> unify p s' a b
        _                  -> mismatch
      unify p s'' ra rb
    -- Mismatched constructors
    _ -> mismatch

-- The core inference function. Walks the Expr tree using the Infer monad.
inferExpr :: Env -> Expr -> Infer Expr
inferExpr env expr = case expr of
  EVar a i              -> inferVar a i
  EUnitLit _            -> return expr
  EIntLit {}            -> return expr
  EFloatLit {}          -> return expr
  EStrLit {}            -> return expr
  EInfixOp a e1 op e2   -> inferInfixOp a e1 op e2
  EIf a c th el         -> inferIf a c th el
  ESequence a es        -> inferSequence a es
  ELambda a b           -> inferLambda a b
  ELet a name b mIn     -> inferLet a name b mIn
  EIndex a e idx        -> inferIndex a e idx
  ESliceLit a es        -> inferSliceLit a es
  EMapLit a             -> inferMapLit a
  EMatch a scrut arms   -> inferMatch a scrut arms
  ECoerce ExprAnn{pos = p} _ _ -> error $ "inferExpr: unexpected ECoerce at " <> show p
  EVariadicSpread a e   -> inferSpread a e
  EApplication a f args -> inferApp a f args
  where
    infer :: Expr -> Infer Expr
    infer = inferExpr env

    -- Reject (name : ()) params — () as a named param type has no representable
    -- function type due to the zero-param rewrite. Use (name : struct{}) instead.
    rejectNamedUnitParams :: SourcePos -> [Param] -> Infer ()
    rejectNamedUnitParams _ [] = return ()
    rejectNamedUnitParams p (PTyped name UnitType : _) = lift (Left (NamedPUnit p name))
    rejectNamedUnitParams p (_ : rest) = rejectNamedUnitParams p rest

    -- Build the type environment introduced by a parameter list.
    paramEnvOf :: [Param] -> Env
    paramEnvOf params = Map.fromList $
      [(name, t) | PTyped name t <- params]
        ++ [(name, TSlice t) | PVariadic name t <- params]

    inferVar ExprAnn{pos = p, ty = origTy} i@(Ident t) =
      case Map.lookup i env of
        Just envTy -> do
          unifyM p origTy envTy
          return (EVar ExprAnn { pos = p, ty = envTy, isStmt = False } i)
        Nothing
          | "." `T.isInfixOf` t -> return (EVar ExprAnn { pos = p, ty = TNamed (Ident "opaque"), isStmt = False } i)
          | otherwise -> lift (Left (UnknownVariable p i))

    inferInfixOp ExprAnn{pos = p} e1 op e2 = do
      te1 <- infer e1
      te2 <- infer e2
      let lhsTy = exprType te1
          rhsTy = exprType te2
      -- All ops unify lhs with rhs, except :: which unifies rhs with [lhs]
      case op of
        "::" -> unifyM p rhsTy (TSlice lhsTy)
        _    -> unifyM p lhsTy rhsTy
      s <- getSubst
      let resultTy = case op of
            "::" -> applySubst s rhsTy
            _ | op `elem` ["==", "!=", "<", ">", "<=", ">=", "&&", "||"] -> TNamed (Ident "bool")
            _ -> applySubst s lhsTy
      return (EInfixOp ExprAnn { pos = p, ty = resultTy, isStmt = False } te1 op te2)

    inferIf ExprAnn{pos = p} cond then' else' = do
      tcond <- infer cond
      unifyM p (exprType tcond) (TNamed (Ident "bool"))
      tthen <- infer then'
      telse <- infer else'
      unifyM p (exprType tthen) (exprType telse)
      s <- getSubst
      return (EIf ExprAnn { pos = p, ty = applySubst s (exprType tthen), isStmt = False } tcond tthen telse)

    inferSequence ExprAnn{pos = p} exprs = do
      texprs <- mapM infer exprs
      let resultTy = case texprs of
            [] -> UnitType
            _  -> exprType (last texprs)
      return (ESequence ExprAnn { pos = p, ty = resultTy, isStmt = False } texprs)

    inferLambda ExprAnn{pos = p} (Binding params retTy body) = do
      rejectNamedUnitParams p params
      tbody <- inferExpr (Map.union (paramEnvOf params) env) body
      unifyM p retTy (exprType tbody)
      s <- getSubst
      return (ELambda ExprAnn { pos = p, ty = bindingType params (applySubst s retTy), isStmt = False } (Binding params retTy tbody))

    inferLet ExprAnn{pos = p} name (Binding params retTy rhs) mInExpr = do
      rejectNamedUnitParams p params
      let bindTy = bindingType params retTy
      let envWithSelf = Map.insert name bindTy env
      trhs <- inferExpr (Map.union (paramEnvOf params) envWithSelf) rhs
      unifyM p retTy (exprType trhs)
      s <- getSubst
      let envForCont = Map.insert name (applySubst s bindTy) env
      mtin <- traverse (inferExpr envForCont) mInExpr
      return (ELet ExprAnn { pos = p, ty = maybe UnitType exprType mtin, isStmt = True } name (Binding params retTy trhs) mtin)

    inferIndex ExprAnn{pos = p} e idx = do
      te <- infer e
      tidx <- infer idx
      containerTy <- applySubstM (exprType te)
      case containerTy of
        TNamed (Ident "opaque") ->
          return (EIndex ExprAnn { pos = p, ty = TNamed (Ident "opaque"), isStmt = False } te tidx)
        _ -> do
          valTy <- freshTypeVar
          cTy <- freshTypeVarIndexable (exprType tidx) valTy
          unifyM p containerTy cTy
          return (EIndex ExprAnn { pos = p, ty = valTy, isStmt = False } te tidx)

    inferSliceLit ExprAnn{pos = p} exprs = do
      texprs <- mapM infer exprs
      case texprs of
        [] -> do
          elemTv <- freshTypeVar
          return (ESliceLit ExprAnn { pos = p, ty = TSlice elemTv, isStmt = False } texprs)
        (te : rest) -> do
          mapM_ (\e' -> unifyM p (exprType te) (exprType e')) rest
          s <- getSubst
          return (ESliceLit ExprAnn { pos = p, ty = TSlice (applySubst s (exprType te)), isStmt = False } texprs)

    inferMapLit ExprAnn{pos = p} = do
      kTv <- freshTypeVar
      vTv <- freshTypeVar
      return (EMapLit ExprAnn { pos = p, ty = TMap kTv vTv, isStmt = False })

    inferMatch ExprAnn{pos = p} scrut arms = do
      tscrut <- infer scrut
      tarms <- inferArms tscrut arms
      case tarms of
        [] -> return ()
        (MatchArm _ _ firstBody : rest) ->
          mapM_ (\(MatchArm _ _ body) -> unifyM p (exprType firstBody) (exprType body)) rest
      s <- getSubst
      let resultTy = case tarms of
            (MatchArm _ _ body : _) -> applySubst s (exprType body)
            [] -> UnitType
      return (EMatch ExprAnn { pos = p, ty = resultTy, isStmt = False } tscrut tarms)
      where
        inferArms :: Expr -> [MatchArm] -> Infer [MatchArm]
        inferArms _ [] = return []
        inferArms tscrut (MatchArm armPos pat body : rest) = do
          s <- getSubst
          let scrutTy = applySubst s (exprType tscrut)
          constrainPattern armPos scrutTy pat
          s' <- getSubst
          patBindings <- patternBindings (applySubst s' scrutTy) pat
          let armEnv = Map.union (Map.fromList patBindings) env
          tbody <- inferExpr armEnv body
          trest <- inferArms tscrut rest
          return (MatchArm armPos pat tbody : trest)

        -- Generate constraints from a pattern against the scrutinee type.
        constrainPattern :: SourcePos -> TypeExpr -> Pattern -> Infer ()
        constrainPattern _ _ PtWildcard = return ()
        constrainPattern _ _ (PtVar _) = return ()
        constrainPattern p' scrutTy (PtBoolLit _) = unifyM p' scrutTy (TNamed (Ident "bool"))
        constrainPattern p' scrutTy (PtIntLit _) = do
          tc <- freshTypeVarConstrained tsInt
          unifyM p' scrutTy tc
        constrainPattern p' scrutTy (PtStrLit _) = unifyM p' scrutTy (TNamed (Ident "string"))
        constrainPattern p' scrutTy PtSliceEmpty = do
          elemTv <- freshTypeVar
          unifyM p' scrutTy (TSlice elemTv)
        constrainPattern p' scrutTy (PtCons _ _) = do
          elemTv <- freshTypeVar
          unifyM p' scrutTy (TSlice elemTv)
        constrainPattern _ _ (PtTuple _) = return ()

        -- Extract variable bindings from a pattern, using the scrutinee type
        -- to give pattern variables their types.
        patternBindings :: TypeExpr -> Pattern -> Infer [(Ident, TypeExpr)]
        patternBindings _ PtWildcard = return []
        patternBindings t (PtVar i) = return [(i, t)]
        patternBindings _ (PtIntLit _) = return []
        patternBindings _ (PtStrLit _) = return []
        patternBindings _ (PtBoolLit _) = return []
        patternBindings _ PtSliceEmpty = return []
        patternBindings t (PtCons hd tl) = do
          elemTy <- case t of
            TSlice et -> return et
            _ -> freshTypeVar
          hdBindings <- patternBindings elemTy hd
          tlBindings <- patternBindings t tl
          return (hdBindings ++ tlBindings)
        patternBindings _ (PtTuple pats) = do
          results <- mapM (\pat -> do { tv <- freshTypeVar; patternBindings tv pat }) pats
          return (concat results)

    inferSpread ExprAnn{pos = p} e = do
      te <- infer e
      containerTy <- applySubstM (exprType te)
      case containerTy of
        TNamed (Ident "opaque") ->
          return (EVariadicSpread ExprAnn { pos = p, ty = containerTy, isStmt = False } te)
        _ -> do
          elemTv <- freshTypeVar
          unifyM p containerTy (TSlice elemTv)
          return (EVariadicSpread ExprAnn { pos = p, ty = TSlice elemTv, isStmt = False } te)

    inferApp ExprAnn{pos = p} f args = do
      tf <- infer f
      targs <- mapM infer args
      s <- getSubst
      let fTy = applySubst s (exprType tf)
      case fTy of
        TFunc fixed mVar _ -> inferKnownApp p tf targs fTy fixed mVar
        -- TODO: opaque calls skip all argument type checking. This means
        -- e.g. `fmt.Println args...` where args : []int generates code that
        -- Go rejects ([]int is not []any). Needs proper import signatures
        -- and interface subtyping to fix; until then, caught at Go compile time.
        nt@(TNamed (Ident "opaque")) -> do
          resultTy <- lift (resultType p nt (length targs))
          return (EApplication ExprAnn { pos = p, ty = resultTy, isStmt = True } tf targs)
        TypeVar _ -> do
          resultTv <- freshTypeVar
          unifyM p fTy (TFunc (map exprType targs) Nothing resultTv)
          return (EApplication ExprAnn { pos = p, ty = resultTv, isStmt = True } tf targs)
        _ -> lift (Left (NotAFunction p fTy))

    -- Infer application where the function type is a known TFunc.
    inferKnownApp p tf targs fTy fixed mVar = do
      let nFixed = length fixed
          nSupplied = length targs
      -- Unify fixed args with fixed param types
      mapM_ (\(arg, paramTy) -> unifyM p (exprType arg) paramTy) (zip targs fixed)
      -- Unify variadic args (if any)
      case mVar of
        Nothing -> return ()
        Just varTy -> do
          let varArgs = drop nFixed targs
          -- A lone () after fixed args is the parser's sentinel for "no variadic
          -- args provided" (e.g. `f 1 ()` on a variadic function means call with
          -- one fixed arg and zero variadic args).
          let isEmptyVariadic = case varArgs of [EUnitLit _] -> True; _ -> False
          if not isEmptyVariadic && nSupplied > nFixed
            then mapM_ (unifyVarArg p varTy) varArgs
            else return ()
      s' <- getSubst
      resultTy <- lift (resultType p (applySubst s' fTy) nSupplied)
      return (EApplication ExprAnn { pos = p, ty = resultTy, isStmt = True } tf targs)

    -- Unify a single variadic argument, checking for missing spread.
    unifyVarArg p varTy arg = case arg of
      EVariadicSpread {} -> unifyM p (exprType arg) (TSlice varTy)
      _ -> do
        resolvedArgTy <- applySubstM (exprType arg)
        case resolvedArgTy of
          TSlice _ -> lift (Left (MissingSpread (exprPos arg) resolvedArgTy))
          _ -> unifyM p (exprType arg) varTy

    -- The result type after applying n arguments to a function type.
    -- Partial application (n < fixed params) returns a TFunc with remaining params.
    -- Excess args beyond fixed params is a type error (no auto-currying into return type).
    resultType :: SourcePos -> TypeExpr -> Int -> Either InferError TypeExpr
    resultType p (TFunc fixed mVar ret) n = case mVar of
      Nothing
        | n == length fixed -> Right ret
        | n < length fixed  -> Right (TFunc (drop n fixed) Nothing ret)
        | otherwise         -> Left (NotAFunction p ret)
      Just varTy
        | n > length fixed  -> Right ret
        | n == length fixed -> Right (TFunc [] (Just varTy) ret)
        | otherwise         -> Right (TFunc (drop n fixed) (Just varTy) ret)
    resultType _ (TNamed (Ident "opaque")) _ = Right (TNamed (Ident "opaque"))
    resultType p t _ = Left (NotAFunction p t)

-- Main entry: runs the full inference pipeline on a parsed Expr tree.
--
--   1. Infer types (constraint generation + unification)
--   2. Apply substitution to resolve TypeVars
--   3. Default any remaining type variables
--   4. Reject any unresolved TypeVars as errors
--   5. Compute isStmt annotations for codegen
--   6. Insert unit<->struct{} coercion wrappers
-- | Run inference and resolution over an expression tree.
-- Takes ParserState for TypeVar minting.
inferAndResolve :: (Expr, ParserState) -> Either [InferError] Expr
inferAndResolve (expr, pState) = do
  -- Map a function over every node in Expr tree (bottom up).
  let
    mapExpr :: (Expr -> Expr) -> Expr -> Expr
    mapExpr f = go
      where
        go e = f $ case e of
          EInfixOp a e1 op e2         -> EInfixOp a (go e1) op (go e2)
          EIf a c th el               -> EIf a (go c) (go th) (go el)
          ESequence a es              -> ESequence a (map go es)
          ELambda a (Binding ps rt body) -> ELambda a (Binding ps rt (go body))
          ELet a name (Binding ps rt rhs) mInE -> ELet a name (Binding ps rt (go rhs)) (fmap go mInE)
          EIndex a e' idx             -> EIndex a (go e') (go idx)
          ESliceLit a es              -> ESliceLit a (map go es)
          EVariadicSpread a e'        -> EVariadicSpread a (go e')
          EApplication a fn args      -> EApplication a (go fn) (map go args)
          EMatch a scrut arms         -> EMatch a (go scrut) (map (\(MatchArm armPos pat body) -> MatchArm armPos pat (go body)) arms)
          ECoerce a c inner           -> ECoerce a c (go inner)
          -- no children
          EUnitLit {}                 -> e
          EVar {}                     -> e
          EIntLit {}                  -> e
          EFloatLit {}                -> e
          EStrLit {}                  -> e
          EMapLit {}                  -> e

  -- Fold over AST with monoid (depth-first, pre-order).
  let
    foldMapExpr :: Monoid m => (Expr -> m) -> Expr -> m
    foldMapExpr f expr' = f expr' <> children
      where
        children = case expr' of
          EInfixOp _ e1 _ e2          -> foldMapExpr f e1 <> foldMapExpr f e2
          EIf _ c t e'                -> foldMapExpr f c <> foldMapExpr f t <> foldMapExpr f e'
          ESequence _ es              -> foldMap (foldMapExpr f) es
          ELambda _ (Binding _ _ body) -> foldMapExpr f body
          ELet _ _ (Binding _ _ rhs) mInE -> foldMapExpr f rhs <> foldMap (foldMapExpr f) mInE
          EIndex _ e' idx             -> foldMapExpr f e' <> foldMapExpr f idx
          ESliceLit _ es              -> foldMap (foldMapExpr f) es
          EVariadicSpread _ e'        -> foldMapExpr f e'
          EApplication _ fn args      -> foldMapExpr f fn <> foldMap (foldMapExpr f) args
          EMatch _ scrut arms         -> foldMapExpr f scrut <> foldMap (\(MatchArm _ _ body) -> foldMapExpr f body) arms
          ECoerce _ _ inner           -> foldMapExpr f inner
          -- no children
          EUnitLit {}                 -> mempty
          EVar {}                     -> mempty
          EIntLit {}                  -> mempty
          EFloatLit {}                -> mempty
          EStrLit {}                  -> mempty
          EMapLit {}                  -> mempty

  -- Apply substitution to every TypeExpr in an Expr tree.
  let applySubstExpr :: Subst -> Expr -> Expr
      applySubstExpr s = mapExpr go
        where
          sub = applySubst s
          go (EVar a@ExprAnn{ty = t} i) = EVar a{ty = sub t} i
          go (EIntLit a@ExprAnn{ty = t} lit) = EIntLit a{ty = sub t} lit
          go (EFloatLit a@ExprAnn{ty = t} lit) = EFloatLit a{ty = sub t} lit
          go (EStrLit a@ExprAnn{ty = t} lit) = EStrLit a{ty = sub t} lit
          go (EInfixOp a@ExprAnn{ty = t} e1 op e2) = EInfixOp a{ty = sub t} e1 op e2
          go (EIf a@ExprAnn{ty = t} c th el) = EIf a{ty = sub t} c th el
          go (ESequence a@ExprAnn{ty = t} es) = ESequence a{ty = sub t} es
          go (ELambda a@ExprAnn{ty = t} (Binding ps rt body)) =
            ELambda a{ty = sub t} (Binding (map goParam ps) (sub rt) body)
          go (ELet a@ExprAnn{ty = t} name (Binding ps rt rhs) mInE) =
            ELet a{ty = sub t} name (Binding (map goParam ps) (sub rt) rhs) mInE
          go (EIndex a@ExprAnn{ty = t} e idx) = EIndex a{ty = sub t} e idx
          go (ESliceLit a@ExprAnn{ty = t} es) = ESliceLit a{ty = sub t} es
          go (EVariadicSpread a@ExprAnn{ty = t} e) = EVariadicSpread a{ty = sub t} e
          go (EApplication a@ExprAnn{ty = t} fn args) = EApplication a{ty = sub t} fn args
          go (EMatch a@ExprAnn{ty = t} scrut arms) = EMatch a{ty = sub t} scrut arms
          go (ECoerce a@ExprAnn{ty = t} c inner) = ECoerce a{ty = sub t} c inner
          go e@(EUnitLit _) = e
          go (EMapLit a@ExprAnn{ty = t}) = EMapLit a{ty = sub t}
          goParam PUnit = PUnit
          goParam (PTyped name t) = PTyped name (sub t)
          goParam (PVariadic name t) = PVariadic name (sub t)

  -- Default remaining unresolved type variables.
  -- Iterative process, only thinks about one layer of defaulting at a time,
  -- which is critical for TypeVarIndexable case - the Indexable can't be defaulted
  -- until it's key type has been.
  -- TODO: blanket TypeVar -> opaque defaulting exists because fog's type system
  -- is underbaked: it lacks eg. a TupleType and Go builtins like map access are
  -- opaque, so vars interacting only with opaque builtins
  -- have no type constraints. Once fog models Go builtin signatures properly,
  -- this blanket default can be removed.

  let defaultLoop :: Expr -> Expr
      defaultLoop e
        | Map.null defaultSubst = e
        | otherwise             = defaultLoop (applySubstExpr defaultSubst e)
        where
          defaultSubst :: Subst
          defaultSubst = foldMapExpr (\e' -> foldMap defaultType (exprTypes e')) e

          defaultType (TypeVarConstrained n ts) = Map.singleton n (TNamed (tsDefault ts))
          defaultType (TypeVarIndexable n k v) =
            let indexableDefault = case k of
                  TNamed name | name `Set.member` tsMembers tsInt -> Map.singleton n (TSlice v)
                  TNamed _                                        -> Map.singleton n (TMap k v)
                  -- Key not yet concrete (TypeVar, TypeVarConstrained, structurally
                  -- invalid): wait. Either a later pass resolves it, or it
                  -- survives to checkNoTypeVars.
                  _                                               -> Map.empty
            in indexableDefault <> defaultType k <> defaultType v
          defaultType (TypeVar n) = Map.singleton n (TNamed (Ident "opaque"))
          defaultType (TSlice t) = defaultType t
          defaultType (TMap k v) = defaultType k <> defaultType v
          -- TFunc params/returns are not defaulted here: a TypeVar that survives
          -- only in a TFunc signature is a genuine inference failure.
          defaultType (TFunc {}) = mempty
          defaultType (TNamed _) = mempty

  -- Collect all remaining TypeVars in an Expr tree as errors.
  let checkNoTypeVars :: Expr -> [InferError]
      checkNoTypeVars = foldMapExpr (\e -> let p = exprPos e in concatMap (collectTypeVars p) (exprTypes e))
        where
          collectTypeVars _ (TNamed _)          = []
          collectTypeVars p (TSlice t)          = collectTypeVars p t
          collectTypeVars p (TMap k v)          = collectTypeVars p k ++ collectTypeVars p v
          collectTypeVars p (TFunc ps mVar ret) =
            concatMap (collectTypeVars p) ps ++ maybe [] (collectTypeVars p) mVar ++ collectTypeVars p ret
          collectTypeVars p (TypeVar _)              = [CannotInferType p]
          collectTypeVars p (TypeVarConstrained _ _) = [CannotInferType p]
          collectTypeVars p (TypeVarIndexable _ k v) = [CannotInferType p] ++ collectTypeVars p k ++ collectTypeVars p v

  -- Compute isStmt annotations for codegen.
  -- Post-order traversal: set each node's isStmt based on whether it or any
  -- child can standalone as a Go statement. EApplication and ELet are inherently
  -- statement-valid; all other nodes are statement-valid if any child is.
  let computeIsStmt :: Expr -> Expr
      computeIsStmt = mapExpr go
        where
          -- Inherently statement-valid
          go (EApplication a f args) = EApplication a{isStmt = True} f args
          go (ELet a name b mtin) = ELet a{isStmt = True} name b mtin
          -- Statement-valid if any child is
          go (EIf a c th el) = EIf a{isStmt = anyIsStmt [c, th, el]} c th el
          go (EMatch a scrut arms) = EMatch a{isStmt = anyIsStmt (scrut : [body | MatchArm _ _ body <- arms])} scrut arms
          go (ESequence a es) = ESequence a{isStmt = anyIsStmt es} es
          go (EInfixOp a e1 op e2) = EInfixOp a{isStmt = anyIsStmt [e1, e2]} e1 op e2
          go (EIndex a e idx) = EIndex a{isStmt = anyIsStmt [e, idx]} e idx
          go (ESliceLit a es) = ESliceLit a{isStmt = anyIsStmt es} es
          go (EVariadicSpread a e) = EVariadicSpread a{isStmt = anyIsStmt [e]} e
          go (ECoerce a c inner) = ECoerce a{isStmt = anyIsStmt [inner]} c inner
          -- No effect on statement-validity
          go e@(ELambda {}) = e
          go e@(EVar {}) = e
          go e@(EIntLit {}) = e
          go e@(EFloatLit {}) = e
          go e@(EStrLit {}) = e
          go e@(EUnitLit {}) = e
          go e@(EMapLit {}) = e
          anyIsStmt = any (isStmt . exprAnn)

  -- Insert ECoerce at type boundaries where function return types disagree
  -- in the unit<->struct{} dimension.
  let insertCoercions :: Expr -> Expr
      insertCoercions = go
        where
          -- Do two function types differ only in their return type's unit<->struct{} name?
          funcVoidMismatch (TFunc _ _ t1) (TFunc _ _ t2) =
            isUnitLike t1 && isUnitLike t2 && t1 /= t2
          funcVoidMismatch _ _ = False

          coerceIfNeeded expectedTy e
            | funcVoidMismatch expectedTy (exprType e) =
                ECoerce (exprAnn e){ty = expectedTy} FuncVoidCoerce e
            | otherwise = e

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
          go (EMatch a scrut arms) = EMatch a (go scrut) (map (\(MatchArm p pat body) -> MatchArm p pat (go body)) arms)
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

          -- Coerce fixed and variadic args where param types disagree.
          coerceArgs (TFunc fixedTys mVarTy _) args = fixedCoerced ++ varCoerced
            where
              fixedCoerced = zipWith (\t a -> coerceIfNeeded t (go a)) fixedTys args
              varArgs = drop (length fixedTys) args
              varCoerced = case (mVarTy, varArgs) of
                -- Zero-variadic sentinel: pass through unchanged
                (Just _, [EUnitLit _]) -> varArgs
                -- Variadic args: coerce each against the variadic type
                (Just varTy, _) -> map (\a -> coerceIfNeeded varTy (go a)) varArgs
                (Nothing, [])   -> []
                (Nothing, _)    -> error "coerceArgs: excess args on non-variadic function (unreachable)"
          coerceArgs _ args = map go args

  let initState = InferState { inferSubst = Map.empty, iNextTypeVarId = pNextTypeVarId pState }
  -- Step 1: infer types (constraint generation + unification)
  (inferred, finalState) <- first (:[]) $ runStateT (inferExpr preludeEnv expr) initState
  -- Step 2: apply substitution to resolve TypeVars
  let resolved = applySubstExpr (inferSubst finalState) inferred
  -- Step 3: default remaining type variables
  let afterDefaults = defaultLoop resolved
  -- Step 4: reject any unresolved TypeVars as errors
  case checkNoTypeVars afterDefaults of
    [] -> pure ()
    errs -> Left errs
  -- Step 5: compute isStmt annotations for codegen
  let afterIsStmt = computeIsStmt afterDefaults
  -- Step 6: insert unit<->struct{} coercion wrappers
  Right (insertCoercions afterIsStmt)

prettyType :: TypeExpr -> String
prettyType (TNamed (Ident n)) = T.unpack n
prettyType (TSlice t) = "[]" <> prettyType t
prettyType (TMap k v) = "map[" <> prettyType k <> "]" <> prettyType v
prettyType (TFunc ps mVar ret) =
  "func(" <> intercalate ", " (map prettyType ps ++ maybe [] (\v -> [prettyType v <> "..."]) mVar) <> ") " <> prettyType ret
prettyType (TypeVar _) = "unknown type"
prettyType (TypeVarConstrained _ ts) = let Ident n = tsDefault ts in T.unpack n
prettyType (TypeVarIndexable _ k v) = "indexable[" <> prettyType k <> "]" <> prettyType v

prettyInferError :: InferError -> String
prettyInferError (TypeMismatch    p t1 t2)        = sourcePosPretty p <> ": error: type mismatch: " <> prettyType t1 <> " vs " <> prettyType t2
prettyInferError (UnknownVariable p (Ident name)) = sourcePosPretty p <> ": error: unknown variable: " <> T.unpack name
prettyInferError (NotAFunction    p t)            = sourcePosPretty p <> ": error: not a function: " <> prettyType t
prettyInferError (NotAnIndexable    p t)          = sourcePosPretty p <> ": error: not an indexable: " <> prettyType t
prettyInferError (CannotInferType p)              = sourcePosPretty p <> ": error: cannot infer type"
prettyInferError (InfiniteType    p n t)          = sourcePosPretty p <> ": error: infinite type: " <> prettyType (TypeVar n) <> " occurs in " <> prettyType t
prettyInferError (NamedPUnit      p (Ident name)) = sourcePosPretty p <> ": error: named unit parameter: " <> T.unpack name
prettyInferError (MissingSpread   p t)            = sourcePosPretty p <> ": error: missing spread: " <> prettyType t <> ", expected ...x"
