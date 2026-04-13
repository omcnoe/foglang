module Foglang.Inference (InferError (..), inferAndResolve, prettyInferError) where

import Control.Monad.State.Strict (State, StateT, get, gets, put, modify, runState, runStateT, lift)
import Data.Bifunctor (first)
import Data.IntSet qualified as IntSet
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)
import Foglang.AST
  ( Binding (..), Coercion (..), ConcreteShape (..), Expr (..), ExprAnn (..),
    GroundType (..), Ident (..), MatchArm (..), Param (..), Pattern (..),
    TypeExpr (..), TypeSet (..),
    pattern OpaqueType, pattern OpaqueTypeExpr, pattern UnitTypeExpr,
    bindingTypeExpr, exprAnn, exprPos, exprType,
    isUnitLike, isUnitLikeShape, isWildcardShape, tsFloat, tsInt, tvarPos )
import Foglang.Parser (ParserState (..))
import Foglang.Subst
  ( Constraint (..), FindResult (..), RootContent (..), Subst,
    bindConcrete, bindConstraint, bindLink, emptySubst, find )

-- Environment maps names to their inference-time types.
type Env = Map.Map Ident TypeExpr

data InferError
  = UnknownVariable SourcePos Ident
  | TypeMismatch SourcePos TypeExpr TypeExpr -- expected, actual
  | InfiniteType SourcePos Int TypeExpr
  | NotAFunction SourcePos TypeExpr
  | NotAnIndexable SourcePos TypeExpr
  | CannotInferType SourcePos
  | NamedPUnit SourcePos Ident
  | MissingSpread SourcePos TypeExpr
  deriving (Eq, Show)

-- Inference state: substitution + fresh TypeVar counter.
data InferState = InferState { inferSubst :: !Subst, iNextTypeVarId :: !Int }

-- StateT for short-circuit on first inference error via Either.
type Infer = StateT InferState (Either InferError)

-- ----------------------------------------------------------------------------
-- Fresh variables

freshTypeVarId :: Infer Int
freshTypeVarId = do
  st <- get
  put st { iNextTypeVarId = iNextTypeVarId st + 1 }
  pure (iNextTypeVarId st)

freshTypeVar :: SourcePos -> Infer TypeExpr
freshTypeVar p = TVar p <$> freshTypeVarId

-- | Mint a fresh TVar bound to a Root holding the given constraint.
-- Used for numeric literals (CNumeric) and indexable operations (CIndexable).
freshConstrainedVar :: SourcePos -> Constraint -> Infer TypeExpr
freshConstrainedVar p c = do
  n <- freshTypeVarId
  modify (\st -> st { inferSubst = bindConstraint n c (inferSubst st) })
  pure (TVar p n)

-- ----------------------------------------------------------------------------
-- Subst accessors

getSubst :: Infer Subst
getSubst = gets inferSubst

putSubst :: Subst -> Infer ()
putSubst s = modify (\st -> st { inferSubst = s })

-- ----------------------------------------------------------------------------
-- Type views via find

-- | A TypeExpr resolved one level: shape, unbound variable, or variable
-- that carries a constraint. Used by unify and inferExpr to dispatch on
-- the type's current known state without chasing Link chains repeatedly.
data TypeView
  = VShape !ConcreteShape
  | VVarUnbound !Int
  | VVarConstraint !Int !Constraint
  deriving (Eq, Show)

-- | Resolve a TypeExpr through the substitution to its current view,
-- compressing paths along the way (inside `find`).
viewType :: TypeExpr -> Subst -> (TypeView, Subst)
viewType (TShape c) s = (VShape c, s)
viewType (TVar _ n) s =
  let (fr, s') = find n s
  in case fr of
       FoundUnbound n'              -> (VVarUnbound n', s')
       -- Resolved to a concrete shape: drop the TVar identity - the
       -- shape is the caller's answer.
       FoundRoot _  (RConcrete c)   -> (VShape c, s')
       -- Still constrained: keep the representative ID so the caller
       -- can link against it or narrow the constraint.
       FoundRoot n' (RConstraint c) -> (VVarConstraint n' c, s')

viewTypeM :: TypeExpr -> Infer TypeView
viewTypeM t = do
  s <- getSubst
  let (v, s') = viewType t s
  putSubst s'
  pure v

-- ----------------------------------------------------------------------------
-- Occurs check

-- | Would binding TVar n to this TypeExpr create a cyclic type? Walks the
-- substitution transitively. Returns updated Subst with any path-compression
-- done during the walk so callers inherit the compressed state.
--
-- The visited set (keyed on Root IDs, not Link IDs) both avoids redundant
-- exploration of a Root reachable via multiple paths and, defensively,
-- guards against cycles already present in the substitution. Well-formed
-- substs (maintained by this occurs check itself at bind time) never have
-- cycles, so in practice the set prevents redundancy more often than cycles.
occursInType :: Int -> TypeExpr -> Subst -> (Bool, Subst)
occursInType n t s0 = runState (go t IntSet.empty) s0
  where
    go :: TypeExpr -> IntSet.IntSet -> State Subst Bool
    go (TShape c) v = goShape c v
    go (TVar _ m) v
      | n == m    = pure True
      | otherwise = do
          s <- get
          let (fr, s') = find m s
          put s'
          case fr of
            FoundUnbound m' -> pure (n == m')
            FoundRoot m' rc
              | n == m'              -> pure True
              | m' `IntSet.member` v -> pure False
              | otherwise            -> goRoot rc (IntSet.insert m' v)

    goShape :: ConcreteShape -> IntSet.IntSet -> State Subst Bool
    goShape (CNamed _)         _ = pure False
    goShape (CSlice t')        v = go t' v
    goShape (CMap k u)         v = orM (go k v) (go u v)
    goShape (CFunc ps mVar r)  v =
      orM (anyM (`go` v) ps)
          (orM (maybe (pure False) (`go` v) mVar) (go r v))

    goRoot :: RootContent -> IntSet.IntSet -> State Subst Bool
    goRoot (RConcrete c)                    v = goShape c v
    goRoot (RConstraint (CNumeric _))       _ = pure False
    goRoot (RConstraint (CIndexable k u))   v = orM (go k v) (go u v)

    orM :: Monad m => m Bool -> m Bool -> m Bool
    orM a b = a >>= \x -> if x then pure True else b

    anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
    anyM f = foldr (\x acc -> f x >>= \b -> if b then pure True else acc) (pure False)

-- ----------------------------------------------------------------------------
-- Unification

unifyM :: SourcePos -> TypeExpr -> TypeExpr -> Infer ()
unifyM p t1 t2 = do
  s <- getSubst
  case unify p s t1 t2 of
    Left err -> lift (Left err)
    Right s' -> putSubst s'

-- | Unify two types, updating the substitution. Dispatches on the current
-- view of each side (shape / unbound var / constrained var).
unify :: SourcePos -> Subst -> TypeExpr -> TypeExpr -> Either InferError Subst
unify p s0 rawT1 rawT2 =
  let (v1, s1) = viewType rawT1 s0
      (v2, s2) = viewType rawT2 s1
  in case (v1, v2) of
    -- Wildcards unify freely without binding.
    (VShape c, _) | isWildcardShape c -> Right s2
    (_, VShape c) | isWildcardShape c -> Right s2
    -- Two unit-like named types unify freely (() vs struct{} tolerated).
    (VShape c1, VShape c2) | isUnitLikeShape c1 && isUnitLikeShape c2 -> Right s2
    -- Var ~ Var
    (VVarUnbound a, VVarUnbound b) ->
      Right (bindLink a b s2)
    (VVarUnbound a, VVarConstraint b _) ->
      Right (bindLink a b s2)
    (VVarConstraint a _, VVarUnbound b) ->
      Right (bindLink b a s2)
    (VVarConstraint a c1, VVarConstraint b c2)
      | a == b    -> Right s2
      | otherwise -> do
          s3 <- unifyConstraints p s2 c1 c2
          Right (bindLink b a s3)
    -- Var ~ Shape
    (VVarUnbound a, VShape c) ->
      let (occurs, s3) = occursInType a (TShape c) s2
      in if occurs
           then Left (InfiniteType p a (TShape c))
           else Right (bindConcrete a c s3)
    (VShape c, VVarUnbound b) ->
      let (occurs, s3) = occursInType b (TShape c) s2
      in if occurs
           then Left (InfiniteType p b (TShape c))
           else Right (bindConcrete b c s3)
    -- Constraint ~ Shape: promote if shape satisfies the constraint.
    (VVarConstraint a c, VShape sh) ->
      unifyConstraintShape p s2 a c sh
    (VShape sh, VVarConstraint a c) ->
      unifyConstraintShape p s2 a c sh
    -- Shape ~ Shape
    (VShape c1, VShape c2) -> unifyShapes p s2 c1 c2

-- | Unify two constraints that have become associated (both on the same
-- equivalence class, or about to be linked). The two variables are
-- expected to end up linked afterwards.
unifyConstraints :: SourcePos -> Subst -> Constraint -> Constraint -> Either InferError Subst
unifyConstraints p s (CNumeric ts1) (CNumeric ts2)
  | ts1 == ts2 = Right s
  | otherwise  = Left (TypeMismatch p (constraintDisplay (CNumeric ts1)) (constraintDisplay (CNumeric ts2)))
unifyConstraints p _ c1@(CNumeric _) (CIndexable _ _) =
  Left (NotAnIndexable p (constraintDisplay c1))
unifyConstraints p _ (CIndexable _ _) c2@(CNumeric _) =
  Left (NotAnIndexable p (constraintDisplay c2))
unifyConstraints p s (CIndexable k1 v1) (CIndexable k2 v2) = do
  s' <- unify p s k1 k2
  unify p s' v1 v2

-- | Promote a constrained variable when the other side is a concrete shape.
-- If the shape satisfies the constraint, the variable's Root is updated to
-- RConcrete (the shape becomes the known answer for the equivalence class).
unifyConstraintShape :: SourcePos -> Subst -> Int -> Constraint -> ConcreteShape -> Either InferError Subst
-- Numeric: check set membership.
unifyConstraintShape p s n (CNumeric ts) c@(CNamed name)
  | name `Set.member` tsMembers ts = Right (bindConcrete n c s)
  | otherwise = Left (TypeMismatch p (constraintDisplay (CNumeric ts)) (TShape c))
unifyConstraintShape p _ _ (CNumeric ts) c =
  Left (TypeMismatch p (constraintDisplay (CNumeric ts)) (TShape c))
-- Indexable: check shape kind, unify children.
unifyConstraintShape p s n (CIndexable k v) (CSlice elemTy) = do
  s' <- unify p s k (TShape (CNamed (Ident "int")))
  s'' <- unify p s' v elemTy
  Right (bindConcrete n (CSlice elemTy) s'')
unifyConstraintShape p s n (CIndexable k v) (CMap mk mv) = do
  s' <- unify p s k mk
  s'' <- unify p s' v mv
  Right (bindConcrete n (CMap mk mv) s'')
unifyConstraintShape p s n (CIndexable k v) (CNamed (Ident "string")) = do
  s' <- unify p s k (TShape (CNamed (Ident "int")))
  s'' <- unify p s' v (TShape (CNamed (Ident "byte")))
  Right (bindConcrete n (CNamed (Ident "string")) s'')
unifyConstraintShape p _ _ (CIndexable _ _) c = Left (NotAnIndexable p (TShape c))

-- | Head-to-head unification of two concrete shapes.
unifyShapes :: SourcePos -> Subst -> ConcreteShape -> ConcreteShape -> Either InferError Subst
unifyShapes p s (CNamed a) (CNamed b)
  | a == b = Right s
  | otherwise = Left (TypeMismatch p (TShape (CNamed a)) (TShape (CNamed b)))
unifyShapes p s (CSlice a) (CSlice b) = unify p s a b
unifyShapes p s (CMap k1 v1) (CMap k2 v2) = do
  s' <- unify p s k1 k2
  unify p s' v1 v2
unifyShapes p s (CFunc as va ra) (CFunc bs vb rb) = do
  s' <- unifyPairwise p s as bs
  s'' <- case (va, vb) of
    (Nothing, Nothing) -> Right s'
    (Just a, Just b)   -> unify p s' a b
    _ -> Left (TypeMismatch p (TShape (CFunc as va ra)) (TShape (CFunc bs vb rb)))
  unify p s'' ra rb
unifyShapes p _ c1 c2 = Left (TypeMismatch p (TShape c1) (TShape c2))

unifyPairwise :: SourcePos -> Subst -> [TypeExpr] -> [TypeExpr] -> Either InferError Subst
unifyPairwise _ s [] [] = Right s
unifyPairwise p s (a : as') (b : bs') = do
  s' <- unify p s a b
  unifyPairwise p s' as' bs'
unifyPairwise p _ xs ys = Left (TypeMismatch p (TShape (CFunc xs Nothing UnitTypeExpr))
                                                 (TShape (CFunc ys Nothing UnitTypeExpr)))

-- | Best-effort TypeExpr for a constraint, used only for error display.
-- CNumeric renders as the set's default (e.g. "int" for tsInt) so the
-- reader sees a familiar concrete name. CIndexable is currently
-- unreachable here (every caller that could produce one uses the
-- CNumeric side instead; see unifyConstraints, unifyConstraintShape)
-- but keep a clear marker for the future rather than lying with TMap.
constraintDisplay :: Constraint -> TypeExpr
constraintDisplay (CNumeric ts)    = TShape (CNamed (tsDefault ts))
constraintDisplay (CIndexable _ _) = TShape (CNamed (Ident "<indexable>"))

-- ----------------------------------------------------------------------------
-- Built-ins

preludeEnv :: Env
preludeEnv =
  Map.fromList
    [ (Ident "true",  TShape (CNamed (Ident "bool"))),
      (Ident "false", TShape (CNamed (Ident "bool"))),
      -- Go builtins: opaque while fog lacks generics.
      (Ident "len",       OpaqueTypeExpr),
      (Ident "append",    OpaqueTypeExpr),
      (Ident "delete",    OpaqueTypeExpr),
      (Ident "mapInsert", OpaqueTypeExpr),
      (Ident "mapDelete", OpaqueTypeExpr),
      (Ident "intRange",  OpaqueTypeExpr)
      -- TODO: eventually remove all usages of opaque.
    ]

-- ----------------------------------------------------------------------------
-- inferExpr: constraint generation + unification

inferExpr :: Env -> Expr TypeExpr -> Infer (Expr TypeExpr)
inferExpr env expr = case expr of
  EVar a i              -> inferVar a i
  EUnitLit _            -> return expr
  EIntLit a _           -> do
    constrainLiteral a (CNumeric tsInt)
    return expr
  EFloatLit a _         -> do
    constrainLiteral a (CNumeric tsFloat)
    return expr
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
    infer = inferExpr env

    -- | Bind the parser-minted TVar on a literal's annotation to the
    -- given constraint. Parser always emits TVar for literals; if it
    -- ever emits a concrete shape (e.g. via later changes), we fall
    -- back to unification so the invariant holds.
    constrainLiteral :: ExprAnn TypeExpr -> Constraint -> Infer ()
    constrainLiteral ExprAnn{pos = p, ty = t} c = case t of
      TVar _ n -> modify (\st -> st { inferSubst = bindConstraint n c (inferSubst st) })
      TShape _ -> unifyM p t (constraintDisplay c)

    rejectNamedUnitParams :: SourcePos -> [Param TypeExpr] -> Infer ()
    rejectNamedUnitParams _ [] = return ()
    rejectNamedUnitParams p (PTyped name UnitTypeExpr : _) = lift (Left (NamedPUnit p name))
    rejectNamedUnitParams p (_ : rest) = rejectNamedUnitParams p rest

    paramEnvOf :: [Param TypeExpr] -> Env
    paramEnvOf params = Map.fromList $
      [(name, t) | PTyped name t <- params]
        ++ [(name, TShape (CSlice t)) | PVariadic name t <- params]

    inferVar ExprAnn{pos = p, ty = origTy} i@(Ident t) =
      case Map.lookup i env of
        Just envTy -> do
          unifyM p origTy envTy
          return (EVar ExprAnn { pos = p, ty = envTy, isStmt = False } i)
        Nothing
          | "." `T.isInfixOf` t ->
              return (EVar ExprAnn { pos = p, ty = OpaqueTypeExpr, isStmt = False } i)
          | otherwise -> lift (Left (UnknownVariable p i))

    inferInfixOp ExprAnn{pos = p} e1 op e2 = do
      te1 <- infer e1
      te2 <- infer e2
      let lhsTy = exprType te1
          rhsTy = exprType te2
      case op of
        "::" -> unifyM p rhsTy (TShape (CSlice lhsTy))
        _    -> unifyM p lhsTy rhsTy
      let resultTy = case op of
            "::" -> rhsTy
            _ | op `elem` ["==", "!=", "<", ">", "<=", ">=", "&&", "||"] ->
                TShape (CNamed (Ident "bool"))
            _ -> lhsTy
      return (EInfixOp ExprAnn { pos = p, ty = resultTy, isStmt = False } te1 op te2)

    inferIf ExprAnn{pos = p} cond then' else' = do
      tcond <- infer cond
      unifyM p (exprType tcond) (TShape (CNamed (Ident "bool")))
      tthen <- infer then'
      telse <- infer else'
      unifyM p (exprType tthen) (exprType telse)
      return (EIf ExprAnn { pos = p, ty = exprType tthen, isStmt = False } tcond tthen telse)

    inferSequence ExprAnn{pos = p} exprs = do
      texprs <- mapM infer exprs
      let resultTy = case texprs of
            [] -> UnitTypeExpr
            _  -> exprType (last texprs)
      return (ESequence ExprAnn { pos = p, ty = resultTy, isStmt = False } texprs)

    inferLambda ExprAnn{pos = p} (Binding params retTy body) = do
      rejectNamedUnitParams p params
      tbody <- inferExpr (Map.union (paramEnvOf params) env) body
      unifyM p retTy (exprType tbody)
      return (ELambda ExprAnn { pos = p, ty = bindingTypeExpr params retTy, isStmt = False }
                      (Binding params retTy tbody))

    inferLet ExprAnn{pos = p} name (Binding params retTy rhs) mInExpr = do
      rejectNamedUnitParams p params
      let bindTy = bindingTypeExpr params retTy
      let envWithSelf = Map.insert name bindTy env
      trhs <- inferExpr (Map.union (paramEnvOf params) envWithSelf) rhs
      unifyM p retTy (exprType trhs)
      let envForCont = Map.insert name bindTy env
      mtin <- traverse (inferExpr envForCont) mInExpr
      return (ELet ExprAnn { pos = p, ty = maybe UnitTypeExpr exprType mtin, isStmt = True }
                   name (Binding params retTy trhs) mtin)

    inferIndex ExprAnn{pos = p} e idx = do
      te <- infer e
      tidx <- infer idx
      containerView <- viewTypeM (exprType te)
      case containerView of
        VShape c | isWildcardShape c ->
          return (EIndex ExprAnn { pos = p, ty = OpaqueTypeExpr, isStmt = False } te tidx)
        _ -> do
          valTy <- freshTypeVar p
          cTy <- freshConstrainedVar p (CIndexable (exprType tidx) valTy)
          unifyM p (exprType te) cTy
          return (EIndex ExprAnn { pos = p, ty = valTy, isStmt = False } te tidx)

    inferSliceLit ExprAnn{pos = p} exprs = do
      texprs <- mapM infer exprs
      case texprs of
        [] -> do
          elemTv <- freshTypeVar p
          return (ESliceLit ExprAnn { pos = p, ty = TShape (CSlice elemTv), isStmt = False } texprs)
        (te : rest) -> do
          mapM_ (\e' -> unifyM p (exprType te) (exprType e')) rest
          return (ESliceLit ExprAnn { pos = p, ty = TShape (CSlice (exprType te)), isStmt = False } texprs)

    inferMapLit ExprAnn{pos = p} = do
      kTv <- freshTypeVar p
      vTv <- freshTypeVar p
      return (EMapLit ExprAnn { pos = p, ty = TShape (CMap kTv vTv), isStmt = False })

    inferMatch ExprAnn{pos = p} scrut arms = do
      tscrut <- infer scrut
      tarms <- inferArms tscrut arms
      case tarms of
        [] -> return ()
        (MatchArm _ _ firstBody : rest) ->
          mapM_ (\(MatchArm _ _ body) -> unifyM p (exprType firstBody) (exprType body)) rest
      let resultTy = case tarms of
            (MatchArm _ _ body : _) -> exprType body
            [] -> UnitTypeExpr
      return (EMatch ExprAnn { pos = p, ty = resultTy, isStmt = False } tscrut tarms)
      where
        inferArms _ [] = return []
        inferArms tscrut (MatchArm armPos pat body : rest) = do
          let scrutTy = exprType tscrut
          constrainPattern armPos scrutTy pat
          patBindings <- patternBindings armPos scrutTy pat
          let armEnv = Map.union (Map.fromList patBindings) env
          tbody <- inferExpr armEnv body
          trest <- inferArms tscrut rest
          return (MatchArm armPos pat tbody : trest)

        constrainPattern _ _ PtWildcard = return ()
        constrainPattern _ _ (PtVar _) = return ()
        constrainPattern p' scrutTy (PtBoolLit _) = unifyM p' scrutTy (TShape (CNamed (Ident "bool")))
        constrainPattern p' scrutTy (PtIntLit _) = do
          tc <- freshConstrainedVar p' (CNumeric tsInt)
          unifyM p' scrutTy tc
        constrainPattern p' scrutTy (PtStrLit _) = unifyM p' scrutTy (TShape (CNamed (Ident "string")))
        constrainPattern p' scrutTy PtSliceEmpty = do
          elemTv <- freshTypeVar p'
          unifyM p' scrutTy (TShape (CSlice elemTv))
        constrainPattern p' scrutTy (PtCons _ _) = do
          elemTv <- freshTypeVar p'
          unifyM p' scrutTy (TShape (CSlice elemTv))
        constrainPattern _ _ (PtTuple _) = return ()

        patternBindings _ _ PtWildcard = return []
        patternBindings _ t (PtVar i) = return [(i, t)]
        patternBindings _ _ (PtIntLit _) = return []
        patternBindings _ _ (PtStrLit _) = return []
        patternBindings _ _ (PtBoolLit _) = return []
        patternBindings _ _ PtSliceEmpty = return []
        patternBindings p' t (PtCons hd tl) = do
          elemTy <- case t of
            TShape (CSlice et) -> return et
            _                  -> freshTypeVar p'
          hdBindings <- patternBindings p' elemTy hd
          tlBindings <- patternBindings p' t tl
          return (hdBindings ++ tlBindings)
        patternBindings p' _ (PtTuple pats) = do
          results <- mapM (\pat -> do tv <- freshTypeVar p'; patternBindings p' tv pat) pats
          return (concat results)

    inferSpread ExprAnn{pos = p} e = do
      te <- infer e
      containerView <- viewTypeM (exprType te)
      case containerView of
        VShape c | isWildcardShape c ->
          return (EVariadicSpread ExprAnn { pos = p, ty = OpaqueTypeExpr, isStmt = False } te)
        _ -> do
          elemTv <- freshTypeVar p
          unifyM p (exprType te) (TShape (CSlice elemTv))
          return (EVariadicSpread ExprAnn { pos = p, ty = TShape (CSlice elemTv), isStmt = False } te)

    inferApp ExprAnn{pos = p} f args = do
      tf <- infer f
      targs <- mapM infer args
      fView <- viewTypeM (exprType tf)
      case fView of
        VShape (CFunc fixed mVar ret) -> inferKnownApp p tf targs fixed mVar ret
        VShape c | isWildcardShape c -> do
          -- Opaque call: skip checking.
          resultTy <- lift (resultTypeFromShape p c (length targs))
          return (EApplication ExprAnn { pos = p, ty = resultTy, isStmt = True } tf targs)
        VVarUnbound _ -> do
          resultTv <- freshTypeVar p
          unifyM p (exprType tf) (TShape (CFunc (map exprType targs) Nothing resultTv))
          return (EApplication ExprAnn { pos = p, ty = resultTv, isStmt = True } tf targs)
        VVarConstraint _ _ ->
          -- Numeric/indexable constraint can't be a function.
          lift (Left (NotAFunction p (exprType tf)))
        VShape c -> lift (Left (NotAFunction p (TShape c)))

    -- inferKnownApp uses the original function's fixed/mVar/ret types.
    inferKnownApp p tf targs fixed mVar ret = do
      let nFixed = length fixed
          nSupplied = length targs
      mapM_ (\(arg, paramTy) -> unifyM p (exprType arg) paramTy) (zip targs fixed)
      case mVar of
        Nothing -> return ()
        Just varTy -> do
          let varArgs = drop nFixed targs
              isEmptyVariadic = case varArgs of [EUnitLit _] -> True; _ -> False
          if not isEmptyVariadic && nSupplied > nFixed
            then mapM_ (unifyVarArg p varTy) varArgs
            else return ()
      resultTy <- lift (resultTypeFromShape p (CFunc fixed mVar ret) nSupplied)
      return (EApplication ExprAnn { pos = p, ty = resultTy, isStmt = True } tf targs)

    unifyVarArg p varTy arg = case arg of
      EVariadicSpread {} -> unifyM p (exprType arg) (TShape (CSlice varTy))
      _ -> do
        v <- viewTypeM (exprType arg)
        case v of
          -- Report the resolved slice in the error, not the raw TVar,
          -- so the user sees "[]int" rather than "unknown type".
          VShape sliceShape@(CSlice _) ->
            lift (Left (MissingSpread (exprPos arg) (TShape sliceShape)))
          _ -> unifyM p (exprType arg) varTy

    -- Result type after applying n arguments to a function shape.
    resultTypeFromShape :: SourcePos -> ConcreteShape -> Int -> Either InferError TypeExpr
    resultTypeFromShape p (CFunc fixed mVar ret) n = case mVar of
      Nothing
        | n == length fixed -> Right ret
        | n < length fixed  -> Right (TShape (CFunc (drop n fixed) Nothing ret))
        | otherwise         -> Left (NotAFunction p ret)
      Just varTy
        | n > length fixed  -> Right ret
        | n == length fixed -> Right (TShape (CFunc [] (Just varTy) ret))
        | otherwise         -> Right (TShape (CFunc (drop n fixed) (Just varTy) ret))
    -- Any wildcard (opaque/any) short-circuits: application result stays
    -- wildcard. `isWildcardShape` is the authoritative predicate - keep
    -- this branch and the unify-side wildcard check in lock-step.
    resultTypeFromShape _ shape _ | isWildcardShape shape = Right (TShape shape)
    resultTypeFromShape p shape _ = Left (NotAFunction p (TShape shape))

-- ----------------------------------------------------------------------------
-- Resolution: TypeExpr + Subst -> GroundType (with folded defaulting)

-- | Resolve one TypeExpr through the substitution to a ground type.
-- Defaulting folds in structurally:
--   * unbound TVar              -> opaque (blanket default)
--   * RConstraint (CNumeric ts) -> TyNamed (tsDefault ts)
--   * RConstraint (CIndexable key val):
--       recursively resolve key and val; decide TySlice vs TyMap vs
--       string based on the resolved key shape. Non-defaultable key
--       (e.g. resolved to slice/func) -> CannotInferType.
--   * RConcrete c               -> structurally descend
--   * TShape c                  -> structurally descend
--
-- The SourcePos for CannotInferType comes from the TVar that owns the
-- failing CIndexable constraint - `viewType` only ever produces
-- VVarConstraint from a TVar input, so `tvarPos t` is Just.
resolveType :: TypeExpr -> StateT Subst (Either [InferError]) GroundType
resolveType t = do
  s0 <- get
  let (v, s1) = viewType t s0
  put s1
  case v of
    VShape c -> resolveShape c
    VVarUnbound _ -> pure OpaqueType
    VVarConstraint _ (CNumeric ts) -> pure (TyNamed (tsDefault ts))
    VVarConstraint _ (CIndexable k u) -> do
      -- Resolve children first; they may default to opaque (which is a
      -- TyNamed, not in tsInt). The fully-unconstrained case therefore
      -- lands on `TyMap opaque opaque`, load-bearing for codegen's nil
      -- detection (see Codegen.genExpr EMapLit). Key resolved to a
      -- non-TyNamed (e.g. a surviving TySlice from a deeper indexable
      -- jam) is genuinely unresolvable: report CannotInferType at the
      -- TVar's mint site.
      kTy <- resolveType k
      vTy <- resolveType u
      case kTy of
        TyNamed name
          | name `Set.member` tsMembers tsInt -> pure (TySlice vTy)
          | otherwise                         -> pure (TyMap kTy vTy)
        _ -> case tvarPos t of
          Just p  -> lift (Left [CannotInferType p])
          Nothing -> error "resolveType: VVarConstraint on non-TVar (unreachable)"

resolveShape :: ConcreteShape -> StateT Subst (Either [InferError]) GroundType
resolveShape c = case c of
  CNamed i        -> pure (TyNamed i)
  CSlice t        -> TySlice <$> resolveType t
  CMap k v        -> TyMap <$> resolveType k <*> resolveType v
  CFunc ps mVar r -> TyFunc
    <$> traverse resolveType ps
    <*> traverse resolveType mVar
    <*> resolveType r

-- | Resolve every TypeExpr slot in an Expr tree to a GroundType. `Expr` and
-- its sub-types derive Traversable over the type payload, so this is a
-- single traverse: every TVar / TShape under every ExprAnn / Binding /
-- Param / MatchArm gets resolved.
resolveExpr :: Subst -> Expr TypeExpr -> Either [InferError] (Expr GroundType)
resolveExpr s0 expr = fst <$> runStateT (traverse resolveType expr) s0

-- ----------------------------------------------------------------------------
-- isStmt computation and coercion insertion (on Expr GroundType)

-- | Compute isStmt annotations for codegen. Bottom-up traversal.
computeIsStmt :: Expr GroundType -> Expr GroundType
computeIsStmt = go
  where
    go (EApplication a f args) = EApplication a{isStmt = True} (go f) (map go args)
    go (ELet a name (Binding ps rt rhs) mtin) =
      ELet a{isStmt = True} name (Binding ps rt (go rhs)) (fmap go mtin)
    go (EIf a c th el) =
      let c' = go c; th' = go th; el' = go el
      in EIf a{isStmt = anyIsStmt [c', th', el']} c' th' el'
    go (EMatch a scrut arms) =
      let scrut' = go scrut
          arms'  = map (\(MatchArm p pat body) -> MatchArm p pat (go body)) arms
      in EMatch a{isStmt = anyIsStmt (scrut' : [b | MatchArm _ _ b <- arms'])} scrut' arms'
    go (ESequence a es) =
      let es' = map go es
      in ESequence a{isStmt = anyIsStmt es'} es'
    go (EInfixOp a e1 op e2) =
      let e1' = go e1; e2' = go e2
      in EInfixOp a{isStmt = anyIsStmt [e1', e2']} e1' op e2'
    go (EIndex a e idx) =
      let e' = go e; idx' = go idx
      in EIndex a{isStmt = anyIsStmt [e', idx']} e' idx'
    go (ESliceLit a es) =
      let es' = map go es
      in ESliceLit a{isStmt = anyIsStmt es'} es'
    go (EVariadicSpread a e) =
      let e' = go e
      in EVariadicSpread a{isStmt = anyIsStmt [e']} e'
    go (ECoerce a c inner) =
      let inner' = go inner
      in ECoerce a{isStmt = anyIsStmt [inner']} c inner'
    go e@(ELambda {}) = e
    go e@(EVar {}) = e
    go e@(EIntLit {}) = e
    go e@(EFloatLit {}) = e
    go e@(EStrLit {}) = e
    go e@(EUnitLit {}) = e
    go e@(EMapLit {}) = e

    anyIsStmt :: [Expr GroundType] -> Bool
    anyIsStmt = any (isStmt . exprAnn)

-- | Insert ECoerce at type boundaries where function return types differ
-- only in the unit<->struct{} dimension.
insertCoercions :: Expr GroundType -> Expr GroundType
insertCoercions = go
  where
    funcVoidMismatch (TyFunc _ _ t1) (TyFunc _ _ t2) =
      isUnitLike t1 && isUnitLike t2 && t1 /= t2
    funcVoidMismatch _ _ = False

    coerceIfNeeded expectedTy e
      | funcVoidMismatch expectedTy (exprType e) =
          ECoerce (exprAnn e){ty = expectedTy} FuncVoidCoerce e
      | otherwise = e

    go (ELet a name (Binding params retTy rhs) mtin) =
      ELet a name (Binding params retTy (coerceIfNeeded retTy (go rhs))) (fmap go mtin)
    go (ELambda a (Binding params retTy body)) =
      ELambda a (Binding params retTy (coerceIfNeeded retTy (go body)))
    go (EApplication a tf targs) =
      let tf' = go tf
      in EApplication a tf' (coerceArgs (exprType tf') targs)
    go (EIf a c th el) = EIf a (go c) (go th) (go el)
    go (EMatch a scrut arms) =
      EMatch a (go scrut) (map (\(MatchArm p pat body) -> MatchArm p pat (go body)) arms)
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

    coerceArgs (TyFunc fixedTys mVarTy _) args = fixedCoerced ++ varCoerced
      where
        fixedCoerced = zipWith (\t a -> coerceIfNeeded t (go a)) fixedTys args
        varArgs = drop (length fixedTys) args
        varCoerced = case (mVarTy, varArgs) of
          (Just _, [EUnitLit _]) -> varArgs
          (Just varTy, _) -> map (\a -> coerceIfNeeded varTy (go a)) varArgs
          (Nothing, [])   -> []
          (Nothing, _)    -> error "coerceArgs: excess args on non-variadic function (unreachable)"
    coerceArgs _ args = map go args

-- ----------------------------------------------------------------------------
-- inferAndResolve: pipeline entry

-- | Full inference pipeline: constraint generation, unification, folded
-- resolution + defaulting, isStmt computation, coercion insertion.
inferAndResolve :: (Expr TypeExpr, ParserState) -> Either [InferError] (Expr GroundType)
inferAndResolve (expr, pState) = do
  let initState = InferState { inferSubst = emptySubst, iNextTypeVarId = pNextTypeVarId pState }
  (inferred, finalState) <- first (:[]) $ runStateT (inferExpr preludeEnv expr) initState
  resolved <- resolveExpr (inferSubst finalState) inferred
  let afterIsStmt = computeIsStmt resolved
  Right (insertCoercions afterIsStmt)

-- ----------------------------------------------------------------------------
-- Pretty printing

prettyType :: TypeExpr -> String
prettyType (TShape (CNamed (Ident n))) = T.unpack n
prettyType (TShape (CSlice t)) = "[]" <> prettyType t
prettyType (TShape (CMap k v)) = "map[" <> prettyType k <> "]" <> prettyType v
prettyType (TShape (CFunc ps mVar ret)) =
  "func(" <> intercalate ", " (map prettyType ps ++ maybe [] (\v -> [prettyType v <> "..."]) mVar) <> ") " <> prettyType ret
prettyType (TVar _ _) = "unknown type"

prettyInferError :: InferError -> String
prettyInferError (TypeMismatch    p t1 t2)        = sourcePosPretty p <> ": error: type mismatch: " <> prettyType t1 <> " vs " <> prettyType t2
prettyInferError (UnknownVariable p (Ident name)) = sourcePosPretty p <> ": error: unknown variable: " <> T.unpack name
prettyInferError (NotAFunction    p t)            = sourcePosPretty p <> ": error: not a function: " <> prettyType t
prettyInferError (NotAnIndexable    p t)          = sourcePosPretty p <> ": error: not an indexable: " <> prettyType t
prettyInferError (CannotInferType p)              = sourcePosPretty p <> ": error: cannot infer type"
prettyInferError (InfiniteType    p _ t)          = sourcePosPretty p <> ": error: infinite type: unknown type occurs in " <> prettyType t
prettyInferError (NamedPUnit      p (Ident name)) = sourcePosPretty p <> ": error: named unit parameter: " <> T.unpack name
prettyInferError (MissingSpread   p t)            = sourcePosPretty p <> ": error: missing spread: " <> prettyType t <> ", expected ...x"
