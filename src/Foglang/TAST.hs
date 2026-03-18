module Foglang.TAST
  ( TBinding (..),
    TExpr (..),
    tExprType,
  )
where

import Data.Text qualified as T
import Foglang.AST (FloatLit, Ident (..), IntLit, Param, StringLit, TypeExpr (..))

-- The common shape of a typed let-binding and a typed lambda.
-- params ([] = value, [...] = function), value/function return type, and typed rhs.
data TBinding = TBinding [Param] TypeExpr TExpr
  deriving (Eq, Show)

-- A typed expression: every constructor carries the TypeExpr for the result of
-- that expression, as resolved by the elaboration pass.
data TExpr
  = TVar TypeExpr Ident
  | TIntLit TypeExpr IntLit
  | TFloatLit TypeExpr FloatLit
  | TStrLit TypeExpr StringLit
  | TUnitLit
  | TLet TypeExpr Ident TBinding TExpr -- inExpr type, name, binding, inExpr
  | TLambda TypeExpr TBinding -- FuncType of the lambda, anonymous "binding" (no name, no inExpr)
  | TIf TypeExpr TExpr TExpr TExpr
  | TBinaryOp TypeExpr TExpr T.Text TExpr
  | TApplication TypeExpr TExpr [TExpr] -- result type of the application (FuncType if partial)
  | TSequence TypeExpr [TExpr]
  | TVariadicSpread TypeExpr TExpr -- expr... unpacks []T into a variadic slot at a call site
  deriving (Eq, Show)

-- Extract the TypeExpr from any TExpr node.
tExprType :: TExpr -> TypeExpr
tExprType (TVar t _) = t
tExprType (TIntLit t _) = t
tExprType (TFloatLit t _) = t
tExprType (TStrLit t _) = t
tExprType TUnitLit = NamedType (Ident "()")
tExprType (TLet t _ _ _) = t
tExprType (TLambda t _) = t
tExprType (TIf t _ _ _) = t
tExprType (TBinaryOp t _ _ _) = t
tExprType (TApplication t _ _) = t
tExprType (TSequence t _) = t
tExprType (TVariadicSpread t _) = t
