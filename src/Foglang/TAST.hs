module Foglang.TAST
  ( TBinding (..),
    TExpr (..),
    TMatchArm (..),
    TPattern (..),
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
  = TEVar TypeExpr Ident
  | TEIntLit TypeExpr IntLit
  | TEFloatLit TypeExpr FloatLit
  | TEStrLit TypeExpr StringLit
  | TEUnitLit
  | TELet TypeExpr Ident TBinding TExpr -- inExpr type, name, binding, inExpr
  | TELambda TypeExpr TBinding -- FuncType of the lambda, anonymous "binding" (no name, no inExpr)
  | TEIf TypeExpr TExpr TExpr TExpr
  | TEInfixOp TypeExpr TExpr T.Text TExpr
  | TEApplication TypeExpr TExpr [TExpr] -- result type of the application (FuncType if partial)
  | TEIndex TypeExpr TExpr TExpr -- expr[expr] — slice/map indexing
  | TESequence TypeExpr [TExpr]
  | TEVariadicSpread TypeExpr TExpr -- expr... unpacks []T into a variadic slot at a call site
  | TESliceLit TypeExpr [TExpr] -- slice literal
  | TEMapLit TypeExpr -- empty map literal
  | TEMatch TypeExpr TExpr [TMatchArm] -- match expression
  deriving (Eq, Show)

data TMatchArm = TMatchArm TPattern TExpr
  deriving (Eq, Show)

data TPattern
  = TPWildcard
  | TPVar Ident
  | TPIntLit IntLit
  | TPBoolLit Bool
  | TPSliceEmpty
  | TPCons TPattern TPattern
  | TPTuple [TPattern]
  deriving (Eq, Show)

-- Extract the TypeExpr from any TExpr node.
tExprType :: TExpr -> TypeExpr
tExprType (TEVar t _) = t
tExprType (TEIntLit t _) = t
tExprType (TEFloatLit t _) = t
tExprType (TEStrLit t _) = t
tExprType TEUnitLit = NamedType (Ident "()")
tExprType (TELet t _ _ _) = t
tExprType (TELambda t _) = t
tExprType (TEIf t _ _ _) = t
tExprType (TEInfixOp t _ _ _) = t
tExprType (TEApplication t _ _) = t
tExprType (TEIndex t _ _) = t
tExprType (TESequence t _) = t
tExprType (TEVariadicSpread t _) = t
tExprType (TESliceLit t _) = t
tExprType (TEMapLit t) = t
tExprType (TEMatch t _ _) = t
