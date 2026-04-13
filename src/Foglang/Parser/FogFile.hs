module Foglang.Parser.FogFile (fogFile) where

import Foglang.AST (FogFile (..), TypeExpr)
import Foglang.Parser (Parser, SC(..), scn)
import Foglang.Parser.Expr (childBlockExprSequence)
import Foglang.Parser.Header (header)

fogFile :: Parser (FogFile TypeExpr)
fogFile = do
  h <- header
  body <- childBlockExprSequence <* runSC scn
  return $ FogFile h body
