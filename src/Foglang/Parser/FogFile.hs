module Foglang.Parser.FogFile (fogFile) where

import Foglang.AST (FogFile (..))
import Foglang.Parser (Parser, SC(..), scn)
import Foglang.Parser.Expr (childBlockExprSequence)
import Foglang.Parser.Header (header)

fogFile :: Parser FogFile
fogFile = do
  h <- header
  body <- childBlockExprSequence <* runSC scn
  return $ FogFile h body
