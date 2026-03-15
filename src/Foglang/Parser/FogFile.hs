module Foglang.Parser.FogFile (fogFile) where

import Foglang.AST (FogFile (..))
import Foglang.Parser (Parser)
import Foglang.Parser.Expr (exprBlock)
import Foglang.Parser.Header (header)

fogFile :: Parser FogFile
fogFile = FogFile <$> header <*> exprBlock
