module Foglang.Parser.FogFile (fogFile) where

import Foglang.AST (FogFile (..))
import Foglang.Parser (Parser)
import Foglang.Parser.Expr (expr)
import Foglang.Parser.Header (header)
import Text.Megaparsec (many)

-- TODO needs refactoring when we work out proper package/module etc. design
fogFile :: Parser FogFile
fogFile = FogFile <$> header <*> many expr
